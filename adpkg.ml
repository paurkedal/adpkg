(* Copyright (C) 2016  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Printf
open Topkg

let (-<-) f g x = f (g x)

module Option = struct
  let fold f = function
    | None -> fun acc -> acc
    | Some x -> f x
end

module List = struct
  include List

  let rec fold f = function
    | [] -> fun acc -> acc
    | x :: xs -> fun acc -> fold f xs (f x acc)
end

module String_map = Map.Make (String)

exception Parse_error of string * int * string

let parse_error fp fl msg = raise (Parse_error (fp, fl, msg))

module Ad_fpath = struct
  let strip_dir dir fp =
    let n = String.length dir in
    if n < String.length fp && String.is_prefix dir fp && fp.[n] = '/'
    then String.with_index_range ~first:(n + 1) fp
    else fp
end

module Ad_os_file = struct
  let with_open_in f fp =
    let ic = open_in fp in
    try
      let y = f ic in
      close_in ic; Ok y
    with
    | Sys_error msg -> close_in ic; Error (`Msg msg)

  let with_open_out f fp =
    let oc = open_out fp in
    try
      let y = f oc in
      close_out oc; y
    with
    | Sys_error msg -> close_out oc; Error (`Msg msg)

  let fold_lines f fp acc =
    fp |> with_open_in @@ fun ic ->
      let rec loop acc =
        try loop (f (input_line ic) acc) with End_of_file -> acc in
      loop acc
end

type tag = string

module Tags = Set.Make (String)
type tags = Tags.t

module Filter = struct
  type t = tags -> bool

  let not f tags = not (f tags)
  let (&&) f1 f2 tags = f1 tags && f2 tags
  let (||) f1 f2 tags = f1 tags || f2 tags
  let tagged tag tags = Tags.mem tag tags
end

module Modules = struct

  type t = tags String_map.t

  let empty = String_map.empty

  let add ?(tags = Tags.empty) ?dir m modules =
    String_map.add (Option.fold Fpath.append dir m) tags modules

  let add_file ?(tags = Tags.empty) ?dir fp modules =
    let dir =
      match dir with
      | Some dir -> Some dir
      | None ->
        match Fpath.dirname fp with
        | "." -> None
        | dir -> Some dir in
    let add_line line (fl, acc) =
      let line =
        match String.cut ~sep:'#' line with
        | None -> String.trim line
        | Some (s, _) -> String.trim s in
      if line = "" then (succ fl, acc) else
      let m, tags' =
        match String.cut ~sep:'{' line with
        | None -> line, tags
        | Some (m, s) ->
          let m, s = String.trim m, String.trim s in
          let n = String.length s in
          if n = 0 then
            parse_error fp fl "Mispatched open brace" else
          if s.[n - 1] <> '}' then
            parse_error fp fl "Gibberish after module name." else
          let s = String.with_index_range ~last:(n - 2) s in
          let tags' =
            List.fold (Tags.add -<- String.trim)
                      (String.cuts ~sep:',' s) tags in
          (m, tags') in
      (succ fl, add ~tags:tags' ?dir m acc) in
    Ad_os_file.fold_lines add_line fp (1, modules) >>| snd

  let union ms1 ms2 =
    let aux m tags1 tags2 =
      match tags1, tags2 with
      | None, None -> Some Tags.empty
      | Some tags, None | None, Some tags -> Some tags
      | Some tags1, Some tags2 -> Some (Tags.union tags1 tags2) in
    String_map.merge aux ms1 ms2

  let extract ?(filter = fun _ -> true) ?strip_dir modules =
    let aux m tags acc =
      if filter tags
      then Option.fold Ad_fpath.strip_dir strip_dir m :: acc
      else acc in
    List.rev (String_map.fold aux modules [])

  let write ?(filter = fun _ -> true) ?strip_dir oc modules =
    let aux m tags =
      if filter tags then begin
        output_string oc (Option.fold Ad_fpath.strip_dir strip_dir m);
        output_char oc '\n'
      end in
    try
      Ok (String_map.iter aux modules)
    with
    | Sys_error msg -> Error (`Msg msg)

  let save ?filter ?strip_dir fp modules =
    OS.Dir.must_exist (Fpath.dirname fp) >>= fun _ ->
    Ad_os_file.with_open_out (fun oc -> write ?filter ?strip_dir oc modules) fp

  let mllib ?(filter = fun _ -> true) ?strip_dir modules
            ?field ?cond ?dst_dir fp =
    let strip_dir =
      match strip_dir with
      | Some dir -> dir
      | None -> Fpath.dirname fp in
    save ~filter ~strip_dir fp modules >>| fun () ->
    let api_filter = Filter.(filter && not (tagged "internal")) in
    let api = extract ~filter:api_filter ~strip_dir modules in
    Pkg.mllib ~api ?dst_dir fp
end
