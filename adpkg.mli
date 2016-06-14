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

(** The main and only toplevel module *)

open Topkg

type tag = string

type tags

module Tags : sig
  type elt = tag
  type t = tags
  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val inter : t -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val cardinal : t -> int
  val elements : t -> elt list
  val of_list : elt list -> t
end

module Filter : sig
  type t

  val any : t
  val not : t -> t
  val (&&) : t -> t -> t
  val (||) : t -> t -> t
  val tagged : tag -> t
end

module Modules : sig

  type t

  val empty : t

  val add : ?tags: Tags.t -> ?dir: fpath -> string -> t -> t

  val add_file : ?tags: tags -> ?dir: fpath -> fpath -> t -> t result

  val of_file : ?tags: tags -> ?dir: fpath -> fpath -> t result

  val union : t -> t -> t

  val extract : ?filter: Filter.t -> ?strip_dir: fpath -> t -> string list

  val write :
    ?filter: Filter.t ->
    ?map_dir: (Tags.t -> fpath -> fpath) ->
    ?strip_dir: fpath ->
    t -> out_channel -> unit result

  val save :
    ?filter: Filter.t ->
    ?map_dir: (Tags.t -> fpath -> fpath) ->
    ?strip_dir: fpath ->
    t -> fpath -> unit result

  val mllib :
    ?filter: Filter.t -> ?strip_dir: fpath -> t ->
    ?field: Pkg.field -> ?cond: bool -> ?dst_dir: fpath -> fpath ->
    Pkg.install result

end
