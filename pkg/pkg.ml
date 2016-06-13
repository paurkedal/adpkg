#! /usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let licenses = List.map Pkg.std_file ["COPYING.LESSER"; "COPYING"]

let () = Pkg.describe ~licenses "adpkg" @@ fun c ->
  Ok [Pkg.mllib "adpkg.mllib"]
