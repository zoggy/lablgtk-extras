(*********************************************************************************)
(*                Cameleon                                                       *)
(*                                                                               *)
(*    Copyright (C) 2004-2010 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Library General Public License as            *)
(*    published by the Free Software Foundation; either version 2 of the         *)
(*    License, or any later version.                                             *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Library General Public          *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(* $Id: example2.ml 749 2010-06-17 06:52:00Z zoggy $ *)

(* Compile with
  ocamlfind ocamlc -package lablgtk2-extras.configwin -linkpkg \
  -o configwin_example2.x <this_file>
*)

let _ = GMain.Main.init ()

open Configwin

let param1 = string ~help: "a string" "a string" "a value"
let param2 = bool ~help: "bool value" "a boolean" true
let param3 = filename ~help: "a file name" "a file name" "foo"
let param4 = strings
    ~help: "a list of strings"
    ~eq: (fun _ -> fun _ -> false)
    ~add: (fun () -> ["another string" ; "and another string"])
    "a string list"
    ["foo" ; "bar"]
let param5 = color ~help: "a color" "a color" "Red"
let param6 = font ~help: "a font" "a font" "7x13bold"
let param7 = date ~help: "a date" "a date" (1, 0, 2002)
let n = ref 0
let param8 = list
    ~help: "a list of int"
    ~add: (fun () -> incr n; [!n])
    ~titles: ["n" ; "n * n"]
    "an int list" (fun n -> [string_of_int n ; string_of_int (n*n)])
    [1 ; 2 ; 3]
let param9 = filenames ~help: "a list of filenames" "filenames" []

let structure = Section_list
  ("Section 1",
   [
    Section ("Section 1.1",
             [ param1 ; param2 ; param5 ; param6 ; param7 ; param9]);
    Section ("Section 1.2",
             [ param3 ; param4 ; param8])
   ]
  )

let _ = Configwin.edit "Titre" [structure]
