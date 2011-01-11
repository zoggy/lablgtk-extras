(*********************************************************************************)
(*                Lablgtk-extras                                                 *)
(*                                                                               *)
(*    Copyright (C) 2011 Institut National de Recherche en Informatique          *)
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
(*                                                                               *)
(*********************************************************************************)

(* $Id: okey_example.ml 749 2010-06-17 06:52:00Z zoggy $ *)

(* Compile with
  ocamlc -o okey_example.x -I +lablgtk2 -I +lablgtk-extras lablgtk.cma lablgtkextras.cma <this_file> *)

let _ = GMain.Main.init ()
let w = GWindow.window
    ~title: "Okey example" ~width: 300 ~height: 300 ()

let message s () =
  GToolbox.message_box "Okey example message" s

let callbacks =
  [
    GdkKeysyms._k, [`MOD1], "Alt-k", "Hello", message "Hello" ;
    GdkKeysyms._w, [], "w", "World", message "World" ;
    GdkKeysyms._q, [`CONTROL], "Ctrl-q", "Quit", GMain.Main.quit ;
  ]

let txt = "Available shortcuts:\n"^
  (String.concat "\n"
     (List.map (fun (_,_,s,l,_) -> Printf.sprintf "%s: %s" s l) callbacks)
  )

let wl = GMisc.label ~text: txt ~packing: w#add ()

(* Here we add the handlers for some key press events *)
let _ = List.iter
    (fun (k,mods,_,_,f) -> Okey.add w ~mods k f)
    callbacks

let _ = w#show ()
let _ = GMain.Main.main ()
