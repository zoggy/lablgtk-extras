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

(* $Id: gstuff.ml 749 2010-06-17 06:52:00Z zoggy $ *)

let find_first_child store it =
  let p = store#get_path it in
  let res = ref None in
  store#foreach
    (fun _ it ->
      let parent = store#iter_parent it in
      match parent with
        None -> false
      | Some itp ->
          if store#get_path itp = p then (res := Some it; true) else false
    );
  !res

let find_iter_above store it =
  let p = store#get_path it in
  let res = ref None in
  store#foreach
    (fun path it ->
      let rr = store#get_row_reference path in
      store#iter_next it;
      if store#get_path it = p then (res := Some rr; true) else false
    );
  !res

let find_iter_below store it =
  if store#iter_next it then
    Some (store#get_row_reference (store#get_path it))
  else
    None
