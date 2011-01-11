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

type modifier = Gdk.Tags.modifier

type handler = {
    cond : (unit -> bool) ;
    cback : (unit -> unit) ;
  }

type keyhit_spec = int * int * Gdk.keysym
      (** mods * mask * key *)

let int_of_modifier = function
    `SHIFT -> 1
  | `LOCK -> 2
  | `CONTROL -> 4
  | `MOD1 -> 8
  | `MOD2 -> 16
  | `MOD3 -> 32
  | `MOD4 -> 64
  | `MOD5 -> 128
  | `BUTTON1 -> 256
  | `BUTTON2 -> 512
  | `BUTTON3 -> 1024
  | `BUTTON4 -> 2048
  | `BUTTON5 -> 4096

let print_modifier l =
  List.iter
    (fun m ->
      print_string
	(((function
	    `SHIFT -> "SHIFT"
	  | `LOCK -> "LOCK"
	  | `CONTROL -> "CONTROL"
	  | `MOD1 -> "MOD1"
	  | `MOD2 -> "MOD2"
	  | `MOD3 -> "MOD3"
	  | `MOD4 -> "MOD4"
	  | `MOD5 -> "MOD5"
	  | `BUTTON1 -> "B1"
	  | `BUTTON2 -> "B2"
	  | `BUTTON3 -> "B3"
	  | `BUTTON4 -> "B4"
	  | `BUTTON5 -> "B5")
	    m)^" ")
    )
    l;
  print_newline ()

let int_of_modifiers l =
  List.fold_left (fun acc -> fun m -> acc + (int_of_modifier m)) 0 l

module H =
  struct
    type t = keyhit_spec * handler
    let equal (m,k) (mods, mask, key) =
      (k = key) && ((m land mask) = mods)

    let filter_with_mask mods mask key l =
      List.filter (fun a -> (fst a) <> (mods, mask, key)) l

    let find_handlers mods key l =
      List.map snd
        (List.filter
	   (fun ((m,ma,k),_) -> equal (mods,key) (m,ma,k))
	   l
	)

  end

let (table : (int, H.t list ref) Hashtbl.t) = Hashtbl.create 13

let key_press w ev =
  let key = GdkEvent.Key.keyval ev in
  let modifiers = GdkEvent.Key.state ev in
  try
    let (r : H.t list ref) = Hashtbl.find table (Oo.id w) in
    let l = H.find_handlers (int_of_modifiers modifiers) key !r in
    match l with
      [] -> false
    | _ ->
	List.iter
	  (fun h ->
	    if h.cond () then
	      try h.cback ()
	      with e -> prerr_endline (Printexc.to_string e)
	    else ()
	  )
	  l;
	true
  with
    Not_found ->
      false

let associate_key_press w =
  ignore ((w#event#connect#key_press ~callback: (key_press w)) : GtkSignal.id)

let default_modifiers = ref ([] : modifier list)
let default_mask = ref ([`MOD2 ; `MOD3 ; `MOD4 ; `MOD5 ; `LOCK] : modifier list)

let set_default_modifiers l = default_modifiers := l
let set_default_mask l = default_mask := l

let remove_widget  (w : < event : GObj.event_ops ; ..>) () =
  try
    let r = Hashtbl.find table (Oo.id w) in
    r := []
  with
    Not_found ->
      ()

let add1 ?(remove=false) w
    ?(cond=(fun () -> true))
    ?(mods= !default_modifiers)
    ?(mask= !default_mask)
    k callback =

  let r =
    try Hashtbl.find table (Oo.id w)
    with Not_found ->
      let r = ref [] in
      Hashtbl.add table (Oo.id w) r;
      ignore (w#connect#destroy ~callback: (remove_widget w));
      associate_key_press w;
      r
  in
  let n_mods = int_of_modifiers mods in
  let n_mask = lnot (int_of_modifiers mask) in
  let new_h = { cond = cond ; cback = callback } in
  if remove then
    (
     let l = H.filter_with_mask n_mods n_mask k !r in
     r := ((n_mods, n_mask, k), new_h) :: l
    )
  else
    r := ((n_mods, n_mask, k), new_h) :: !r

let add w
    ?(cond=(fun () -> true))
    ?(mods= !default_modifiers)
    ?(mask= !default_mask)
    k callback =
  add1 w ~cond ~mods ~mask k callback

let add_list w
    ?(cond=(fun () -> true))
    ?(mods= !default_modifiers)
    ?(mask= !default_mask)
    k_list callback =
  List.iter (fun k -> add w ~cond ~mods ~mask k callback) k_list

let set w
    ?(cond=(fun () -> true))
    ?(mods= !default_modifiers)
    ?(mask= !default_mask)
    k callback =
  add1 ~remove: true w ~cond ~mods ~mask k callback

let set_list w
    ?(cond=(fun () -> true))
    ?(mods= !default_modifiers)
    ?(mask= !default_mask)
    k_list callback =
  List.iter (fun k -> set w ~cond ~mods ~mask k callback) k_list

(** {2 Trees of handlers, a la emacs} *)

let ignored_keys = ref
    [
      GdkKeysyms._Num_Lock ;
      GdkKeysyms._Scroll_Lock ;
      GdkKeysyms._Pause ;
      GdkKeysyms._Shift_L ;
      GdkKeysyms._Shift_R ;
      GdkKeysyms._Control_L ;
      GdkKeysyms._Control_R ;
      GdkKeysyms._Caps_Lock ;
      GdkKeysyms._Shift_Lock ;
      GdkKeysyms._Meta_L ;
      GdkKeysyms._Meta_R ;
      GdkKeysyms._Alt_L ;
      GdkKeysyms._Alt_R ;
      GdkKeysyms._Super_L ;
      GdkKeysyms._Super_R ;
      GdkKeysyms._Hyper_L ;
      GdkKeysyms._Hyper_R ;
      GdkKeysyms._ISO_Lock ;
      GdkKeysyms._ISO_Level2_Latch ;
      GdkKeysyms._ISO_Level3_Shift ;
      GdkKeysyms._ISO_Level3_Latch ;
      GdkKeysyms._ISO_Level3_Lock ;
      GdkKeysyms._ISO_Group_Shift ;
      GdkKeysyms._ISO_Group_Latch ;
      GdkKeysyms._ISO_Group_Lock ;
      GdkKeysyms._ISO_Next_Group ;
      GdkKeysyms._ISO_Next_Group_Lock ;
      GdkKeysyms._ISO_Prev_Group ;
      GdkKeysyms._ISO_Prev_Group_Lock ;
      GdkKeysyms._ISO_First_Group ;
      GdkKeysyms._ISO_First_Group_Lock ;
      GdkKeysyms._ISO_Last_Group ;
      GdkKeysyms._ISO_Last_Group_Lock ;
    ]

type handler_tree_node =
    Handler of handler
  | Node of handler_tree list
and handler_tree =
    { mutable hst_spec : keyhit_spec ;
      mutable hst_v : handler_tree_node ;
    }

type keyhit_state = (modifier list * Gdk.keysym) list

(** associations between a widget object id and its associated key press callback and its "key hit state" *)
let (states_table : (int, GtkSignal.id * (modifier list * Gdk.keysym) list) Hashtbl.t) = Hashtbl.create 37

let reset_state w =
  let oid = Oo.id w in
  try
    let (evid,state) = Hashtbl.find states_table oid in
    Hashtbl.replace states_table oid (evid, [])
  with Not_found ->
    ()

let rec trees_of_state trees state =
  match state with
    [] -> trees
  | (mods,k) :: q ->
      let mods = int_of_modifiers mods in
      let hst = List.find (fun t -> H.equal (mods,k) t.hst_spec) trees in
      match hst.hst_v with
        Handler _ -> raise Not_found
      | Node l -> trees_of_state l q

let on_key_press ?f_display_state stop_mods stop_key f_trees w ev =
  try
    let oid = Oo.id w in
    let (evid,state) = Hashtbl.find states_table oid in
    let key = GdkEvent.Key.keyval ev in
    if List.mem key !ignored_keys then raise Not_found;
(*    prerr_endline (Printf.sprintf "key=%X" key);*)
    let modifiers = GdkEvent.Key.state ev in
    let modifiers_n = int_of_modifiers modifiers in
    let disp_state after_handler st =
      match f_display_state with
        None -> ()
      | Some f -> f ~after_handler st
    in
    let set_state ?(after_handler=false) st =
      Hashtbl.replace states_table oid (evid, st);
      disp_state after_handler st
    in
    if key = stop_key && int_of_modifiers stop_mods = modifiers_n then
      (
       set_state []; true
      )
    else
    let trees =
      try trees_of_state (f_trees ()) state
      with Not_found ->
        set_state [];
        raise Not_found
    in
    try
      let hst = List.find (fun t -> H.equal (modifiers_n,key) t.hst_spec) trees in
      match hst.hst_v with
        Node _ ->
          let new_state = state @ [modifiers,key] in
          set_state new_state;
          true
      | Handler h ->
          if h.cond () then
	    try h.cback ()
	    with e -> prerr_endline (Printexc.to_string e)
	  else ();
          set_state ~after_handler: true [];
          true
    with
      Not_found ->
        set_state [];
        false
  with Not_found ->
    false

let set_handler_trees ?(stop=([`CONTROL],GdkKeysyms._g))f_trees ?f_display_state w =
  let (stop_mods, stop_key) = stop in
  let add () =
    let id = w#event#connect#key_press
        ~callback: (on_key_press ?f_display_state stop_mods stop_key f_trees w)
    in
    Hashtbl.add states_table (Oo.id w) (id, [])
  in
  try
    let (id, _) = Hashtbl.find states_table (Oo.id w) in
    w#misc#disconnect id;
    add ()
  with
    Not_found -> add ()

let handler ?(cond=(fun () -> true)) callback =
  { cond = cond ; cback = callback }

let keyhit_spec
    ?(mods= !default_modifiers)
    ?(mask= !default_mask)
    key =
  let n_mods = int_of_modifiers mods in
  let n_mask = lnot (int_of_modifiers mask) in
  (n_mods, n_mask, key)

let handler_tree ?mods ?mask key v =
  { hst_spec = keyhit_spec ?mods ?mask key ;
    hst_v = v ;
  }

let rec trees_of_list l =
  match l with
    [] -> []
  | ([],_) :: q ->
      trees_of_list q
  | ([(mods,key)],f) :: q ->
      (handler_tree
        ~mods key (Handler (handler f))) ::
      (trees_of_list q)
  | (((mods,key)::b),_) :: q ->
      let mods = List.sort Pervasives.compare mods in
      let pred = function
          ([],_) -> false
        | (((mods2,key2)::_),_) ->
            key2 = key &&
            List.sort Pervasives.compare mods2 = mods
      in
      let (same,diff) = List.partition pred l in
      let subs = List.map
          (function
              ((_::q),f) -> (q,f)
            | _ -> assert false
          )
          same
      in
      (handler_tree ~mods key (Node (trees_of_list subs))) ::
      (trees_of_list q)
