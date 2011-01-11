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

(** A class to display directories. *)

let file_exists f =
  try let _ = Unix.stat f in true
  with _ -> false


let is_prefix f1 f2 =
  let len1 = String.length f1 in
  let len2 = String.length f2 in
  (len1 < len2) &&
  (String.sub f2 0 len1) = f1

(*c==v=[File.subdirs]=0.1====*)
let subdirs path =
  let d = Unix.opendir path in
  let rec iter acc =
    let file =
      try Some (Unix.readdir d)
      with End_of_file -> Unix.closedir d; None
    in
    match file with
    | None -> List.rev acc
    | Some s when
	s = Filename.parent_dir_name or
	s = Filename.current_dir_name -> iter acc
    | Some file ->
        try
	  let complete_f = Filename.concat path file in
          match (Unix.stat complete_f).Unix.st_kind with
            Unix.S_DIR ->
	      iter (complete_f :: acc)
	  | _ -> iter acc
	with
	  Unix.Unix_error _ -> iter acc
  in
  iter []
(*/c==v=[File.subdirs]=0.1====*)


class type gdir_behaviour =
  object

  end

class default_behaviour : gdir_behaviour =
  object

  end

class gdir () =
  let vbox = GPack.vbox () in
  let wscroll = GBin.scrolled_window
      ~vpolicy: `AUTOMATIC
      ~hpolicy: `AUTOMATIC
      ~packing: (vbox#pack ~expand: true)
      ()
  in
  let cols = new GTree.column_list in
  let col_display = cols#add Gobject.Data.string in
  let col_complete = cols#add Gobject.Data.string in

  let store = GTree.tree_store cols in
  let view = GTree.view
      ~headers_visible: false
      ~model: store ~packing: wscroll#add_with_viewport () in
  let renderer = GTree.cell_renderer_text [] in
  let col = GTree.view_column ()
      ~renderer:(renderer, ["text", col_display]) in
  let () = ignore (view#append_column col) in

  object(self)
    val mutable selection = (None : string option)

    method on_select_dir _ = ()
    method on_unselect_dir _ = ()
    method expand_dir _ = true
    method dir_string = Filename.basename
    method roots = [Filename.current_dir_name]
    method menu_ctx _ = []
    method on_expand_dir _ = ()
    method on_collapse_dir _ = ()
    method subdirs s = subdirs s
    method row_inserted _ _ = ()
    method init_col_display ~col_display ~complete ~renderer _ = ()


    method selection = selection

    method box = vbox

    method select_dir dir =
      selection <- Some dir ;
      self#on_select_dir dir

    method unselect_dir dir =
      selection <- None ;
      self#on_unselect_dir dir

    method col_display = col_display
    method col_complete = col_complete
    method view = view

    method insert_node ?parent dirname name =
      let row = store#append ?parent () in
      store#set row col_complete name;
      store#set row col_display (self#dir_string name);
      self#row_inserted row name;

      let subdirs = self#subdirs name in
      match subdirs with
        [] ->
          ()
      | l ->
	  let rr = store#get_row_reference (store#get_path row) in
          List.iter
	    (self#insert_node ~parent: row name)
            (List.sort compare l);
          if self#expand_dir name then
	    view#expand_row rr#path

    method update =
      (
       match selection with
	 None -> ()
       | Some dir ->
	   selection <- None ;
	   self#unselect_dir dir
      );
      store#clear ();
      List.iter (self#insert_node "") self#roots

    method update_selected_dir =
      let sel = view#selection in
      match sel#get_selected_rows with
	[] -> ()
      |	row :: _ ->
	  let it = store#get_iter row in
	  while store#iter_has_child it do
	    match Gstuff.find_first_child store it with
	      None -> ()
	    | Some iter -> ignore (store#remove iter)
	  done;
	  let dir = store#get ~row: it ~column: col_complete in
	  let subdirs = self#subdirs dir in
	  (
	   match subdirs with
	     [] ->
	       ()
	   | l ->
               List.iter
		 (self#insert_node ~parent: it dir)
		 (List.sort compare l)
	  );
	  self#select_dir dir


    initializer

      view#selection#set_mode `SINGLE;

      ignore
	(view#connect#row_expanded
	   (fun it _ ->
	     let dir = store#get ~row: it ~column: col_complete in
	     self#on_expand_dir dir
	   )
	);
      ignore
	(view#connect#row_collapsed
	   (fun it _ ->
	     let dir = store#get ~row: it ~column: col_complete in
	     self#on_collapse_dir dir
	   )
	);
      ignore
	(view#selection#connect#changed
	   (fun () ->
	     (
	      match selection with
		None -> ()
	      | Some d -> self#unselect_dir d
	     );
	     let sel = view#selection in
	     match sel#get_selected_rows with
	       [] -> ()
	     | row :: _ ->
		 let it = store#get_iter row in
		 let dir = store#get ~row: it ~column: col_complete in
		 self#select_dir dir
	   )
	);

      (* connect the press on button 3 for contextual menu *)
      let _ = view#event#connect#button_press ~callback:
	(
	 fun ev ->
	   GdkEvent.Button.button ev = 3 &&
	   GdkEvent.get_type ev = `BUTTON_PRESS &&
	   (
	    match self#menu_ctx self#selection with
	      [] -> true
	    | l ->
		GToolbox.popup_menu
		  ~button: 3
		  ~time: (Int32.of_int 0)
		  ~entries: l;
		true
	   )
	)
      in
      self#init_col_display
	~col_display: col ~complete: col_complete ~renderer store;
      self#update
  end
