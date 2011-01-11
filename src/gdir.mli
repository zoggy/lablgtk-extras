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

(* $Id: gdir.mli 749 2010-06-17 06:52:00Z zoggy $ *)

(** Displaying a directory tree.

   @cgname Gdir
   @version 1.0
   @author Maxence Guesdon
*)

class gdir : unit ->
  object
    method dir_string : string -> string
    method expand_dir : string -> bool
    method init_col_display :
      col_display:GTree.view_column ->
      complete:string GTree.column ->
      renderer:GTree.cell_renderer_text -> GTree.tree_store -> unit
    method menu_ctx : string option -> GToolbox.menu_entry list
    method on_collapse_dir : string -> unit
    method on_expand_dir : string -> unit
    method on_select_dir : string -> unit
    method on_unselect_dir : string -> unit
    method roots : string list
    method row_inserted : Gtk.tree_iter -> string -> unit
    method subdirs : string -> string list

    val mutable selection : string option
    method box : GPack.box
    method col_complete : string GTree.column
    method col_display : string GTree.column
    method insert_node : ?parent:Gtk.tree_iter -> string -> string -> unit
    method select_dir : string -> unit
    method selection : string option
    method unselect_dir : string -> unit
    method update : unit
    method update_selected_dir : unit
    method view : GTree.view
  end
