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

(** Convenient class to use multicolumn list.

   @cgname Gmylist
   @version 1.0
   @author Maxence Guesdon
*)

type 'a content =
  String of ('a -> string)
  | Pixbuf of ('a -> GdkPixbuf.pixbuf option)
  | Check of (('a -> bool) * ('a -> bool -> bool))
;;

type 'a col_desc = string option * 'a content

class virtual ['a] plist :
  Gtk.Tags.selection_mode ->
  (string option * 'a content) list ->
  bool ->
  object
    val mutable current_sort : int
    val mutable view : GTree.view
    method box : GObj.widget
    method compare : 'a -> 'a -> int
    method private connect_events : unit
    method insert : ?row: Gtk.tree_iter -> 'a -> unit
    method menu : GToolbox.menu_entry list
    method on_deselect : 'a -> unit
    method on_double_click : 'a -> unit
    method on_enter : unit -> unit
    method on_select : 'a -> unit
    method selection : 'a list
    method set_titles : string list -> unit
    method private sort : 'a list -> 'a list
    method update_data : 'a list -> unit
    method view : GTree.view

    method up_selected : unit
    method down_selected : unit
    method copy_selected : ('a list -> unit) -> unit
    method cut_selected : ('a list -> unit) -> unit
    method delete_selected : unit
    method paste_where_selected : 'a list -> unit
    method edit_first_selected : ('a -> 'a) -> unit
    method content : 'a list

    method init_cols_display :
	cols: GTree.view_column list ->
	  datacol: 'a GTree.column ->
	    renderer: GTree.cell_renderer_text -> GTree.list_store -> unit
  end
