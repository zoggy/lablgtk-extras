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

(** Editable trees.
   @cgname Gmytree
   @version 1.0
   @author Maxence Guesdon
*)

type col_desc = [ `Pixmap of string option | `String of string ]
type col_row_contents =
    [ `Pixmap of GdkPixbuf.pixbuf option | `String of string ]
class ['a] tree_edit :
  ?m_edit:string ->
  ?m_add:string ->
  ?m_remove:string ->
  ?m_close:string ->
  ?f_expand:('a -> bool) ->
  f_roots:(unit -> 'a list) ->
  f_children:('a -> 'a list) ->
  ?f_edit:('a -> 'a) ->
  ?f_add:('a option -> 'a option) ->
  ?f_remove:('a -> bool) ->
  ?f_close:(unit -> unit) ->
  f_contents:('a -> col_row_contents list) ->
  [< `Pixmap of 'b | `String of 'c ] list ->
  object
    val mutable selection : 'a option
    method selected_row : Gtk.tree_iter option
    method add : unit -> unit
    method add_button :
      string -> ('a option -> (unit -> unit) -> unit) -> unit
    method box : GPack.box
    method buttons_box : GPack.box
    method edit : unit -> unit
    method father_data : Gtk.tree_iter -> 'a option
    method insert : ?append: bool -> ?parent:Gtk.tree_iter -> 'a -> unit
    method menu : GToolbox.menu_entry list
    method on_double_click : 'a -> unit
    method on_select : 'a -> unit
    method on_unselect : 'a -> unit
    method remove : unit -> unit
    method remove_row : Gtk.tree_iter -> bool
    method select : 'a -> unit
    method set_row : Gtk.tree_iter -> 'a -> unit
    method unselect : 'a -> unit
    method update : unit
    method view : GTree.view
  end
