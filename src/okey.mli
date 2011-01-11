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

(** Okey interface.
   Convenient functions to handle key press events in Lablgtk2 widgets.

   Once the lib is compiled and installed, you can use it by referencing
   it with the [Okey] module. You must add [okey.cmo] or [okey.cmx]
   on the commande line when you link.

   @cgname Okey
   @version 1.0
   @author Maxence Guesdon
*)

type modifier = Gdk.Tags.modifier

(** Set the default modifier list. The first default value is [[]].*)
val set_default_modifiers : modifier list -> unit

(** Set the default modifier mask. The first default value is
   [[`MOD2 ; `MOD3 ; `MOD4 ; `MOD5 ; `LOCK]].
   The mask defines the modifiers not taken into account
   when looking for the handler of a key press event.
*)
val set_default_mask : modifier list -> unit

(** {2 Setting handlers for simple key press events.} *)

(** [add widget key callback] associates the [callback] function to the event
   "key_press" with the given [key] for the given [widget].

   @param remove when true, the previous handlers for the given key and modifier
   list are not kept.
   @param cond this function is a guard: the [callback] function is not called
   if the [cond] function returns [false].
   The default [cond] function always returns [true].

   @param mods the list of modifiers. If not given, the default modifiers
   are used.
   You can set the default modifiers with function {!Okey.set_default_modifiers}.

   @param mask the list of modifiers which must not be taken
   into account to trigger the given handler. [mods]
   and [mask] must not have common modifiers. If not given, the default mask
   is used.
   You can set the default modifiers mask with function {!Okey.set_default_mask}.
*)
val add :
    < connect : < destroy : callback: (unit -> unit) -> GtkSignal.id; .. >;
      event : GObj.event_ops; get_oid : int; .. > ->
	?cond: (unit -> bool) ->
	  ?mods: modifier list ->
	    ?mask: modifier list ->
	      Gdk.keysym ->
		(unit -> unit) ->
		  unit

(** It calls {!Okey.add} for each given key.*)
val add_list :
    < connect : < destroy : callback: (unit -> unit) -> GtkSignal.id; .. >;
      event : GObj.event_ops; get_oid : int; .. > ->
	?cond: (unit -> bool) ->
	  ?mods: modifier list ->
	    ?mask: modifier list ->
	      Gdk.keysym list ->
		(unit -> unit) ->
		  unit

(** Like {!Okey.add} but the previous handlers for the
   given modifiers and key are not kept.*)
val set :
    < connect : < destroy : callback: (unit -> unit) -> GtkSignal.id; .. >;
      event : GObj.event_ops; get_oid : int; .. > ->
	?cond: (unit -> bool) ->
	  ?mods: modifier list ->
	    ?mask: modifier list ->
	      Gdk.keysym ->
		(unit -> unit) ->
		  unit

(** It calls {!Okey.set} for each given key.*)
val set_list :
    < connect : < destroy : callback: (unit -> unit) -> GtkSignal.id; .. >;
      event : GObj.event_ops; get_oid : int; .. > ->
	?cond: (unit -> bool) ->
	  ?mods: modifier list ->
	    ?mask: modifier list ->
	      Gdk.keysym list ->
		(unit -> unit) ->
		  unit

(** Remove the handlers associated to the given widget.
   This is automatically done when a widget is destroyed but
   you can do it yourself. *)
val remove_widget :
    < connect : < destroy : callback: (unit -> unit) -> GtkSignal.id; .. >;
      event : GObj.event_ops; get_oid : int; .. > ->
	unit ->
	  unit


(** {2 Setting handlers for combination of key press events, a la emacs} *)

type handler

type keyhit_spec

type keyhit_state = (modifier list * Gdk.keysym) list

(** The keys which are ignored when they are pressed alone. *)
val ignored_keys : Gdk.keysym list ref

type handler_tree_node =
    Handler of handler
  | Node of handler_tree list
and handler_tree = {
  mutable hst_spec : keyhit_spec;
  mutable hst_v : handler_tree_node;
}

val set_handler_trees :
    ?stop:(modifier list * Gdk.keysym) ->
      (unit -> handler_tree list) ->
        ?f_display_state: (after_handler: bool -> keyhit_state -> unit) ->
          < misc : GObj.misc_ops ; event : GObj.event_ops; ..> -> unit

val handler : ?cond:(unit -> bool) -> (unit -> unit) -> handler

val keyhit_spec :
  ?mods:modifier list -> ?mask:modifier list -> Gdk.keysym -> keyhit_spec

val handler_tree :
  ?mods:modifier list ->
  ?mask:modifier list ->
    Gdk.keysym -> handler_tree_node -> handler_tree

val reset_state : < misc : GObj.misc_ops ; ..> -> unit

val trees_of_list : (keyhit_state * (unit -> unit)) list -> handler_tree list
