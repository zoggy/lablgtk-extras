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

(** Using the same sourceview preferences through all applications
   which use the LablGtkSourceView2 library.

   Use the [source_language_manager] and [source_style_scheme_manager]
   below to get the languages to use in your code views.

   @cgname Gtksv_utils
   @version 1.0
   @author Maxence Guesdon
*)

(** {2 Languages} *)

val source_language_manager : GSourceView2.source_language_manager

(** Sort the given list of languages by name. *)
val sort_languages_by_name :
  GSourceView2.source_language list -> GSourceView2.source_language list

(** [available_source_languages ()] returns the list of languages known by the
    language_manager.
    @param manager can be given to use an alternative language_manager *)
val available_source_languages :
  ?manager: GSourceView2.source_language_manager ->
    unit -> GSourceView2.source_language list

(** [source_language_by_name ?manager name] returns the language with
       the given name, or [None] if no such language was found. *)
val source_language_by_name :
  ?manager: GSourceView2.source_language_manager ->
    string -> GSourceView2.source_language option

(** {2 Styles} *)

(** This source style scheme manager has an extended search path to
       look for styles in the user's ~/.mlgtksourceview2 directory first. *)
val source_style_scheme_manager : GSourceView2.source_style_scheme_manager

val available_source_style_schemes :
  ?manager: GSourceView2.source_style_scheme_manager -> unit ->
    GSourceView2.source_style_scheme list

(** [source_style_scheme_by_name ?manager name] returns the style scheme with
       the given name, or [None] if no such style scheme was found. *)
val source_style_scheme_by_name :
  ?manager: GSourceView2.source_style_scheme_manager ->
    string -> GSourceView2.source_style_scheme option

(** Store the name of the given style scheme in the user's personal files. *)
val store_style_scheme_selection : GSourceView2.source_style_scheme option -> unit

(** Read the name of the selected style scheme from the user's personal files.
     @param manager can specify the style scheme manager to use instead of
     {!source_style_scheme_manager}.*)
val read_style_scheme_selection :
  ?manager: GSourceView2.source_style_scheme_manager -> unit ->
    GSourceView2.source_style_scheme option

(** Return the currently selected style scheme, if any. *)
val source_style_scheme : unit -> GSourceView2.source_style_scheme option

(** Set the currently selected style scheme, if any. *)
val set_source_style_scheme : GSourceView2.source_style_scheme option -> unit

(** Register a source buffer so that each time the scheme returned
     by {!source_style_scheme} changes, the new scheme is set in
     the buffer. It is also set when registering the buffer. *)
val register_source_buffer : GSourceView2.source_buffer -> unit

(** Unregister the given source buffer. *)
val unregister_source_buffer : GSourceView2.source_buffer -> unit

(** Apply the given style scheme to the registered buffers. *)
val apply_source_style_scheme_to_registered_buffers :
  GSourceView2.source_style_scheme option -> unit

(** {2 Sourceview props} *)

type source_view_props

(** Store the given source view properties in the user's personal files.*)
val store_sourceview_props : source_view_props -> unit

(** Read the source view properties from the user's personal files.
   If there is no properties file, default properties are returned.*)
val read_sourceview_props : unit -> source_view_props

(** Register a source view so that each time the function {!read_sourceview_props}
   is called, the properties read are applied to every registered view.
   The sourceview is automatically unregistered when it is destroyed. *)
val register_source_view : GSourceView2.source_view -> unit

(** Apply the given source view properties to the given source view. *)
val apply_sourceview_props : GSourceView2.source_view -> source_view_props -> unit

(** Apply the given source view properties to the registered views. *)
val apply_sourceview_props_to_registered : source_view_props -> unit

(** {2 Choosing style scheme} *)

(** This box can be used to make the user set the source style scheme.
   Use the {!read_style_scheme_selection} and {!store_style_scheme_selection} functions
   to restore or save the selection. The function in parameter is used
   to apply the scheme when a change occurs. For example, you can
   given the {!apply_source_style_scheme_to_registered_buffers} function to update
   your registered source buffers when the user chooses a scheme.
*)
class source_style_scheme_box :
   ?manager: GSourceView2.source_style_scheme_manager ->
    ?current: GSourceView2.source_style_scheme option ->
    ?preview: (GSourceView2.source_style_scheme option -> unit) -> unit ->
    object
      method box : GObj.widget
      method scheme : GSourceView2.source_style_scheme option
      method set_scheme : GSourceView2.source_style_scheme option -> unit
    end

(** Make the user set his source style scheme.
   Then {!store_style_scheme_selection} is used to save the changes or
   {!read_style_scheme_selection} is used to restore the original values.
   @param preview is the function called to apply the shcheme when
   the user makes a change or when he closes the window with "Ok".
*)
val edit_source_style_scheme :
  ?modal:bool ->
    ?manager: GSourceView2.source_style_scheme_manager ->
    ?current: GSourceView2.source_style_scheme option ->
    ?preview:(GSourceView2.source_style_scheme option -> unit) -> unit -> unit

(** {2 Setting sourceview props} *)

(** This box can be used to make the user set source view properties.
   Use the {!read_sourceview_props} and {!store_sourceview_props} functions
   to restore or save the properties. The function in parameter is used
   to apply the properties when a change occurs. For example, you can
   given the {!apply_sourceview_props_to_registered} function to update
   your registered source views when the user changes a property.
*)
class sourceview_props_box :
  (source_view_props -> unit) ->
  object
    val mutable props : source_view_props option
    method box : GObj.widget
    method props : source_view_props option
    method set_props : source_view_props option -> unit
  end

(** Make the user set his source view properties.
   Then {!store_sourceview_props} is used to save the changes or
   {!read_sourceview_props} is used to restore the original values.
   @param preview is the function called to apply the properties when
   the user makes a change or when he closes the window with "Ok".
*)
val edit_sourceview_props :
  ?modal:bool -> ?preview:(source_view_props -> unit) -> unit -> unit
