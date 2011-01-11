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

(** *)

(** {2 Files} *)

let home =
  try Sys.getenv "HOME"
  with Not_found -> ""

let sourceviews_basename = "sourceviews";;

let rc_dir =
  let d = Filename.concat home ".mlgtksourceview2" in
  let exist =
    try (Unix.stat d).Unix.st_kind = Unix.S_DIR
    with _ -> false
  in
  if not exist then
    begin
      try
        Unix.mkdir d 0o700;
        let old_d = Filename.concat home ".mlgtksourceview" in
        let old_sv_file = Filename.concat old_d sourceviews_basename in
        let new_sv_file =  Filename.concat d sourceviews_basename in
        if Sys.file_exists old_sv_file then
          begin
            let com = Printf.sprintf "cp %s %s"
              (Filename.quote old_sv_file)
              (Filename.quote new_sv_file)
            in
            ignore (Sys.command com)
          end;
      with _ -> ()
    end;
  d
;;

let file_sourceviews = Filename.concat rc_dir sourceviews_basename

(** {2 Languages} *)

let source_language_manager =
  GSourceView2.source_language_manager ~default: false;;
source_language_manager#set_search_path
  (rc_dir :: source_language_manager#search_path);;

let available_source_languages ?(manager=source_language_manager) () =
  List.fold_right
    (fun id acc ->
       match manager#language id with
         None -> acc
       | Some l -> l :: acc)
    manager#language_ids []
;;

let source_language_by_name ?(manager=source_language_manager) name =
  let langs = available_source_languages ~manager () in
  try Some (List.find (fun s -> s#name = name) langs)
  with _ -> None
;;
let sort_languages_by_name =
  List.sort
    (fun l1 l2 -> Pervasives.compare (String.lowercase l1#name) (String.lowercase l2#name))

(** {2 Styles} *)

let source_style_scheme_manager =
  GSourceView2.source_style_scheme_manager ~default: false
let _ = source_style_scheme_manager#prepend_search_path rc_dir;;

let available_source_style_schemes ?(manager=source_style_scheme_manager) () =
  List.fold_right
    (fun id acc ->
       match manager#style_scheme id with
         None -> acc
       | Some l -> l :: acc)
    manager#style_scheme_ids []
;;

let source_style_scheme_by_name ?(manager=source_style_scheme_manager) name =
  let schemes = available_source_style_schemes ~manager () in
  try Some (List.find (fun s -> s#name = name) schemes)
  with _ -> None
;;

let scheme_rc_file = Filename.concat rc_dir "source_style_scheme";;

let scheme_options_group () =
  let scheme_group = new Config_file.group in
  let scheme_name = new Config_file.option_cp
    Config_file.string_wrappers
      ~group: scheme_group
      ["scheme_name"] None ""
  in
  (scheme_group, scheme_name)
;;

let store_style_scheme_selection sss =
  let (group, name) = scheme_options_group () in
  name#set (match sss with None -> None | Some sss -> Some sss#name);
  group#write scheme_rc_file;;

let read_style_scheme_selection ?(manager=source_style_scheme_manager) () =
  let (group, name) = scheme_options_group () in
  group#read scheme_rc_file;
  match name#get with
    None -> None
  | Some name -> source_style_scheme_by_name ~manager name
;;
let source_style_scheme = ref None;;
let set_source_style_scheme = (:=) source_style_scheme;;
let source_style_scheme () = !source_style_scheme;;

let registered_source_buffers = ref ([] : GSourceView2.source_buffer list)
let unregister_source_buffer sb =
  registered_source_buffers :=
    List.filter (fun sb2 -> sb2#get_oid <> sb#get_oid)
    !registered_source_buffers
;;

let register_source_buffer (sb : GSourceView2.source_buffer) =
  unregister_source_buffer sb;
  registered_source_buffers := sb :: !registered_source_buffers;
  sb#set_style_scheme (source_style_scheme ())
;;

let apply_source_style_scheme_to_registered_buffers sss =
  List.iter
    (fun sb -> sb#set_style_scheme sss)
    !registered_source_buffers
;;

class source_style_scheme_box
  ?(manager=source_style_scheme_manager)
  ?(current=source_style_scheme())
  ?(preview=apply_source_style_scheme_to_registered_buffers) () =
  let vbox = GPack.vbox () in
  let hbox = GPack.hbox ~packing: vbox#pack () in
  let _wl_scheme = GMisc.label ~text: "Style scheme: "
    ~packing: (hbox#pack ~expand: false ~fill: true)
    ()
  in
  let wcombo_scheme = GEdit.combo
    ~packing: (hbox#pack ~expand: true ~fill: true)
    ~allow_empty: true
    ~enable_arrow_keys: true
    ~value_in_list: true
    ()
  in
  object(self)
    method box = vbox#coerce

    val mutable scheme = (current : GSourceView2.source_style_scheme option)
    method scheme = scheme
    method set_scheme sopt =
      scheme <- sopt;

    initializer
      manager#force_rescan ();
      let on_scheme_change () =
        let name = wcombo_scheme#entry#text in
        let s_opt = source_style_scheme_by_name ~manager name in
        self#set_scheme s_opt;
        preview s_opt
      in
      wcombo_scheme#set_popdown_strings
        (List.map (fun s -> s#name) (available_source_style_schemes ~manager()));
      let s = match self#scheme with None -> "" | Some s -> s#name in
      wcombo_scheme#entry#set_text s;
      ignore(wcombo_scheme#entry#connect#changed on_scheme_change);

  end

let edit_source_style_scheme ?modal ?manager
  ?(current=source_style_scheme())
  ?(preview=(fun s ->
        set_source_style_scheme s;
        apply_source_style_scheme_to_registered_buffers s;
        store_style_scheme_selection s)
    )
    () =
  let d = GWindow.dialog ?modal ~type_hint: `DIALOG ~width: 400 ~height: 600 () in
  let box = new source_style_scheme_box ?manager ~current ~preview ()  in
  let f_ok () =
    let s = box#scheme in
    store_style_scheme_selection s; preview s;
    d#destroy ()
  in
  let f_cancel () = preview current; d#destroy ()
  in
  d#vbox#pack ~expand: true ~fill: true box#box;
  d#add_button_stock `OK `OK;
  d#add_button_stock `CANCEL `CANCEL;
  match d#run () with
    `OK -> f_ok ()
  | `CANCEL
  | `DELETE_EVENT -> f_cancel ()
;;

(** {2 Sourceview props} *)

type source_view_props =
  {
    mutable sv_font : string option ;
    mutable sv_auto_indent : bool ;
    mutable sv_tab_width : int option ;
    mutable sv_tab_spaces : bool ;
  }

let xml_of_string_prop name v =
  Xml.Element ("prop",["name",name;"value",v],[])
let string_of_opt = function
  None -> ""
| Some s -> s
let xml_of_string_opt_prop name v =
  xml_of_string_prop name (string_of_opt v)
let xml_of_bool_prop name v =
  xml_of_string_prop name (if v then "true" else "false")
let xml_of_int_prop name v =
  xml_of_string_prop name (string_of_int v)
let xml_of_int_opt_prop name v =
  xml_of_string_opt_prop name
    (match v with None -> None | Some n -> Some (string_of_int n))


let xml_of_svprops st =
  [
    xml_of_string_opt_prop "font" st.sv_font ;
    xml_of_bool_prop "auto-indent" st.sv_auto_indent ;
    xml_of_int_opt_prop "tab-width" st.sv_tab_width ;
    xml_of_bool_prop "tab-spaces" st.sv_tab_spaces ;
  ]

let xml_store_sourceview_props ~file svprops =
  let l = xml_of_svprops svprops in
  let xml = Xml.Element ("sourceview", [], l) in
  let oc = open_out file in
  output_string oc "<?xml version=\"1.0\"?>\n";
  output_string oc (Xml.to_string_fmt xml);
  close_out oc


let empty_sourceview_props () =
  {
    sv_font = None ;
    sv_auto_indent = false ;
    sv_tab_width = None ;
    sv_tab_spaces = false ;
  }

let find_prop_of_xml name l =
  try
    let pred = function
      Xml.Element ("prop",atts,_) ->
        List.exists
          (function ("name",s) -> s = name | _ -> false)
          atts
    |	_ -> false
    in
    match List.find pred l with
      Xml.Element ("prop",atts,_) ->
        Some (List.assoc "value" atts)
    | _ -> assert false
  with
    Not_found -> None

let map_opt f = function
  None -> None
| Some v -> Some (f v)

let string_opt_prop_of_xml name l =
  match find_prop_of_xml name l with
    None | Some "" -> None
  | Some s -> Some s
let string_prop_of_xml name l =
  match find_prop_of_xml name l with
    None -> ""
  | Some s -> s
let int_opt_prop_of_xml name l =
  try map_opt int_of_string (find_prop_of_xml name l)
  with Invalid_argument _ -> None
let bool_prop_of_xml name l =
  match find_prop_of_xml name l with
  | Some "true" -> true
  | _ -> false

let source_view_props_of_xml = function
  Xml.Element ("sourceview", _, l) ->
    Some
      {
        sv_font = string_opt_prop_of_xml "font" l ;
        sv_auto_indent = bool_prop_of_xml "auto-indent" l ;
        sv_tab_width = int_opt_prop_of_xml "tab-width" l ;
        sv_tab_spaces = bool_prop_of_xml "tab-spaces" l ;
      }
| _ ->
    None

let xml_read_sourceview_props ~file =
  let error s = failwith (Printf.sprintf "File %s: %s" file s) in
  try
    let xml = Xml.parse_file file in
    source_view_props_of_xml xml
  with
    Xml.Error e ->
      error (Xml.error e)

let svprops_of_source_view sv =
  { sv_font = None ;
    sv_auto_indent = sv#auto_indent ;
    sv_tab_width = Some sv#tab_width ;
    sv_tab_spaces = sv#insert_spaces_instead_of_tabs ;
  }

let apply_sourceview_props sv st =
  (
   match st.sv_font with
     None -> ()
   | Some s -> sv#misc#modify_font_by_name s
  );
  sv#set_auto_indent st.sv_auto_indent;
  (
   match st.sv_tab_width with
     None -> ()
   | Some n -> sv#set_tab_width n
  );
  sv#set_insert_spaces_instead_of_tabs st.sv_tab_spaces
;;

let store_sourceview_props st =
  xml_store_sourceview_props ~file: file_sourceviews st

let registered_source_views = ref []
let remove_source_view sv =
  registered_source_views :=
    List.filter (fun sv2 -> sv2#get_oid <> sv#get_oid)
    !registered_source_views

let register_source_view (sv : GSourceView2.source_view) =
  remove_source_view sv;
  registered_source_views := sv :: !registered_source_views;
  ignore(sv#misc#connect#destroy (fun () -> remove_source_view sv))

let apply_sourceview_props_to_registered st =
  List.iter
    (fun sv -> apply_sourceview_props sv st)
    !registered_source_views

let read_sourceview_props () =
  let file = file_sourceviews in
  try
    match xml_read_sourceview_props ~file with
      None -> empty_sourceview_props ()
    | Some st -> st
  with
    Xml.File_not_found _ ->
      empty_sourceview_props ()



class sourceview_props_box f_preview =
  let vbox = GPack.vbox () in

  let wftab = GBin.frame ~label: "Tab stops"
    ~packing: (vbox#pack ~fill: true ~padding: 3) () in
  let vbtab = GPack.vbox ~packing: wftab#add () in
  let hbtab = GPack.hbox ~packing: (vbtab#pack ~expand: false ~fill: true) () in
  let _ = GMisc.label ~text: "Tab width: " ~packing: (hbtab#pack ~expand: false) () in
  let spin_tab_width = GEdit.spin_button
    ~rate: 1.0 ~digits: 0 ~numeric: true
      ~snap_to_ticks: true ~value: 2.0 ~wrap: false
      ~packing: (hbtab#pack ~expand: false) () in
  let _ = spin_tab_width#adjustment#set_bounds ~lower: 1.0 ~upper: 40.0
    ~step_incr: 1.0 () in
  let wc_tab_spaces = GButton.check_button
    ~label: "Insert spaces instead of tab"
      ~packing: (vbtab#pack ~expand: false ~fill: true) () in

  let wfautoindent = GBin.frame ~label: "Automatic indentation"
    ~packing: (vbox#pack ~fill: true ~padding: 3) () in
  let wc_auto_indent = GButton.check_button
    ~label: "Enable automatic indentation"
      ~packing: wfautoindent#add () in

  let wffont = GBin.frame ~label: "Font"
    ~packing: (vbox#pack ~fill: true ~padding: 3) () in
  let vbfont = GPack.vbox ~packing: wffont#add () in
  let wc_default_font = GButton.check_button
    ~label: "Use default theme font"
      ~packing: (vbfont#pack ~expand: false ~fill: true) () in
  let hbfont = GPack.hbox ~packing: (vbfont#pack ~expand: false ~fill: true) () in
  let _ = GMisc.label ~text: "Use this font: "
    ~packing: (hbfont#pack ~expand: false ~fill: true) () in
  let wb_font = GButton.font_button
    ~packing: (hbfont#pack ~expand: true ~fill: true) () in

  object(self)
    method box = vbox#coerce

    val mutable props = (None : source_view_props option)
    method props = props
    method set_props o =
      props <- o;
      self#update_params_widgets

    method private update_params_widgets =
      match props with
        None -> vbox#misc#set_sensitive false
      |	Some st ->
          vbox#misc#set_sensitive true;
          let n = match st.sv_tab_width with
              None -> 2
            | Some n -> n
          in
          spin_tab_width#set_value (float n);
          wc_tab_spaces#set_active st.sv_tab_spaces;
          wc_auto_indent#set_active st.sv_auto_indent;

          wc_default_font#set_active (st.sv_font = None);
          (
           match st.sv_font with
             None -> ()
           | Some s -> wb_font#set_font_name s
          );

    initializer
      let handle_change (f : source_view_props -> unit) =
        fun () ->
          match props with
            None -> ()
          | Some st -> f st; f_preview st
      in
      let on_font_toggled st =
        let fn = not wc_default_font#active in
        wb_font#misc#set_sensitive fn;
        if fn then
          st.sv_font <- Some wb_font#font_name
        else
          st.sv_font <- None
      in
      let on_font_set st =
        if st.sv_font <> None then
          st.sv_font <- Some wb_font#font_name
      in
      let on_bool_toggled (wc : GButton.toggle_button) f st = f st wc#active in
      let on_auto_indent_toggled =
        on_bool_toggled wc_auto_indent (fun st b -> st.sv_auto_indent <- b)
      in
      let on_tab_spaces_toggled =
        on_bool_toggled wc_tab_spaces (fun st b -> st.sv_tab_spaces <- b)
      in
      let on_tab_width_changed st =
        st.sv_tab_width <- Some spin_tab_width#value_as_int
      in
      ignore(wb_font#connect#font_set (handle_change on_font_set));
      List.iter
        (fun ((wc : GButton.toggle_button),f) -> ignore (wc#connect#toggled (handle_change f)))
        [
          wc_default_font, on_font_toggled ;
          wc_auto_indent, on_auto_indent_toggled ;
          wc_tab_spaces, on_tab_spaces_toggled ;
        ];
      ignore(spin_tab_width#connect#value_changed (handle_change on_tab_width_changed));
  end

let edit_sourceview_props ?modal ?(preview=apply_sourceview_props_to_registered) () =
  let d = GWindow.dialog ?modal ~type_hint: `DIALOG ~width: 400 ~height: 600 () in
  let box = new sourceview_props_box preview in
  let f_ok () =
    (
     match box#props with
       None -> ()
     | Some p -> store_sourceview_props p; preview p
    );
    d#destroy ()
  in
  let f_cancel () =
    let p = read_sourceview_props () in
    preview p;
    d#destroy ()
  in
  box#set_props (Some (read_sourceview_props ()));
  d#vbox#pack ~expand: true ~fill: true box#box;
  d#add_button_stock `OK `OK;
  d#add_button_stock `CANCEL `CANCEL;
  match d#run () with
    `OK -> f_ok ()
  | `CANCEL
  | `DELETE_EVENT -> f_cancel ()
