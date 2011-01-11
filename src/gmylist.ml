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

type 'a content =
  | String of ('a -> string)
  | Pixbuf of ('a -> GdkPixbuf.pixbuf option)
  | Check of (('a -> bool) * ('a -> bool -> bool))

type 'a col_desc = string option * 'a content

let rec list_iter3 f l1 l2 l3 =
  match l1, l2, l3 with
    [],[],[] -> ()
  | h1::q1, h2::q2, h3::q3 ->
      f h1 h2 h3;
      list_iter3 f q1 q2 q3
  |  _ ->
      raise (Invalid_argument "list_iter3")

type 'a col_content =
    ColString of  string GTree.column * ('a -> string)
  | ColPixbuf of  GdkPixbuf.pixbuf GTree.column * ('a -> GdkPixbuf.pixbuf option)
  | ColCheck of bool GTree.column * GTree.cell_renderer_toggle * ('a -> bool) * ('a -> bool -> bool)


class virtual ['a] plist
    sel_mode col_descs titles_show =
  let wscroll = GBin.scrolled_window
      ~hpolicy: `AUTOMATIC
      ~vpolicy: `AUTOMATIC
      ()
  in
  let cols = new GTree.column_list in
  let cols_display = List.map
    (fun (_,c) ->
       match c with
         String f -> ColString (cols#add Gobject.Data.string, f)
       | Pixbuf f -> ColPixbuf (cols#add (Gobject.Data.gobject : GdkPixbuf.pixbuf Gobject.data_conv), f)
       | Check (f, f_set) ->
           let tog_renderer = GTree.cell_renderer_toggle
             [`ACTIVATABLE true ; `MODE `ACTIVATABLE]
           in
           ColCheck (cols#add Gobject.Data.boolean, tog_renderer, f, f_set)
    )
      col_descs
  in
  let (col_data: 'a GTree.column) = cols#add Gobject.Data.caml in

  let store = GTree.list_store cols in
  let view = GTree.view ~model: store
      ~headers_visible: titles_show
      ~packing: wscroll#add
      ()
  in
  let renderer = GTree.cell_renderer_text [] in
  let pix_renderer = GTree.cell_renderer_pixbuf [] in
  let view_cols = List.map2
      (fun (title,_) coldisp ->
	let col =
	  match coldisp with
           ColString (colstore, _) ->
             let renderer = (renderer, ["text", colstore]) in
             GTree.view_column ?title ~renderer ()
         | ColPixbuf (c, _) ->
             GTree.view_column () ~renderer: (pix_renderer, ["pixbuf",c])
         | ColCheck (c, renderer, _, _) ->
             GTree.view_column () ~renderer: (renderer, ["active", c])
       in
       ignore (view#append_column col);
       col
    )
      col_descs
      cols_display
  in

  let _ = view#selection#set_mode sel_mode in
  object (self)
    val mutable current_sort = 0
    val mutable view = view
    val mutable selection = []
    method box = wscroll#coerce
    method view = view

    method private rr_of_iter row =
      store#get_row_reference (store#get_path row)

    method private rr_of_path p =
      store#get_row_reference p

    method content =
      match store#get_iter_first with
	None -> []
      |	Some it ->
	  let rec f acc it =
	    let data = store#get ~row: it ~column: col_data in
	    match store#iter_next it with
	      false -> data :: acc
	    | true -> f (data::acc) it
	  in
	  List.rev (f [] it)

    method update_data l =
      store#clear ();
      let f d =
	let row = store#prepend () in
	self#insert ~row d
      in
      List.iter f (List.rev l)

    method private content_rr =
      match store#get_iter_first with
	None -> []
      |	Some it ->
	  let rec f acc it =
	    let rr = self#rr_of_iter it in
	    match store#iter_next it with
	      false -> rr :: acc
	    | true -> f (rr::acc) it
	  in
	  List.rev (f [] it)

    method private up_in_rr_list lups l =
      let rec up prev_rr = function
	| [] -> ()
	| rr :: q ->
	    let p = rr#path in
	    if List.exists (fun rr2 -> rr2#path = p) lups then
	      match prev_rr with
		None -> (* stop, we can't move up the whole selection *)
		  ()
	      | Some rr2 ->
		  ignore (store#swap rr#iter rr2#iter);
		  up prev_rr q
	    else
	      up (Some rr) q
      in
      up None l

    method up_selected =
      match view#selection#get_selected_rows with
	[] -> ()
      |	l ->
	  let lups = List.map self#rr_of_path l in
	  let lrr = self#content_rr in
	  self#up_in_rr_list lups lrr

    method down_selected =
      match view#selection#get_selected_rows with
	[] -> ()
      |	l ->
	  let lups = List.map self#rr_of_path l in
	  let lrr = List.rev self#content_rr in
	  self#up_in_rr_list lups lrr

    method private data_selected =
      List.map
	(fun p -> store#get ~row: (store#get_iter p) ~column: col_data)
	view#selection#get_selected_rows

    method copy_selected f =
      match self#data_selected with
	[] -> ()
      |	l -> f l

    method cut_selected f =
      self#copy_selected f;
      self#delete_selected

    method delete_selected =
      let l = List.map self#rr_of_path view#selection#get_selected_rows in
      List.iter (fun rr -> ignore (store#remove rr#iter)) l

    method paste_where_selected l =
      match view#selection#get_selected_rows with
	[] -> List.iter (self#insert ?row: None) l
      |	p :: _ ->
	  let rec f rr = function
	      [] -> ()
	    | d :: q ->
		let row = store#insert_before rr#iter in
		let rr = self#rr_of_iter row in
		self#insert ~row d;
		f rr q
	  in
	  f (self#rr_of_path p) l

    method edit_first_selected f =
      match view#selection#get_selected_rows with
	[] -> ()
      |	path :: _ ->
	  let rr = self#rr_of_path path in
	  let d =  store#get ~row: rr#iter ~column: col_data in
	  let d = f d in
	  store#set ~row: rr#iter ~column: col_data d;
	  self#set_display rr#iter d

    method compare = (Pervasives.compare : 'a -> 'a -> int)
    method selection = selection
    method on_select (d:'a) = ()
    method on_deselect (d:'a) = ()
    method on_double_click (d:'a) = ()
    method on_enter () = ()

    method private sort l =
      List.sort self#compare l

    method set_titles l =
      if List.length l <> List.length cols_display then
	failwith "Bad number of titles"
      else
	List.iter2
	  (fun title c -> c#set_title title)
	  l
	  view_cols

    method private set_display row d =
      let f coldisp =
        match coldisp with
        | ColString (column, f) ->
            store#set ~row ~column (f d)
        | ColCheck (column, renderer, f, _) ->
            let b = f d in
            store#set ~row ~column b
        | ColPixbuf (column, f) ->
            match f d with
              None -> ()
            | Some p -> store#set ~row ~column p
      in
      List.iter f cols_display

    method insert ?row (d: 'a) =
      let row = match row with
	None -> store#append ()
      |	Some row -> row
      in
      self#set_display row d;
      store#set ~row ~column: col_data d

    method menu = ([] : GToolbox.menu_entry list)

    method private connect_events =
      (* connect the press on button 3 for contextual menu *)
      ignore (view#event#connect#button_press ~callback:
                (
                 fun ev ->
		   match GdkEvent.get_type ev with
		     `BUTTON_PRESS when GdkEvent.Button.button ev = 3 ->
                       (
			GToolbox.popup_menu
			  ~button: 3
			  ~time: (Int32.zero)
			  ~entries: self#menu;
			true
		       )
		   | `TWO_BUTTON_PRESS ->
		       (
			let x = int_of_float (GdkEvent.Button.x ev) in
			let y = int_of_float (GdkEvent.Button.y ev) in
			match view#get_path_at_pos ~x ~y with
			  None -> true
			| Some (path,_,_,_) ->
			   let d =
			     let it = store#get_iter path in
			     store#get ~row: it ~column: col_data
			   in
			   self#on_double_click d;
			   true
		       )
		   | `BUTTON_PRESS
		   | `BUTTON_RELEASE
		   | `THREE_BUTTON_PRESS -> false
                )
             );
      ignore
	(view#selection#connect#changed
	   (fun () ->
	     let sel = view#selection in
	     match sel#get_selected_rows with
	       [] ->
		 selection <- [];
		 List.iter self#on_deselect selection

	     | l ->
		 let l = List.map
		     (fun path ->
		       let it = store#get_iter path in
		       store#get ~row: it ~column: col_data
		     )
		     l
		 in
		 let newly_selected =
		   List.filter (fun e -> not (List.mem e selection)) l
		 in
		 let no_more_selected =
		   List.filter (fun e -> not (List.mem e l)) selection
		 in
		 selection <- l;
		 List.iter self#on_deselect no_more_selected;
		 List.iter self#on_select newly_selected;
	   )
	);

      ignore
        (view#event#connect#key_press
         (fun t ->
            (List.mem (GdkEvent.Key.keyval t)
             [
               GdkKeysyms._ISO_Enter ;
               GdkKeysyms._KP_Enter ;
               GdkKeysyms._Return ;
             ]
            ) && (self#on_enter (); true)
         )
        );

      let f = function
      | ColString _ | ColPixbuf _ -> ()
      | ColCheck(col,renderer,_,f_on_toggle) ->
          ignore(renderer#connect#toggled
           (fun path ->
              let it = store#get_iter path in
              let (data, v) =
                (store#get ~row: it ~column: col_data,
                 store#get ~row: it ~column: col)
              in
              let v = f_on_toggle data (not v) in
              store#set ~row: it ~column: col v;
           )
          )
      in
      List.iter f cols_display;

    method init_cols_display ~cols ~datacol ~renderer _ = ()

    initializer
      self#init_cols_display
        ~cols: view_cols ~datacol: col_data ~renderer store;


      self#connect_events


  end
