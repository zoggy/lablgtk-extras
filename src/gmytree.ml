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

type col_desc = [
    `String of string
  | `Pixmap of string option
]

type col_row_contents = [
    `String of string
  | `Pixmap of GdkPixbuf.pixbuf option
  ]

class ['a] tree_edit
    ?(m_edit="Edit")
    ?(m_add="Add")
    ?(m_remove="Remove")
    ?(m_close="Close")
    ?(f_expand=fun (_:'a) -> false)
    ~(f_roots: unit -> 'a list)
    ~(f_children: 'a -> 'a list)
    ?(f_edit: ('a -> 'a) option)
    ?(f_add: ('a option -> 'a option) option)
    ?(f_remove: ('a -> bool) option)
    ?(f_close: (unit -> unit) option)
    ~(f_contents: 'a -> col_row_contents list)
    cols =
  let box1 = GPack.hbox () in
  let wscroll = GBin.scrolled_window
      ~vpolicy: `AUTOMATIC
      ~hpolicy: `AUTOMATIC
      ~packing: (box1#pack ~expand: true)
      ()
  in
  let box2 = GPack.vbox ~packing: (box1#pack ~expand: false) () in

  let tcols = new GTree.column_list in
  let disp_cols = List.map
      (function
          `String _ ->
            `String (tcols#add Gobject.Data.string)
        | `Pixmap _ ->
            `Pixbuf (tcols#add (Gobject.Data.gobject : GdkPixbuf.pixbuf Gobject.data_conv))
      ) cols
  in
  let (datacol : 'a GTree.column) =  tcols#add Gobject.Data.caml in

  let store = GTree.tree_store tcols in
  let view = GTree.view
      ~headers_visible: false
      ~model: store ~packing: wscroll#add_with_viewport () in
  let renderer = GTree.cell_renderer_text [] in
  let pix_renderer = GTree.cell_renderer_pixbuf [] in
  let _ =
    List.iter
      (fun c ->
        let col =
          match c with
            `String c -> GTree.view_column () ~renderer: (renderer, ["text", c])
          | `Pixbuf c -> GTree.view_column () ~renderer: (pix_renderer, ["pixbuf",c])
        in
        ignore (view#append_column col)
      )
      disp_cols
  in
  object(self)
    val mutable selection = (None : 'a option)
    method selected_row =
      match view#selection#get_selected_rows with
        [] -> None
      | p :: _ -> Some (store#get_iter p)

    method view = view
    method box = box1
    method buttons_box = box2

    method on_select v = ()
    method on_unselect v = ()
    method on_double_click v = ()
    method select v =
      selection <- Some v ;
      self#on_select v

    method unselect v =
      selection <- None ;
      self#on_unselect v

    method insert ?(append=false) ?parent (t : 'a) =
      let row =
        (if append then store#append else store#prepend) ?parent ()
      in
      self#set_row row t;
      match List.rev (f_children t) with
        [] -> ()
      | l ->
          let rr = store#get_row_reference (store#get_path row) in
          List.iter (self#insert ~append ~parent: row) l;
          if f_expand t then
            view#expand_row rr#path

    method update =
      (
       match selection with
         None -> ()
       | Some v ->
           selection <- None ;
           self#unselect v
      );
      store#clear ();
      let roots = f_roots () in
      List.iter (self#insert ?parent: None) (List.rev roots)

    method private set_row_col row col contents =
      match col, contents with
        `String col, `String s -> store#set row col s
      | `Pixbuf col, `Pixmap (Some pix) -> store#set row col pix
      | _ -> ()

    method set_row row t =
      let contents = f_contents t in
      List.iter2 (self#set_row_col row) disp_cols contents;
      store#set row datacol t

    method edit () =
      match f_edit, view#selection#get_selected_rows with
        None, _ | _, [] -> ()
      | Some f, path::_ ->
          let row = store#get_iter path in
          let t  = store#get ~row ~column: datacol in
          let (t2 : 'a) = f t in
          self#set_row row t2

    method add () =
      match f_add with
        None -> ()
      | Some f ->
          match view#selection#get_selected_rows with
            [] ->
              (
               match f None with
                 None -> ()
               | Some t -> self#insert t
              )
          | path::_ ->
              let rr = store#get_row_reference path in
              let parent = store#get ~row: rr#iter ~column: datacol in
              (
               match f (Some parent) with
                 None -> ()
               | Some t -> self#insert ~parent: rr#iter t
              )

    method remove () =
      match f_remove with
        None -> ()
      | Some f ->
          match view#selection#get_selected_rows with
            [] -> ()
          | path::_ ->
              let row = store#get_iter path in
              if f ( store#get ~row ~column: datacol) then
                ignore(store#remove row)
              else
                ()

    method remove_row row = store#remove row

    method add_button : string -> ('a option -> (unit -> unit) -> unit) -> unit =
      fun label f ->
        let w = GButton.button ~label  ~packing: self#buttons_box#pack () in
        let g () =
          match view#selection#get_selected_rows with
            [] -> f None (fun () -> self#update)
          | path :: _ ->
              let row = store#get_iter path in
              f (Some (store#get ~row ~column: datacol)) (fun () -> self#update)
        in
        ignore (w#connect#clicked g)

    method menu = ([] : GToolbox.menu_entry list)

    method father_data row =
      match store#iter_parent row with
	None -> None
      |	Some it ->
	  Some (store#get ~row: it ~column: datacol)

    initializer
      view#selection#set_mode `SINGLE;
      List.iter self#insert (List.rev (f_roots()));
      let l =
        (if f_edit = None then [] else [m_edit, self#edit]) @
        (if f_add = None then [] else [m_add, self#add]) @
        (if f_remove = None then [] else [m_remove, self#remove]) @
        (match f_close with None -> [] | Some f -> [m_close, f])
      in
      List.iter
        (fun (label, cb) ->
          let wb = GButton.button ~label ~packing: self#buttons_box#pack () in
          ignore(wb#connect#clicked cb)
        )
        l;
      ignore
	(view#selection#connect#changed
	   (fun () ->
	     let sel = view#selection in
	     match sel#get_selected_rows with
	       [] ->
                 (match selection with
                   None -> ()
                 | Some v -> self#unselect v
                 )
	     | path::_ ->
		 let it = store#get_iter path in
		 let v = store#get ~row: it ~column: datacol in
                 (
                  match selection with
                    None -> ()
                  | Some v -> self#unselect v
                 );
		 self#select v
	   )
	);
      (* connect the press on button 3 for contextual menu
	 and two_button for double click *)
      ignore
	(view#event#connect#button_press ~callback:
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
			 store#get ~row: it ~column: datacol
		       in
		       self#on_double_click d;
		       true
		  )
	      | `BUTTON_PRESS
	      | `BUTTON_RELEASE
	      | `THREE_BUTTON_PRESS -> false
           )
        );
  end
