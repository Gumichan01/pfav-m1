open GMain
open GdkKeysyms

let locale = GtkMain.Main.init ()

let main () =
  let window = GWindow.window ~width:320 ~height:240
                              ~title:"Simple lablgtk program" () in
  let vbox = GPack.vbox ~packing:window#add () in
  let _ = window#connect#destroy ~callback:Main.quit in

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  let _ = factory#add_item "Quit" ~key:_Q ~callback: Main.quit in

  (* Text input *)
  let scroll = GBin.scrolled_window
                 ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
                 ~packing:vbox#pack () in
  let textview = GText.view ~packing:scroll#add_with_viewport () in
  textview#buffer#set_text "multi-\nline\ntext";

  (* Math_view *)
  let mview = GMathViewAux.single_selection_math_view
                ~font_size:14 ~packing:vbox#add() in
  let _ = mview#load_uri ~filename:"ex.mathml" in

  (* Display the windows and enter Gtk+ main loop *)
  let () = window#add_accel_group accel_group in
  let () = window#show () in
  Main.main ()

let () = main ()
