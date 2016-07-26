
[%%server

 include Eba_handlers

 let disconnect_handler () () =
   disconnect_handler () ();
   Lwt.return @@ ignore
     [%client 
	 (Eliom_client.change_page ~service:Eba_services.main_service () ()
	    : unit Lwt.t)]

 let upload_user_avatar_handler myid () ((), (cropping, photo)) =
   let avatar_dir =
     List.fold_left Filename.concat
       (List.hd !%%%MODULE_NAME%%%_config.avatar_dir)
       (List.tl !%%%MODULE_NAME%%%_config.avatar_dir) in
   let%lwt avatar =
     Eba_uploader.record_image avatar_dir ~ratio:1. ?cropping photo in
   let%lwt user = Eba_user.user_of_userid myid in
   let old_avatar = Eba_user.avatar_of_user user in
   let%lwt () = Eba_user.update_avatar avatar myid in
   match old_avatar with
   | None -> Lwt.return ()
   | Some old_avatar ->
     Lwt_unix.unlink (Filename.concat avatar_dir old_avatar )

]

[%%client

  let set_personal_data_handler' id () d =
    let set_personal_data_rpc =
      ~%(Eliom_client.server_function
	   [%derive.json : ((string * string) * (string * string))]
	   (Eba_session.connected_rpc
	      (fun id s -> set_personal_data_handler' id () s)))
    in
    set_personal_data_rpc d

  let set_password_handler' id () p =
    Eba_handlers.set_password_rpc p

  let forgot_password_handler _ () mail =
    let forgot_password_rpc =
      ~%(Eliom_client.server_function
	   [%derive.json : string]
	   (Eba_session.Opt.connected_rpc
	      (fun _ mail ->
		forgot_password_handler Eba_services.main_service () mail)))
    in
    forgot_password_rpc mail

  let preregister_handler' () mail =
    let preregister_rpc =
      ~%(Eliom_client.server_function
	   [%derive.json : string]
	   (Eba_session.Opt.connected_rpc
	      (fun _ mail -> preregister_handler' () mail)))
    in
    preregister_rpc mail
     
  let activation_handler akey () =
    let activation_handler_rpc =
      ~%(Eliom_client.server_function
	   [%derive.json : string]
	   (Eba_session.Opt.connected_rpc
	      (fun _ akey -> activation_handler akey ())))
    in
    activation_handler_rpc akey
  
  let disconnect_handler () () =
    let disconnect_rpc =
      ~%(Eliom_client.server_function
	   [%derive.json: unit]
	   (fun () -> disconnect_handler () ()))
    in
    disconnect_rpc ()
]

[%%shared

 let main_service_handler userid_o () () = Eliom_content.Html.F.(
  %%%MODULE_NAME%%%_container.page userid_o (
    [
      p [em [pcdata "Eliom base app: Put app content here."]]
    ]
  )
 )

 let about_handler userid_o () () = Eliom_content.Html.F.(
  %%%MODULE_NAME%%%_container.page userid_o [
    div [
      p [pcdata "This template provides a skeleton \
                 for an Ocsigen application."];
      br ();
      p [pcdata "Feel free to modify the generated code and use it \
                 or redistribute it as you want."]
    ]
  ]
 )

]
