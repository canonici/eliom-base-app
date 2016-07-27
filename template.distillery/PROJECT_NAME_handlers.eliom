
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

 let forgot_password_handler =
   forgot_password_handler Eba_services.main_service

]

[%%client

  let set_personal_data_handler' =
    let set_personal_data_rpc =
      ~%(Eliom_client.server_function
	   [%derive.json : ((string * string) * (string * string))]
	   (Eba_session.connected_rpc
	      (fun id s -> set_personal_data_handler' id () s)))
    in
    fun (_ : int64) () d -> set_personal_data_rpc d

  let set_password_handler' id () p =
    Eba_handlers.set_password_rpc p

  let forgot_password_handler =
    let forgot_password_rpc =
      ~%(Eliom_client.server_function
	   [%derive.json : string]
	   (Eba_session.Opt.connected_rpc
	      (fun _ mail ->
		forgot_password_handler () mail)))
    in
    fun () mail -> forgot_password_rpc mail

  let preregister_handler' =
    let preregister_rpc =
      ~%(Eliom_client.server_function
	   [%derive.json : string]
	   (Eba_session.Opt.connected_rpc
	      (fun _ mail -> preregister_handler' () mail)))
    in
    fun () mail -> preregister_rpc mail
     
  let activation_handler =
    let activation_handler_rpc =
      ~%(Eliom_client.server_function
	   [%derive.json : string]
	   (Eba_session.Opt.connected_rpc
	      (fun _ akey -> activation_handler akey ())))
    in
    fun akey () -> activation_handler_rpc akey
  
  let disconnect_handler =
    let disconnect_rpc =
      ~%(Eliom_client.server_function
	   [%derive.json: unit]
	   (fun () -> disconnect_handler () ()))
    in
    fun () () -> disconnect_rpc ()
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

 let settings_handler =
   let settings_content =
     let none = [%client ((fun () -> ()) : unit -> unit)] in
     fun user ->
       Eliom_content.Html.D.(
	 [
	   div ~a:[a_class ["eba-welcome-box"]] [
	     p [pcdata "Change your password:"];
	     Eba_view.password_form ~service:Eba_services.set_password_service' ();
	     br ();
	     Eba_userbox.upload_pic_link
	       none
	       %%%MODULE_NAME%%%_services.upload_user_avatar_service
	       (Eba_user.userid_of_user user);
	     br ();
	     Eba_userbox.reset_tips_link none;
	   ]
	 ]
       )
   in
   fun userid_o () () ->
     let%lwt user = %%%MODULE_NAME%%%_container.get_user_data userid_o in
     let content = match user with
       | Some user ->
	 settings_content user
       | None -> []
     in
     %%%MODULE_NAME%%%_container.page userid_o content

]
