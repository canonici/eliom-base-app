
[%%server

 include Eba_handlers

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
