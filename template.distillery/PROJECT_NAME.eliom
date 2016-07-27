(* This file was generated by Eliom-base-app.
   Feel free to use it, modify it, and redistribute it as you wish. *)

[%%shared
    open Eliom_content.Html.D
]

[%%shared
let () =
  (* Registering services. Feel free to customize handlers. *)
  Eliom_registration.Action.register
    ~service:Eba_services.set_personal_data_service'
    (Eba_session.connected_fun %%%MODULE_NAME%%%_handlers.set_personal_data_handler');

  Eliom_registration.Action.register
    ~service:Eba_services.set_password_service'
    (Eba_session.connected_fun %%%MODULE_NAME%%%_handlers.set_password_handler');

  Eliom_registration.Action.register
    ~service:Eba_services.forgot_password_service
    (%%%MODULE_NAME%%%_handlers.forgot_password_handler Eba_services.main_service);

  Eliom_registration.Action.register
    ~service:Eba_services.preregister_service'
    %%%MODULE_NAME%%%_handlers.preregister_handler';

  Eliom_registration.Action.register
    ~service:Eba_services.sign_up_service'
    Eba_handlers.sign_up_handler;

  Eliom_registration.Action.register
    ~service:Eba_services.connect_service
    Eba_handlers.connect_handler;

  Eliom_registration.Action.register
    ~service:Eba_services.disconnect_service
    %%%MODULE_NAME%%%_handlers.disconnect_handler;

  Eliom_registration.Any.register
    ~service:Eba_services.activation_service
    %%%MODULE_NAME%%%_handlers.activation_handler;

  %%%MODULE_NAME%%%_base.App.register
    ~service:Eba_services.main_service
    (%%%MODULE_NAME%%%_page.Opt.connected_page %%%MODULE_NAME%%%_handlers.main_service_handler);

  %%%MODULE_NAME%%%_base.App.register
    ~service:%%%MODULE_NAME%%%_services.about_service
    (%%%MODULE_NAME%%%_page.Opt.connected_page %%%MODULE_NAME%%%_handlers.about_handler);

  %%%MODULE_NAME%%%_base.App.register
    ~service:%%%MODULE_NAME%%%_services.settings_service
    (%%%MODULE_NAME%%%_page.Opt.connected_page %%%MODULE_NAME%%%_handlers.settings_handler);

]

let () =
  Eliom_registration.Ocaml.register
    ~service:%%%MODULE_NAME%%%_services.upload_user_avatar_service
    (Eba_session.connected_fun %%%MODULE_NAME%%%_handlers.upload_user_avatar_handler)


(* Print more debugging information when <debugmode/> is in config file
   (DEBUG = yes in Makefile.options).
   Example of use:
   let section = Lwt_log.Section.make "%%%MODULE_NAME%%%:sectionname"
   ...
   Lwt_log.ign_info ~section "This is an information";
   (or ign_debug, ign_warning, ign_error etc.)
 *)
let _ =
  if Eliom_config.get_debugmode ()
  then begin
    ignore
      [%client (
        (* Eliom_config.debug_timings := true; *)
        (* Lwt_log_core.add_rule "eliom:client*" Lwt_log.Debug; *)
        (* Lwt_log_core.add_rule "eba*" Lwt_log.Debug; *)
        Lwt_log_core.add_rule "%%%MODULE_NAME%%%*" Lwt_log.Debug
        (* Lwt_log_core.add_rule "*" Lwt_log.Debug *)
        : unit ) ];
    (* Lwt_log_core.add_rule "*" Lwt_log.Debug *)
    Lwt_log_core.add_rule "%%%MODULE_NAME%%%*" Lwt_log.Debug
  end
