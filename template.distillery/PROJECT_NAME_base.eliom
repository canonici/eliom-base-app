let%server application_name = !%%%MODULE_NAME%%%_config.app_name

let%client application_name = Eliom_client.get_application_name ()

let () =
  let int_of_pgport s =
    try
      int_of_string s
    with Failure _ ->
      failwith @@ Printf.sprintf
        "PGPORT environment variable must be an integer, not '%s'" s
  in
  Eba_db.init
    ?host:!%%%MODULE_NAME%%%_config.eba_db_host
    ?port:!%%%MODULE_NAME%%%_config.eba_db_port
    ?user:!%%%MODULE_NAME%%%_config.eba_db_user
    ?password:!%%%MODULE_NAME%%%_config.eba_db_password
    ?database:!%%%MODULE_NAME%%%_config.eba_db_database
    ?unix_domain_socket_dir:!%%%MODULE_NAME%%%_config.eba_db_unix_domain_socket_dir
    ()

let () = Eba_email.set_mailer "/usr/sbin/sendmail"

module App = Eliom_registration.App(struct
    let application_name = application_name
    let global_data_path = Some ["__global_data__"]
  end)
