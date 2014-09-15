{shared{
  open Eliom_content.Html5
  open Eliom_content.Html5.F
}}

let generic_email_form ?label ~service () =
  D.post_form ~service
    (fun name ->
      let l = [
        string_input
          ~a:[a_placeholder "e-mail address"]
          ~input_type:`Email
          ~name
          ();
        string_input
          ~a:[a_class ["button"]]
          ~input_type:`Submit
          ~value:"Send"
          ();
      ]
      in
      match label with
        | None -> l
        | Some lab -> F.label [pcdata lab]::l) ()

let connect_form () =
  D.post_form ~service:Eba_services.connect_service
    (fun (login, password) -> [
      string_input
        ~a:[a_placeholder "Your email"]
        ~name:login
        ~input_type:`Email
        ();
      string_input
        ~a:[a_placeholder "Your password"]
        ~name:password
        ~input_type:`Password
        ();
      string_input
        ~a:[a_class ["button"]]
        ~input_type:`Submit
        ~value:"Sign in"
        ();
    ]) ()

{shared{
let disconnect_button () =
  post_form ~service:%(Eba_services.disconnect_service)
    (fun _ -> [
         button ~button_type:`Submit
           [Ow_icons.F.signout (); pcdata "Logout"]
       ]) ()
 }}

let sign_up_form () =
  generic_email_form ~service:Eba_services.sign_up_service' ()

let forgot_password_form () =
  generic_email_form
    ~service:Eba_services.forgot_password_service ()

let information_form
    ?(firstname="") ?(lastname="") ?(password1="") ?(password2="")
    () =
  D.post_form ~service:Eba_services.set_personal_data_service'
    (fun ((fname, lname), (passwordn1, passwordn2)) -> [
         string_input
           ~a:[a_placeholder "Your firstname"]
           ~name:fname
           ~value:firstname
           ~input_type:`Text
           ();
         string_input
           ~a:[a_placeholder "Your lastname"]
           ~name:lname
           ~value:lastname
           ~input_type:`Text
           ();
         string_input
           ~a:[a_placeholder "Your password"]
           ~name:passwordn1
           ~value:password1
           ~input_type:`Password
           ();
         string_input
           ~a:[a_placeholder "Re-enter password"]
           ~name:passwordn2
           ~value:password2
           ~input_type:`Password
           ();
         string_input
           ~a:[a_class ["button"]]
           ~input_type:`Submit
           ~value:"Submit"
           ();
       ]) ()

let preregister_form label =
  generic_email_form ~service:Eba_services.preregister_service' ~label ()

let home_button () =
  form ~service:Eba_services.main_service
    (fun _ -> [
      string_input
        ~input_type:`Submit
        ~value:"home"
        ();
    ])

let avatar user =
  match Eba_user.avatar_uri_of_user user with
  | Some src ->
    img ~alt:"picture" ~a:[a_class ["eba_avatar"]] ~src ()
  | None -> Ow_icons.F.user ()

let username user =
  lwt n = match Eba_user.firstname_of_user user with
    | "" ->
      lwt email = Eba_user.email_of_user user in
      Lwt.return [pcdata email]
    | s ->
      Lwt.return [pcdata s;
                  pcdata " ";
                  pcdata (Eba_user.lastname_of_user user);
                 ]
  in
  Lwt.return (div ~a:[a_class ["eba_username"]] n)

{shared{
let password_form () =
  D.post_form
    ~service:%(Eba_services.set_password_service')
    (fun (pwdn, pwd2n) ->
       let pass1 =
         D.string_input
           ~a:[a_required `Required;
               a_autocomplete `Off]
           ~input_type:`Password ~name:pwdn ()
       in
       let pass2 =
         D.string_input
           ~a:[a_required `Required;
               a_autocomplete `Off]
           ~input_type:`Password ~name:pwd2n ()
       in
       ignore {unit{
         let pass1 = To_dom.of_input %pass1 in
         let pass2 = To_dom.of_input %pass2 in
         Lwt_js_events.async
           (fun () ->
              Lwt_js_events.inputs pass2
                (fun _ _ ->
                   ignore
                     (if Js.to_string pass1##value <> Js.to_string pass2##value
                      then
                        (Js.Unsafe.coerce
                           pass2)##setCustomValidity("Passwords do not match")
                      else (Js.Unsafe.coerce pass2)##setCustomValidity(""));
                   Lwt.return ()))
       }};
       [
         table
           [
             tr [td [label [pcdata "Password:"]]; td [pass1]];
             tr [td [label [pcdata "Retype password:"]]; td [pass2]];
           ];
         string_input ~input_type:`Submit ~value:"Send" ()
       ])
    ()
 }}