
let%client generic_email_form = Eba_view.generic_email_form

let%server generic_email_form ~service () = Eliom_content.Html.D.(
  Form.post_form ~service ~xhr:false
      (fun name ->
	[
          Form.input
            ~a:[a_placeholder "e-mail address"]
            ~input_type:`Email
            ~name
            Form.string;
	  hr ();
          Form.input
            ~a:[a_class ["button"]]
            ~input_type:`Submit
            ~value:"Send"
            Form.string;
	]
      ) ()
)

let%shared sign_up_form () =
  generic_email_form ~service:Eba_services.sign_up_service' ()

let%shared forgot_password_form () =
  generic_email_form ~service:Eba_services.forgot_password_service ()

let%shared forgotpwd_button () = Eliom_content.Html.D.(
  let popup_content = fun () -> Lwt.return @@
    div ~a:[a_class ["navbar-inverse";"eba-login-menu"]]
    [forgot_password_form ()] in
  let button_name = "forgot your password?" in
  Eba_tools.popup_button
    ~button_name
    ~button_class:["button"]
    ~popup_content
)

let%shared sign_in_button () = Eliom_content.Html.D.(
  let popup_content = fun () -> Lwt.return @@
    div ~a:[a_class ["navbar-inverse";"eba-login-menu"]]
    [Eba_view.connect_form ()] in
  let button_name = "Sign In" in
  Eba_tools.popup_button
    ~button_name
    ~button_class:["button"]
    ~popup_content
)

let%shared sign_up_button () = Eliom_content.Html.D.(
  let popup_content = fun () -> Lwt.return @@
    div ~a:[a_class ["navbar-inverse";"eba-login-menu"]]
    [sign_up_form ()] in
  let button_name = "Sign Up" in
  Eba_tools.popup_button
    ~button_name
    ~button_class:["button"]
    ~popup_content
)