
let%shared sign_up_form () =
  Eba_view.generic_email_form ~service:Eba_services.sign_up_service' ()

let%shared forgot_password_form () =
  Eba_view.generic_email_form ~service:Eba_services.forgot_password_service ()

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
