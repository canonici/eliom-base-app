
let%shared disconnect_button () = Eliom_content.Html.D.(
  Form.post_form ~service:Eba_services.disconnect_service
    (fun _ -> [
         Form.button_no_value
           ~a:[ a_class ["button"] ]
           ~button_type:`Submit
           [Ot_icons.F.signout (); pcdata "Logout"]
       ]) ()
)

let%shared user_menu close user = Eliom_content.Html.D.(
  [
    p [pcdata "Change your password:"];
    Eba_view.password_form ~service:Eba_services.set_password_service' ();
    hr ();
    Eba_userbox.upload_pic_link
      close
      %%%MODULE_NAME%%%_services.upload_user_avatar_service
      (Eba_user.userid_of_user user);
    hr ();
    Eba_userbox.reset_tips_link close;
    hr ();
    disconnect_button ();
  ]
)

let%shared user_menu user = Eliom_content.Html.D.(
  let but = div ~a:[a_class ["btn";"eba-usermenu-button"]]
    [pcdata "Menu"]
  in
  let menu = div ~a:[a_class ["navbar-inverse";"usermenu-pop"]] [] in
  ignore
    (Ow_button.button_dyn_alert but menu
       [%client (fun _ _ ->
         let close () =
           let o = Ow_button.to_button_dyn_alert ~%but in
           o##unpress
         in
         Lwt.return (user_menu close ~%user): 'a -> 'b)]);
  div ~a:[a_class ["eba-usermenu"]] [but; menu]
)
