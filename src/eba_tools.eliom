
[%%shared
 module Navigation_bar : sig

   type service = (
     unit,
     unit,
     Eliom_service.get,
     Eliom_service.att,
     Eliom_service.non_co,
     Eliom_service.non_ext,
     Eliom_service.reg,
     [ `WithoutSuffix ],
     unit,
     unit,
     Eliom_service.non_ocaml
   ) Eliom_service.t

   type elt = string * service

   val ul_of_elts :
     ?ul_class:string list -> elt list -> [>`Ul] Eliom_content.Html.F.elt Lwt.t

 end = struct

   type service = (
     unit,
     unit,
     Eliom_service.get,
     Eliom_service.att,
     Eliom_service.non_co,
     Eliom_service.non_ext,
     Eliom_service.reg,
     [ `WithoutSuffix ],
     unit,
     unit,
     Eliom_service.non_ocaml
   ) Eliom_service.t

   type elt = string * service

   let li_of_elt elt = Eliom_content.Html.F.(
     let text, service = elt in
     li [a ~service [pcdata text] ()]
   )

   let ul_of_elts ?(ul_class = []) elt_list = Eliom_content.Html.F.(
     Lwt.return
     @@ ul ~a:[a_class ul_class]
     @@ List.map li_of_elt elt_list
   )

 end
]


let%shared popup_button
    ~button_name
    ?(button_class = ["eba_popup_button"])
    ~popup_content
    = Eliom_content.Html.D.(
      let button =
	button ~a:[a_class button_class] [pcdata button_name]
      in
      let%lwt popup_content = popup_content () in
      ignore
	[%client
            (Lwt.async (fun () ->
              Lwt_js_events.clicks
		(Eliom_content.Html.To_dom.of_element ~%button)
		(fun _ _ ->
		  let%lwt _ =
                    Ot_popup.popup
                      ~close_button:[Eliom_content.Html.D.pcdata "close"]
                      (fun _ -> Lwt.return ~%popup_content)
		  in
		  Lwt.return ()))
               : _)
	];
      Lwt.return button
    )