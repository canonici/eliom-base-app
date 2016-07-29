[%%shared
    open Eliom_content.Html
    open Eliom_content.Html.D
]

[%%shared
module type DemoPage = sig
  val name : string
  val service :
    (unit, unit,
     Eliom_service.get,
     Eliom_service.att,
     Eliom_service.non_co,
     Eliom_service.non_ext,
     Eliom_service.reg,
     [ `WithoutSuffix ],
     unit, unit,
     Eliom_service.non_ocaml)
      Eliom_service.t
  val page : unit -> ([> `Input | `P | `Div] Eliom_content.Html.D.elt) list Lwt.t
end
]

(* popup button demo **********************************************************)

let%server service = Eliom_service.create
  ~id:(Eliom_service.Path ["otdemo-popup"])
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()

[%%shared
module PopupPage : DemoPage = struct

  let name = "Popup Button"

  let service = ~%service

  let page () =
    let button =
      D.Form.input 
	~a:[a_class ["button"]]
	~input_type:`Submit
	~value:"Click for a popup!"
	(Form.string)
    in
    ignore
      [%client
          (Lwt.async (fun () ->
            Lwt_js_events.clicks
              (To_dom.of_element ~%button)
              (fun _ _ ->
                let%lwt _ =
                  Ot_popup.popup
                    ~close_button:[pcdata "close"]
                    (fun _ -> Lwt.return @@ p [pcdata "Popup message"])
                in
                Lwt.return ()))
             : _)
      ];
    Lwt.return
    [
      p [pcdata "Here is a button showing a simple popup window when clicked:"];
      p [button]
    ]
end
]

(* carousel demo **************************************************************)

let%client (carousel_update, carousel_change) = React.E.create ()

let%server service = Eliom_service.create
  ~id:(Eliom_service.Path ["otdemo-carousel"])
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()

[%%shared
module CarouselPage : DemoPage = struct

  let name = "Carousel"

  let service = ~%service

  let page () =
    let make_page content =
      div ~a:[a_class ["otdemo-carousel-page"]] [pcdata content]
    in
    let carousel_pages = ["1"; "2"; "3"] in
    let ribbon_content = List.map (fun p -> [pcdata p]) carousel_pages in
    let carousel_content = List.map make_page carousel_pages in
    let bullets_content =
      List.map (fun n -> [div [p [pcdata n]]]) carousel_pages
    in
    let carousel,
      Some ribbon,
      Some prev,
      Some next,
      Some bullets
      =
      Eba_tools.Carousel.make
	~update:[%client carousel_update]
	~change:[%client carousel_change]
	~carousel:([a_class ["otdemo-carousel"]], carousel_content)
	~ribbon:([], ribbon_content)
	~previous:([a_class ["button"]], [pcdata "←"])
	~next:([a_class ["button"]], [pcdata "→"])
	~bullets:([], bullets_content)
	()
    in
    Lwt.return
      [
	p [pcdata "The carousel displays a number of blocks side-by-side (or vertically stacked)."];
	p [pcdata "To switch to a different block, either use the buttons above or below the carousel."];
	p [pcdata "In the mobile app you can also swipe the screen."];
	ribbon; carousel; p [prev; next];
	div ~a:[a_class ["otdemo-bullets"]] [bullets]
      ]
end
]

(* rpc button demo **********************************************************)

let%server value =
  Eliom_reference.Volatile.eref_from_fun
    ~scope:Eliom_common.global_scope
    (fun () -> 0)

let%server s, f =
  let value = Eliom_reference.Volatile.get value in
  Eliom_shared.React.S.create value

let%server demo_function () = Lwt.return (
  let v = succ (Eliom_reference.Volatile.get value) in
  Eliom_reference.Volatile.set value v;
  ignore [%client (~%f ~%v : unit)]
)

let%client demo_function =
  let demo_rpc =
    ~%(Eliom_client.server_function
	 [%derive.json : unit]
	 demo_function)
  in
  demo_rpc

let%server value_as_string () : string Eliom_shared.React.S.t =
  Eliom_shared.React.S.map [%shared string_of_int] s

let%server value_reactive () =
  Lwt.return @@ value_as_string ()

let%client value_reactive =
  ~%(Eliom_client.server_function [%derive.json: unit] value_reactive)

let%server service = Eliom_service.create
  ~id:(Eliom_service.Path ["otdemo-rpc"])
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()

[%%shared
module RpcPage : DemoPage = struct

  let name = "RPC Button"

  let service = ~%service

  let page () =
    let button =
      button
	~a:[a_class ["button"]]
	[pcdata "Click to call a RPC"]
    in
    ignore
      [%client
          (Lwt.async (fun () ->
            Lwt_js_events.clicks
              (To_dom.of_element ~%button)
              (fun _ _ -> demo_function ())
	   )
             : _)
      ];
    let%lwt value = value_reactive () in
    Lwt.return
      [
	p [pcdata "Here is a button calling a rpc to increase a server side value."];
	p [Eliom_content.Html.R.pcdata value];
	p [button]
      ]
end
]


(* calendar demo **********************************************************)

let%server service = Eliom_service.create
  ~id:(Eliom_service.Path ["otdemo-calendar"])
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()

let%server s, f = Eliom_shared.React.S.create None

let%client action y m d = ~%f (Some (y, m, d)); Lwt.return ()

let%shared string_of_date = function
  | Some (y, m, d) ->
    Printf.sprintf "You clicked on %d %d %d" y m d
  | None ->
    ""

let%server date_as_string () : string Eliom_shared.React.S.t =
  Eliom_shared.React.S.map [%shared string_of_date] s

let%server date_reactive () = Lwt.return @@ date_as_string ()

let%client date_reactive =
  ~%(Eliom_client.server_function [%derive.json: unit] date_reactive)

[%%shared
module CalendarPage : DemoPage = struct

  let name = "Calendar"

  let service = ~%service

  let page () =
    let calendar = Ot_calendar.make
      ~click_non_highlighted:true
      ~action:[%client action]
      ()
    in
    let%lwt dr = date_reactive () in
    Lwt.return
      [
	p [pcdata "This page shows the calendar."];
	div ~a:[a_class ["eba-calendar"]] [calendar];
	p [Eliom_content.Html.R.pcdata dr]
      ]
end
]

(* drawer / otdemo welcome page ***********************************************)

let%shared demos = [
  (module PopupPage : DemoPage);
  (module CarouselPage);
  (module RpcPage);
  (module CalendarPage)
]

(* adds a drawer menu to the document body *)
let%shared make_drawer_menu () =
  let menu =
    let make_link (module D : DemoPage) =
      li [a ~service:D.service [pcdata @@ D.name] ()]
    in
    let menu = ul (List.map make_link demos) in
    [div ~a:[a_class ["eba-drawer"]] [h3 [pcdata "otdemo: drawer menu"]; menu]]
  in
  let drawer, _,_ = Ot_drawer.drawer menu in
  drawer

let%shared make_page userid_o content =
  %%%MODULE_NAME%%%_container.page userid_o (
    make_drawer_menu () :: content
  )

let%shared handler userid_o () () = make_page userid_o
  [
    p [pcdata "This page contains some demos for some widgets \
               from ocsigen-toolkit."];
    p [pcdata "The different demos are accessible through the drawer\
               menu. To open it click the top left button on the screen."];
    p [pcdata "Feel free to modify the generated code and use it \
               or redistribute it as you want."];
  ]


let%shared () =
  let registerDemo (module D : DemoPage) =
    %%%MODULE_NAME%%%_base.App.register
      ~service:D.service
      (%%%MODULE_NAME%%%_page.Opt.connected_page @@ fun id () () ->
	let%lwt p = D.page () in
	make_page id p)
  in
  List.iter registerDemo demos;
  %%%MODULE_NAME%%%_base.App.register
    ~service:%%%MODULE_NAME%%%_services.otdemo_service
    (%%%MODULE_NAME%%%_page.Opt.connected_page handler)
