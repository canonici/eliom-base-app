(* Eliom-base-app
 * http://www.ocsigen.org/eliom-base-app
 *
 * Copyright (C) 2014
 *      Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)


module type Data_storage = sig
  type key
  type data
  val get          : key -> data option Lwt.t
  val add          : key -> data -> unit Lwt.t
  val remove       : key -> unit Lwt.t
end

module type S = sig
  type key
  type data
  val table_name    : string
  val string_of_key : key -> string
  val add_data      : data -> data -> data
end

module Make (D : S) : (
  Data_storage with type key  := D.key
	       and  type data := D.data
) = struct
    
  let db = Ocsipersist.open_table D.table_name

  let lock = Lwt_mutex.create ()

  let locked f =
    let%lwt () = Lwt_mutex.lock lock in
    let%lwt r = f () in
    Lwt_mutex.unlock lock;
    Lwt.return r

  let get key = locked @@ fun () ->
    try%lwt
      let%lwt db = db in
      let%lwt data = Ocsipersist.find db (D.string_of_key key) in
      Lwt.return @@ Some data
    with Not_found ->
      Lwt.return None

  let add key value = locked @@ fun () ->
    let%lwt db = db in
    let%lwt data = get key in
    match data with
    | Some data ->
      Ocsipersist.add db (D.string_of_key key) (D.add_data data value) 
    | None ->
      Ocsipersist.add db (D.string_of_key key) value

  let remove key = locked @@ fun () ->
    let%lwt db = db in
    Ocsipersist.remove db (D.string_of_key key)

end

module Tips_data = Make (struct
  type key = int64
  type data = string list
  let table_name = "tips_data"
  let string_of_key = Int64.to_string
  let add_data old_data new_data = old_data @ new_data
end)


let%server get_tips_seen userid () =
  let%lwt data = Tips_data.get userid in
  Lwt.return @@
    match data with
    | Some data -> data
    | None -> []
      
let%client get_tips_seen =
  let rpc =
    ~%(Eliom_client.server_function [%derive.json: unit] 
	 (Eba_session.connected_rpc get_tips_seen))
  in
  fun (_:int64) () -> rpc ()

let%server set_tip_seen userid name = Tips_data.add userid [name]

let%client set_tip_seen =
  let rpc =
    ~%(Eliom_client.server_function [%derive.json : string]
	 (Eba_session.connected_rpc set_tip_seen))
  in
  fun (_:int64) name -> rpc name


(* I want to see the tips again *)
let%server reset_tips_user userid =
  Tips_data.remove userid

let reset_tips () =
  Eliom_lib.Option.Lwt.iter
    reset_tips_user
    (Eba_current_user.Opt.get_current_userid ())

let%server reset_tips_service =
  Eliom_service.create
    ~name:"resettips"
    ~id:Eliom_service.Global
    ~meth:
      (Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    ()

let%client reset_tips_service = ~%reset_tips_service

let%server _ =
  Eliom_registration.Action.register
    ~service:reset_tips_service
    (Eba_session.connected_fun (fun myid () () -> reset_tips_user myid))

let%client reset_tips () =
  ~%(Eliom_client.server_function
       ~name:"Eba_tips.reset_tips"
       [%derive.json: unit]
       (Eba_session.connected_wrapper reset_tips))
    ()


(* Returns a block containing a tip,
   if it has not already been seen by the user. *)
let%shared block ?(a = []) ~name ~content () =
  let myid_o = Eba_current_user.Opt.get_current_userid () in
  match myid_o with
  | None ->
    Lwt.return None
  | Some myid ->
    let%lwt seen = get_tips_seen myid () in
    if List.mem name seen
    then Lwt.return None
    else begin
      let close_button = Ot_icons.D.close () in
      let box =
        Eliom_content.Html.D.div
	  ~a:(Eliom_content.Html.D.a_class [ "tip" ; "block" ]::a)
	  (close_button :: content)
      in
      ignore [%client
        (Lwt_js_events.(async (fun () ->
           clicks (Eliom_content.Html.To_dom.of_element ~%close_button)
             (fun ev _ ->
                let () = Eliom_content.Html.Manip.removeSelf ~%box in
                Lwt.async (fun () ->
		  (Eba_session.connected_rpc set_tip_seen) ~%name);
                Lwt.return ()
             )))
         : unit)];
      Lwt.return (Some box)
    end

(* This thread is used to display only one tip at a time: *)
let%client waiter = ref (let%lwt _ = Lwt_js_events.onload () in Lwt.return ())

(* Display a tip bubble *)
let%client display_bubble ?(a = [])
    ?arrow ?top ?left ?right ?bottom ?height ?width
    ?(parent_node : _ Eliom_content.Html.elt option)
    ~name ~content ()
  =
  let current_waiter = !waiter in
  let new_waiter, new_wakener = Lwt.wait () in
  waiter := new_waiter;
  let%lwt () = current_waiter in
  let bec = Eliom_content.Html.D.div
    ~a:[Eliom_content.Html.D.a_class ["bec"]] []
  in
  let close_button = Ot_icons.D.close () in
  let box =
    Eliom_content.Html.D.div
      ~a:(Eliom_content.Html.D.a_class [ "tip" ; "bubble" ]::a)
      (close_button::match arrow with None -> content | _ -> bec::content)
  in
  Lwt_js_events.(async (fun () ->
    clicks (Eliom_content.Html.To_dom.of_element close_button)
      (fun ev _ ->
         let () = Eliom_content.Html.Manip.removeSelf box in
         Lwt.async (fun () ->
	   (Eba_session.connected_rpc set_tip_seen) (name : string));
         Lwt.wakeup new_wakener ();
         Lwt.return ()
      )));
  let parent_node = match parent_node with
    | None -> Dom_html.document##.body
    | Some p -> Eliom_content.Html.To_dom.of_element p
  in
  Dom.appendChild parent_node (Eliom_content.Html.To_dom.of_element box);
  let box = Eliom_content.Html.To_dom.of_element box in
  Eliom_lib.Option.iter
    (fun v -> box##.style##.top := Js.string (Printf.sprintf "%ipx" v))
    top;
  Eliom_lib.Option.iter
    (fun v -> box##.style##.left := Js.string (Printf.sprintf "%ipx" v))
    left;
  Eliom_lib.Option.iter
    (fun v -> box##.style##.right := Js.string (Printf.sprintf "%ipx" v))
    right;
  Eliom_lib.Option.iter
    (fun v -> box##.style##.bottom := Js.string (Printf.sprintf "%ipx" v))
    bottom;
  Eliom_lib.Option.iter
    (fun v -> box##.style##.width := Js.string (Printf.sprintf "%ipx" v))
    width;
  Eliom_lib.Option.iter
    (fun v -> box##.style##.height := Js.string (Printf.sprintf "%ipx" v))
    height;
  Eliom_lib.Option.iter
    (fun a ->
       let bec = Eliom_content.Html.To_dom.of_element bec in
       match a with
       | `top i ->
         bec##.style##.top := Js.string "-11px";
         bec##.style##.left := Js.string (Printf.sprintf "%ipx" i);
         bec##.style##.borderBottom := Js.string "none";
         bec##.style##.borderRight := Js.string "none"
       | `left i ->
         bec##.style##.left := Js.string "-11px";
         bec##.style##.top := Js.string (Printf.sprintf "%ipx" i);
         bec##.style##.borderTop := Js.string "none";
         bec##.style##.borderRight := Js.string "none"
       | `bottom i ->
         bec##.style##.bottom := Js.string "-11px";
         bec##.style##.left := Js.string (Printf.sprintf "%ipx" i);
         bec##.style##.borderTop := Js.string "none";
         bec##.style##.borderLeft := Js.string "none"
       | `right i ->
         bec##.style##.right := Js.string "-11px";
         bec##.style##.top := Js.string (Printf.sprintf "%ipx" i);
         bec##.style##.borderBottom := Js.string "none";
         bec##.style##.borderLeft := Js.string "none"
    )
    arrow;
  Lwt.return ()

(* Function to be called on server to display a tip *)
let%shared bubble ?a ?arrow ?top ?left ?right ?bottom ?height ?width
    ?parent_node ~(name : string) ~content () =
  let myid_o = Eba_current_user.Opt.get_current_userid () in
  match myid_o with
  | None ->
    Lwt.return ()
  | Some myid ->
    let%lwt seen = get_tips_seen myid () in
    if List.mem name seen
    then Lwt.return ()
    else let _ = [%client ( Lwt.async (fun () ->
              display_bubble ?a:~%a ?arrow:~%arrow
                ?top:~%top ?left:~%left ?right:~%right ?bottom:~%bottom
                ?height:~%height ?width:~%width
                ?parent_node:~%parent_node
                ~name:(~%name : string)
                ~content:~%content
                ())
                            : unit)]
      in
      Lwt.return ()
