(* Ocsigen-start
 * http://www.ocsigen.org/ocsigen-start
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

(** Tips for new users and new features. *)

[%%shared.start]
(** Display tips in pages, as a speech bubble.

    One tip is displayed at a time.

    Tips can be inserted in page using function [display],
    that may be called anywhere during the generation of a page.
    The tip will be actually sent and displayed on client side
    only if the user has not already seen it.

    - [~name] is a unique name you must choose for your tip
    - [?arrow] is the position of the arrow if you want one
    - [?top], [?bottom], [?left], [?right], [?width], [?right] correspond
    to the eponymous CSS properties.
    - [~content] takes the function closing the tip as parameter
    and return the content of the tip div.

*)
val bubble :
  ?a:[< Html_types.div_attrib > `Class ] Eliom_content.Html.D.attrib list ->
  ?arrow: [< `left of int
          | `right of int
          | `top of int
          | `bottom of int ] ->
  ?top:int ->
  ?left:int ->
  ?right:int ->
  ?bottom:int ->
  ?height:int ->
  ?width:int ->
  ?parent_node:[< `Body | Html_types.body_content ] Eliom_content.Html.elt ->
  name:string ->
  content:((unit -> unit Lwt.t) Eliom_client_value.t
           -> Html_types.div_content Eliom_content.Html.elt list) ->
  unit ->
  unit Lwt.t

(** Return a box containing a tip, to be inserted where you want in a page.
    The box contains a close button. Once it is closed, it is never displayed
    again for this user. In that case the function returns [None].
 *)
val block :
  ?a:[< Html_types.div_attrib > `Class ] Eliom_content.Html.D.attrib list ->
  name:string ->
  content:((unit -> unit Lwt.t) Eliom_client_value.t
           -> Html_types.div_content Eliom_content.Html.elt list) ->
  unit ->
  [> `Div ] Eliom_content.Html.elt option Lwt.t

(** Call this function to reset tips for current user.
    Tips will be shown again from the beginning.
*)
val reset_tips : unit -> unit Lwt.t

[%%shared.start]
(** A non-attached service that will reset tips.
    Call it with [Eliom_client.exit_to] to restart the application and
    see tips again. *)
val reset_tips_service :
  (unit, unit, Eliom_service.post, Eliom_service.non_att,
   Eliom_service.co, Eliom_service.non_ext, Eliom_service.reg,
   [ `WithoutSuffix ], unit, unit,
   Eliom_service.non_ocaml)
    Eliom_service.t

[%%client.start]
(** Call this function to reset tips for current users.
    Tips will be shown again from the beginning.
*)
val reset_tips : unit -> unit Lwt.t
