module Json = Yojson.Raw

exception Non_standard_format of Json.t

type atom = Empty | Yes | No | Number of string | Text of string

type mjs =
  | Atom of atom
  | Lst of (mjs list * bool)
  | NamedLst of ((string * mjs) list * bool)

let rec to_mjs (js : Json.t) =
  match js with
  | `Null -> Atom Empty
  | `Bool true -> Atom Yes
  | `Bool false -> Atom No
  | `Intlit num -> Atom (Number num)
  | `Floatlit num -> Atom (Number num)
  | `Stringlit txt -> Atom (Text (String.sub txt 1 (String.length txt - 2)))
  | `List xs ->
      let items = List.map to_mjs xs in
      let simple =
        List.for_all
          (function
            | Atom _ -> true | Lst (_, simple) -> simple | NamedLst _ -> false)
          items
      in
      Lst (items, simple)
  | `Assoc xs ->
      let names, xs = List.split xs in
      let items = List.map to_mjs xs in
      let simple =
        List.for_all (function Atom _ -> true | _ -> false) items
      in
      NamedLst (List.combine names items, simple)
  | `Tuple _ | `Variant _ -> raise (Non_standard_format js)

open Tyxml_html

let to_span = function
  | Empty -> span ~a:[ a_class [ "empty" ] ] []
  | Yes -> span ~a:[ a_class [ "yes" ] ] [ entity "check" ]
  | No -> span ~a:[ a_class [ "no" ] ] [ entity "cross" ]
  | Number n -> span ~a:[ a_class [ "number" ] ] [ txt n ]
  | Text t -> span ~a:[ a_class [ "text" ] ] [ txt t ]

let rec to_html = function
  | Atom atom -> to_span atom
  | Lst (items, true) ->
      items |> List.map (fun item -> li [ to_html item ]) |> ol
  | Lst (items, false) ->
      items |> List.map (fun item -> section [ to_html item ]) |> div
  | NamedLst (named_items, true) ->
      named_items
      |> List.map (fun (name, item) ->
             tr [ th [ txt name ]; td [ to_html item ] ])
      |> table
  | NamedLst (named_items, false) ->
      named_items
      |> List.concat_map (fun (name, item) ->
             [ dt [ txt name ]; dd [ to_html item ] ])
      |> dl

let () =
  Json.from_channel stdin |> to_mjs |> to_html
  |> Format.asprintf "%a" (pp_elt ())
  |> print_endline
