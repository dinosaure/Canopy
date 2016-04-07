open Canopy_config
open Canopy_utils
open Html5.M

module StringPrinter = struct
    type out = string
    type m = string

    let empty = ""
    let concat = (^)
    let put a = a
    let make a = a
end

module StringHtml = Html5.Make_printer(StringPrinter)

let empty =
  div []

let taglist tags =
  let format_tag tag =
    let taglink = Printf.sprintf "/tags/%s" in
    a ~a:[taglink tag |> a_href; ] [pcdata tag] in
  match tags with
  | [] -> empty
  | tags ->
    let tags = List.map format_tag tags in
    div ~a:[a_class ["tags"]] tags

let links keys =
  let paths =
    List.map (function
              | x :: _ -> x
              | _ -> assert false) keys
    |> List.sort_uniq (Pervasives.compare) in
  let format_link link =
    li [ a ~a:[a_href ("/" ^ link)] [span [pcdata link]]] in
 List.map format_link paths

let script_mathjax =
  [script ~a:[a_src "https://travis-ci.org/Engil/Canopy"] (pcdata "")]

let main ~config ~content ~title ~keys =
  let mathjax = if config.mathjax then script_mathjax else [] in
  let page =
    html
      (head
        (Html5.M.title (pcdata title))
        ([ meta ~a:[a_charset "UTF-8"] ()
         ; link ~rel:[`Stylesheet] ~href:"/static/css/style.css" ()
         ; link ~rel:[`Alternate] ~href:"/atom" ~a:[a_title title; a_mime_type "application/atom+xml"] ()
         ] ++ mathjax))
      (body
      [ div ~a:[a_class ["banner"]]
        [ h1 [ a ~a:[a_href "/"] [pcdata title]
             ; span ~a:[a_class ["dashed"]]
               [ pcdata "["
               ; a ~a:[a_href "/atom"]
                 [ img ~src:"/static/feed.png" ~alt:"[Atom]"~a:[a_class ["feedicon"]] ()]
               ; pcdata " abridged edition]" ]]]
      ; hr ()
      ; div ~a:[a_id "main"] content
      ; hr () ])
  in
  StringHtml.print page

let listing entries = [ ul ~a:[a_class ["index"]] entries ]

let error msg =
  [div ~a:[a_class ["alert alert-danger"]] [pcdata msg]]
