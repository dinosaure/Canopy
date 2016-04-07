open Canopy_utils
open Html5.M

type t = {
  title : string;
  content : string;
  author : string;
  abstract : string option;
  uri : string;
  created: Ptime.t;
  updated: Ptime.t;
  tags: string list;
}

let of_string meta uri created updated content =
  try
    let split_tags = Re_str.split (Re_str.regexp ",") in
    let content = Cow.Markdown.of_string content |> Cow.Html.to_string in
    let author = List.assoc "author" meta in
    let title = List.assoc "title" meta in
    let tags = assoc_opt "tags" meta |> map_opt split_tags [] |> List.map String.trim in
    let abstract = assoc_opt "abstract" meta in
    Some {title; content; author; uri; abstract; created; updated; tags}
  with
  | _ -> None

let to_tyxml article =
  let tags = Canopy_templates.taglist article.tags in
  let str = Printf.sprintf in
  let ((y, m, d), ((hh, mm, ss), _)) = Ptime.to_date_time article.updated in
  let edito = match article.abstract with
    | Some t -> [ div ~a:[a_class ["Edito"]] [pcdata t]]
    | None -> []
  in
  let content = [ Unsafe.data article.content ] in
  [ div ~a:[a_id "main"]
    ([ h2 [pcdata article.title]
     ; div ~a:[a_class ["pupdated"]]
       ([ tags ]
        ++ [pcdata (str "(%s @ %d-%02d-%02d %02d:%02d:%02d)" article.author y m d hh mm ss)]) ]
     ++ edito
     ++ content) ]

let to_tyxml_listing_entry article =
  let date =
    let d, m, y = Ptime.to_date article.updated in
    Printf.sprintf "%d-%02d-%02d" d m y
  in
  li
  [ span ~a:[a_class ["date"]] [pcdata date]
  ; a ~a:[a_href article.uri] [pcdata article.title] ]

let to_atom ({ title; author; abstract; uri; created; updated; tags; content; } as article) =
  let text x : Syndic.Atom.text_construct = Syndic.Atom.Text x in
  let summary = match abstract with
    | Some x -> Some (text x)
    | None -> None
  in
  let categories =
    List.map
      (fun x -> Syndic.Atom.category ~scheme:(Uri.of_string ("/tags/" ^ x)) x)
      tags
  in
  let generate_id ?(root = "") { created; uri; _ } =
    let y, m, d = Ptime.to_date created in
    let relatif = Uri.path @@ Uri.of_string uri in
    let ts = Ptime.Span.to_int_s @@ Ptime.to_span created in
    Printf.sprintf "tag:%s,%d-%d-%d:%s/%a" root y m d relatif
      (fun () -> function Some a -> string_of_int a | None -> "") ts
    |> Uri.of_string
  in
  Syndic.Atom.entry
    ~id:(generate_id article)
    ~content:(Syndic.Atom.Html (None, content))
    ~authors:(Syndic.Atom.author author, [])
    ~title:(text title)
    ~updated
    ?summary
    ~categories
    ~links:[Syndic.Atom.link ~rel:Syndic.Atom.Alternate (Uri.of_string uri)]
    ()
