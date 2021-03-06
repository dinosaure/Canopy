open Canopy_utils

type content_t =
  | Markdown of Canopy_article.t

type error_t =
  Unknown
  | Error of string
  | Ok of content_t

let meta_assoc str =
  Re_str.split (Re_str.regexp "\n") str |>
  List.map (fun meta ->
      let reg = Re_str.regexp "\\(.*\\): \\(.*\\)" in
      let _ = Re_str.string_match reg meta 0 in
      let key = Re_str.matched_group 1 meta in
      let value = Re_str.matched_group 2 meta in
      key, value)

let of_string ~uri ~date ~content =
  let splitted_content = Re_str.bounded_split (Re_str.regexp "---") content 2 in
  match splitted_content with
  | [raw_meta;raw_content] ->
     begin
       match meta_assoc raw_meta with
       | meta ->
	  begin
	    match assoc_opt "content" meta with
	    | Some "markdown"
	    | None ->
	       Canopy_article.of_string meta uri date raw_content
	       |> map_opt (fun article -> Ok (Markdown article)) (Error "Error while parsing article")
	    | Some _ -> Unknown
	  end
       | exception _ -> Unknown
     end
  | _ -> Error "No header found"

let to_tyxml = function
  | Markdown m ->
     let open Canopy_article in
     m.title, to_tyxml m

let to_tyxml_listing_entry = function
  | Markdown m -> Canopy_article.to_tyxml_listing_entry m

let find_tag tagname = function
  | Markdown m ->
     let open Canopy_article in
     List.exists ((=) tagname) m.tags

let date = function
  | Markdown m ->
     let open Canopy_article in
     m.date

let compare a b = CalendarLib.Calendar.compare (date b) (date a)
