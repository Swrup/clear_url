open Yojson.Basic.Util

let file =
  match Fpath.of_string "./src/rules.json" with
  | Ok f -> f
  | Error (`Msg s) -> failwith s

let parsed =
  match Bos.OS.File.read file with
  | Error (`Msg s) -> failwith s
  | Ok s -> Yojson.Safe.to_basic (Yojson.Safe.from_string s)

type regexps =
  { urlpattern : Re.re
  ; rawrules : Re.re array
  ; rules : Re.re array
  ; exceptions : Re.re array
  ; redirections : Re.re array
  }

let values = parsed |> member "providers" |> values |> Array.of_list

let regexps_array =
  Array.map
    (fun value ->
      let urlpattern =
        value |> member "urlPattern" |> to_string |> Re.Perl.re |> Re.compile
      in

      let get_array key =
        value
        |> (fun el ->
             (*we can't use `filter_member "rules"` here because empty list 
              * where removed from the file ..*)
             match member key el with
             | `Null -> (*it was an empty list *) []
             | json -> to_list json )
        |> filter_string |> Array.of_list
      in
      let compile_array l =
        l |> Array.map (fun s -> Re.compile @@ Re.Perl.re s)
      in

      (* referralMarketing should be in rules *)
      let rules =
        Array.append (get_array "rules") (get_array "referralMarketing")
        |> Array.map (Format.sprintf "(?:&amp;|[/?#&])(?:%s=[^&]*)")
        |> compile_array
      in
      let rawrules = get_array "rawRules" |> compile_array in
      let exceptions = get_array "exceptions" |> compile_array in
      let redirections = get_array "redirections" |> compile_array in

      { urlpattern; rawrules; rules; exceptions; redirections } )
    values

let split_queries_regex = Re.compile (Re.Perl.re "[^\\/|\\?|&]+=[^\\/|\\?|&]+")

(* see https://docs.clearurls.xyz/1.23.0/specs/rules/ *)
let clean url =
  (* filter by url pattern *)
  let regexps_list =
    Array.fold_left
      (fun acc regexps ->
        if Re.matches regexps.urlpattern url <> [] then regexps :: acc else acc
        )
      [] regexps_array
  in

  let is_exception =
    List.exists
      (fun t ->
        Array.exists (fun except -> Re.matches except url <> []) t.exceptions )
      regexps_list
  in
  if is_exception then url
  else
    (*TODO implement redirection, is it always percent-encoded ?? *)

    (* apply rawrules to whole url *)
    let url =
      List.fold_left
        (fun acc t ->
          Array.fold_left
            (fun acc rawrule -> Re.replace_string ~all:true rawrule ~by:"" acc)
            acc t.rawrules )
        url regexps_list
    in

    let uri = Uri.of_string url in
    match Uri.verbatim_query uri with
    | None -> url
    | Some query ->
      let query = "?" ^ query in
      let fragment =
        Option.fold ~none:"" ~some:(fun s -> "#" ^ s) (Uri.fragment uri)
      in
      (* let query = "?" ^ Re.replace_string get_query_regex ~by:"" url in *)
      let base =
        String.sub url 0
          (String.length url - String.length query - String.length fragment)
      in

      (* apply rules to query *)
      let apply_rules s =
        List.fold_left
          (fun s t ->
            Array.fold_left
              (fun acc rule -> Re.replace_string ~all:true rule ~by:"" acc)
              s t.rules )
          s regexps_list
      in

      let clean_query = apply_rules query in

      (* TODO fragment might contains tracking fields as well (really?) *)
      let query_list = Re.matches split_queries_regex clean_query in

      let pp_field_list =
        Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "&")
          Format.pp_print_string
      in
      let pp_query fmt query_list =
        match query_list with
        | [] -> Format.fprintf fmt ""
        | query_list -> Format.fprintf fmt "?%a" pp_field_list query_list
      in
      Format.asprintf "%s%a%s" base pp_query query_list fragment

(* TODO implement link unshortening like https://github.com/AmanoTeam/Unalix/blob/master/unalix/core/url_unshort.py *)
