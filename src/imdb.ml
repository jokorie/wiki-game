open! Core

(* [get_credits] should take the contents of an IMDB page for an actor and return a list
   of strings containing that actor's main credits. *)
let get_credits contents : string list =
  let open Soup in
  let html_content =
    parse contents $$ "meta[property=og:description]" |> to_list
  in
  let desired_content = R.attribute "content" (List.hd_exn html_content) in
  String.split desired_content ~on:','
;;

let print_credits_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"given an IMDB page for an actor, print out a list of their main credits"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_credits contents) ~f:print_endline]
;;

let command =
  Command.group ~summary:"imdb commands" [ "print-credits", print_credits_command ]
;;
