open! Core

(* type wiki = { url : string ; title : string } [@@deriving compare, sexp]

   let wiki_of_link link = let title = Str.global_replace (Str.regexp
   {|/wiki/|}) link "" in { url = link; title } ;; *)

(* let rec list_combo list = match list with | [] -> [] | head1 :: head2 ::
   rest -> (head1, head2) :: list_combo (head2 :: rest) | _ -> [] ;; *)

(* We separate out the [Network] module to represent our social network in
   OCaml types. *)
module Network = struct
  (* We can represent our social network graph as a set of connections, where
     a connection represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t = string * string [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s.
       This is needed to defined our [Network.t] type later. Using this
       [Comparable.Make] functor also gives us immutable maps, which might
       come in handy later. *)
    include Comparable.Make (T)

    (* let _of_string c1 c2 hw = match c1, c2 with | c1, c2 -> City.of_string
       c1, Highway.of_string hw, City.of_string c2 ;; *)
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  (* let of_file input_file = let all_lines = In_channel.read_lines
     (File_path.to_string input_file) |> List.map ~f:(Str.global_replace
     (Str.regexp {|\.|}) "") |> List.map ~f:(Str.global_replace (Str.regexp
     {| |}) "_") in let l_all_lines = List.map all_lines ~f:(fun string_line
     -> String.split string_line ~on:',') in let new_connections =
     List.concat_map l_all_lines ~f:(fun l_line -> match l_line with | hw ::
     cities -> let city_combos = list_combo cities in List.concat_map
     city_combos ~f:(fun (c1, c2) -> [ c1, hw, c2 ]) | [] -> []) in
     Connection.Set.of_list new_connections ;; *)
end

module G = Graph.Imperative.Graph.Concrete (String)

(* We extend our [Graph] structure with the [Dot] API so that we can easily
   render constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
  include G

  (* These functions can be changed to tweak the appearance of the generated
     graph. Check out the ocamlgraph graphviz API
     (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
     for examples of what values can be set here. *)
  let edge_attributes _ = [ `Dir `Back ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

(* [get_linked_articles] should return a list of wikipedia article lengths
   contained in the input.

   Note that [get_linked_articles] should ONLY return things that look like
   wikipedia articles. In particular, we should discard links that are: -
   Wikipedia pages under special namespaces that are not articles (see
   https://en.wikipedia.org/wiki/Wikipedia:Namespaces) - other Wikipedia
   internal URLs that are not articles - resources that are external to
   Wikipedia - page headers

   One nice think about Wikipedia is that stringent content moderation
   results in uniformity in article format. We can expect that all Wikipedia
   article links parsed from a Wikipedia page will have the form
   "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  let open Soup in
  parse contents
  $$ "a[href]"
  |> to_list
  |> List.map ~f:(fun a -> R.attribute "href" a)
  |> List.filter ~f:(fun link -> String.is_prefix link ~prefix:"/wiki/")
  |> List.filter ~f:(fun link ->
       match Wikipedia_namespace.namespace link with
       | None -> true
       | _ -> false)
  |> Set.of_list (module String)
  |> Set.to_list
;;

let get_linked_articles_wrapper ~rel_url ~how_to_fetch =
  let abs_url =
    match how_to_fetch with
    | File_fetcher.How_to_fetch.Local _ -> rel_url
    | Remote -> "https://en.wikipedia.org/" ^ rel_url
  in
  print_s [%message abs_url];
  let contents = File_fetcher.fetch_exn how_to_fetch ~resource:abs_url in
  print_s [%message (get_linked_articles contents : string list)];
  get_linked_articles contents
;;

(* |> List.map ~f:(fun li -> texts li |> String.concat ~sep:"" |>
   String.strip) |> List.filter ~f:(fun href_link) *)

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

let clean_string string =
  Str.global_replace (Str.regexp {|wiki/|}) "" string
  |> Str.global_replace (Str.regexp {|/|}) ""
  |> Str.global_replace (Str.regexp {|(|}) ""
  |> Str.global_replace (Str.regexp {|)|}) ""
;;

let rec dfs graph rel_url visited depth_rem how_to_fetch =
  match depth_rem >= 0 with
  | true ->
    Hash_set.add visited rel_url;
    let wiki_children = get_linked_articles_wrapper ~rel_url ~how_to_fetch in
    List.iter wiki_children ~f:(fun child_url ->
      let rel_url_stored = clean_string rel_url in
      let child_url_stored = clean_string child_url in
      G.add_edge graph rel_url_stored child_url_stored;
      if not (Hash_set.mem visited child_url)
      then dfs graph child_url visited (depth_rem - 1) how_to_fetch
      else ())
  | false -> ()
;;

(* [visualize] should explore all linked articles up to a distance of
   [max_depth] away from the given [origin] article, and output the result as
   a DOT file. It should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory. *)
let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let graph = G.create () in
  let visited = String.Hash_set.create () in
  dfs graph origin visited max_depth how_to_fetch;
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
;;

(* let network = Network.of_file input_file in let graph = G.create () in
   Set.iter network ~f:(fun (c1, _hw, c2) -> (* [G.add_edge] auomatically
   adds the endpoints as vertices in the graph if they don't already exist.
   *) G.add_edge graph c1 c2); Dot.output_graph (Out_channel.create
   (File_path.to_string output_file)) graph; printf !"Done! Wrote dot file to
   %{File_path}\n%!" output_file *)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and
   the destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the
   graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
