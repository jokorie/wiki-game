open! Core
module City = String
module Highway = String

let rec list_combo list =
  match list with
  | [] -> []
  | head1 :: head2 :: rest -> (head1, head2) :: list_combo (head2 :: rest)
  | _ -> []
;;

(* We separate out the [Network] module to represent our social network in
   OCaml types. *)
module Network = struct
  (* We can represent our social network graph as a set of connections, where
     a connection represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t = City.t * Highway.t * City.t [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s.
       This is needed to defined our [Network.t] type later. Using this
       [Comparable.Make] functor also gives us immutable maps, which might
       come in handy later. *)
    include Comparable.Make (T)

    let _of_string c1 c2 hw =
      match c1, c2 with
      | c1, c2 -> City.of_string c1, Highway.of_string hw, City.of_string c2
    ;;
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let of_file input_file =
    let all_lines =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.map ~f:(Str.global_replace (Str.regexp {|\.|}) "")
      |> List.map ~f:(Str.global_replace (Str.regexp {| |}) "_")
    in
    let l_all_lines =
      List.map all_lines ~f:(fun string_line ->
        String.split string_line ~on:',')
    in
    let new_connections =
      List.concat_map l_all_lines ~f:(fun l_line ->
        match l_line with
        | hw :: cities ->
          let city_combos = list_combo cities in
          List.concat_map city_combos ~f:(fun (c1, c2) -> [ c1, hw, c2 ])
        | [] -> [])
    in
    Connection.Set.of_list new_connections
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        ignore (input_file : File_path.t);
        failwith "TODO"]
;;

module G = Graph.Imperative.Graph.Concrete (City)

(* We extend our [Graph] structure with the [Dot] API so that we can easily
   render constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
  include G

  (* These functions can be changed to tweak the appearance of the generated
     graph. Check out the ocamlgraph graphviz API
     (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
     for examples of what values can be set here. *)
  let edge_attributes _ = [ `Dir `None ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (c1, _hw, c2) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the
             graph if they don't already exist. *)
          G.add_edge graph c1 c2);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
