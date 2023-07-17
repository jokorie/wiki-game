(* Note: This incantation allows us to tell the compiler to temporarily stop
   notifying us when we have unused functions and values. Feel free to delete
   after completing exercise 6. *)
(* [@@@disable_unused_warnings] *)

open Core

module Node_id : sig
  (** A [Node_id.t] uniquely identifies a node in a graph. We will using it
      later for looking up and setting the state of nodes in the course of
      our path search. *)
  type t [@@deriving compare, equal, sexp]

  include Comparable.S with type t := t

  val create : int -> t
end = struct
  module T = struct
    type t = int [@@deriving compare, equal, sexp]
  end

  (* Remember that this is the syntax for include modules such as [Map] and
     [Set] that are provided by [Comparable.Make] to our module. In our case,
     we use [Node_id.Map.t] in the [Nodes.t]. *)
  include T
  include Comparable.Make (T)

  let create id = id
end

module Edge = struct
  (** This type represents an edge between two nodes [a] and [b]. Note that since we are
      working with undirected graphs, the order of [a] and [b] do not matter. That is, an
      [Edge.t] { a = 1; b = 2; ... } is equivalent to { a = 2; b = 1; ... } *)

  module T = struct
    type t =
      { a : Node_id.t
      ; b : Node_id.t
      ; distance : int
      }
    [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Edges = struct
  type t = Edge.t list [@@deriving sexp]

  (* Exercise 1: Given a [t] (list of edges) and a [Node_id.t], implement a
     function that returns a list of neighboring nodes with their
     corresponding distances. *)
  let neighbors (t : Edge.t list) node_id : (Node_id.t * int) list =
    List.fold t ~init:[] ~f:(fun acc { Edge.a; b; distance } ->
      if Node_id.( = ) node_id a
      then acc @ [ b, distance ]
      else if Node_id.( = ) node_id b
      then acc @ [ a, distance ]
      else acc)
  ;;

  (* We've left all of the tets in this file disabled. As you complete the
     exercises, please make sure to remove `[@tags "disabled"]` and run `dune
     runtest` to ensure that your implementation passes the test. *)
  let%expect_test ("neighbors" [@tags "disabled"]) =
    let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      Edge.
        [ { a = n0; b = n1; distance = 1 }
        ; { a = n1; b = n2; distance = 3 }
        ; { a = n2; b = n3; distance = 2 }
        ; { a = n2; b = n4; distance = 1 }
        ; { a = n3; b = n5; distance = 5 }
        ; { a = n4; b = n5; distance = 1 }
        ]
    in
    let neighbors = neighbors t n2 in
    print_s [%message (neighbors : (Node_id.t * int) list)];
    [%expect {| (neighbors ((1 3) (3 2) (4 1))) |}]
  ;;
end

module Node = struct
  module State = struct
    type t =
      | Origin (** Used to mark the node where our search starts *)
      | Unseen (** Used to mark unexplored Nodes *)
      | Todo of
          { distance : int
          ; via : Node_id.t
          }
          (** Used to mark nodes that have been encountered but have not been
              processed yet *)
      | Done of { via : Node_id.t }
          (** Used to mark nodes that we are finished processing *)
    [@@deriving sexp]
  end

  type t = { mutable state : State.t } [@@deriving fields, sexp]

  let init () = { state = Unseen }
  let set_state t state = t.state <- state
end

module Nodes = struct
  (** This type represents a stateful collection of nodes in our graph. These
      [Node.t]s will be updated in the course of our graph search to keep
      track of progress. *)
  type t = Node.t Node_id.Map.t [@@deriving sexp]

  (* Exercise 2: Given a list of edges, create a [t] that contains all nodes
     found in the edge list. Note that you can construct [Node.t]s with the
     [Node.init] function. *)
  let of_edges edges =
    List.concat_map edges ~f:(fun { Edge.a; b; _ } -> [ a; b ])
    |> Node_id.Set.of_list
    |> Set.to_map ~f:(fun _ -> Node.init ())
  ;;

  let find = Map.find_exn
  let _state t node_id = find t node_id |> Node.state

  let set_state t id state =
    let node = Map.find_exn t id in
    Node.set_state node state
  ;;

  (* Exercise 3: Given a [t], find the next node to process by selecting the
     node with the smallest distance along with its via route. *)
  let next_node t : (Node_id.t * (int * Node_id.t)) option =
    let _empty_node = Node.init () in
    let start_dist = -1 in
    let next_node_id, (best_dist, best_via_id) =
      Map.fold
        t
        ~init:(Node_id.create (-1), (start_dist, Node_id.create (-1)))
        ~f:(fun ~key:node_id
                ~data:curr_node
                (best_node_id, (best_dist, best_via)) ->
          let dist, via =
            match Node.state curr_node with
            | Todo { distance; via } -> distance, via
            | _ -> best_dist, best_via
          in
          if best_dist < 0
          then node_id, (dist, via)
          else (
            match dist < best_dist with
            | true -> node_id, (dist, via)
            | false -> best_node_id, (best_dist, best_via)))
    in
    match best_dist < 0 with
    | true ->
      (* print_s [%message "No next node found"]; *)
      None
    | false ->
      (* print_s [%message "Next Node found"]; *)
      Some (next_node_id, (best_dist, best_via_id))
  ;;

  let%expect_test "next_node" =
    let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      [ n0, { Node.state = Origin }
      ; n1, { Node.state = Done { via = n0 } }
      ; n2, { Node.state = Done { via = n1 } }
      ; n3, { Node.state = Todo { distance = 2; via = n1 } }
      ; n4, { Node.state = Todo { distance = 1; via = n1 } }
      ; n5, { Node.state = Unseen }
      ]
      |> Node_id.Map.of_alist_exn
    in
    let next_node = next_node t in
    print_s [%message (next_node : (Node_id.t * (int * Node_id.t)) option)];
    [%expect {| (next_node ((4 (1 1)))) |}]
  ;;

  (* Exercise 4: Given a [t] that has already been processed from some origin
     -- that is the origin has been marked as [Origin] and nodes on the
     shortest path have been marked as [Done] -- return the path from the
     origin to the given [destination]. *)
  let rec path t (curr_node : Node_id.t) : Node_id.t list =
    (* print_s [%message curr desired_start]; *)
    match Node.state (find t curr_node) with
    | Origin -> [ curr_node ]
    | Done { via } -> path t via @ [ curr_node ]
    | _ ->
      (* print_s [%message "No complete path"]; *)
      []
  ;;

  (* Excercise 5: Write an expect test for the [path] function above. *)
  let%expect_test "path" = ()
end

let rec branch_out
  ~nodes_map
  ~(curr_node : Node_id.t)
  ~edges
  ~(destination : Node_id.t)
  : 'a option
  =
  match Node_id.equal curr_node destination with
  | true -> Some nodes_map
  | false ->
    let neighbors = Edges.neighbors edges curr_node in
    (*maybe I need to filter neighbors*)
    let neighbors =
      List.filter neighbors ~f:(fun (node_id, _) ->
        match Node.state (Map.find_exn nodes_map node_id) with
        | Origin -> false
        | Done _ -> false
        | _ -> true)
    in
    (* print_s [%message "Neighbors are: " (neighbors : (Node_id.t * int) list)]; *)
    let nodes_map =
      List.fold
        neighbors
        ~init:nodes_map
        ~f:(fun curr_map (curr_id, curr_dist) ->
        Nodes.set_state
          nodes_map
          curr_id
          (Todo { distance = curr_dist; via = curr_node });
        curr_map)
    in
    let next_node = Nodes.next_node nodes_map in
    (match next_node with
     | None -> None
     | Some (next_node, _) ->
       let _, prev_node =
         match Node.state (Map.find_exn nodes_map next_node) with
         | Todo { distance; via } -> distance, via
         | _ -> -1, curr_node
       in
       Nodes.set_state nodes_map next_node (Done { via = prev_node });
       branch_out ~nodes_map ~curr_node:next_node ~edges ~destination)
;;

(* Exercise 6: Using the functions and types above, implement Dijkstras graph
   search algorithm! Remember to reenable unused warnings by deleting the
   first line of this file. *)
let shortest_path ~edges ~(origin : Node_id.t) ~destination : Node_id.t list =
  let nodes_map = Nodes.of_edges edges in
  Nodes.set_state nodes_map origin Origin;
  let nodes_map =
    branch_out ~nodes_map ~curr_node:origin ~edges ~destination
  in
  match nodes_map with
  | None ->
    (* print_s [%message "No Map"]; *)
    []
  | Some nodes_map -> Nodes.path nodes_map destination
;;

let%expect_test "shortest_path" =
  let n = Node_id.create in
  let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
  let edges =
    Edge.
      [ { a = n0; b = n1; distance = 1 }
      ; { a = n1; b = n2; distance = 1 }
      ; { a = n2; b = n3; distance = 1 }
      ; { a = n2; b = n4; distance = 1 }
      ; { a = n3; b = n5; distance = 2 }
      ; { a = n4; b = n5; distance = 1 }
      ]
  in
  let origin = n0 in
  let destination = n5 in
  let path = shortest_path ~edges ~origin ~destination in
  print_s ([%sexp_of: Node_id.t list] path);
  [%expect {| (0 1 2 4 5) |}]
;;

(* Exercise 7: Add some more test cases, exploring any corner cases you can
   think of. *)
