open! Core

module Position = struct
  module T = struct
    type t = int * int [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

let all_lines input_file =
  let lines =
    Array.of_list (In_channel.read_lines (File_path.to_string input_file))
  in
  let coord_array =
    Array.map lines ~f:(fun string_line ->
      Array.of_list (String.to_list string_line))
  in
  coord_array
;;

let get_pos ~board ~row ~col ~r_max ~c_max =
  (* row col is already checked by in bounds. this function not called
     directly *)
  ignore r_max;
  ignore c_max;
  Array.get (Array.get board row) col
;;

let in_bounds ~row ~col ~r_max ~c_max ~board =
  row >= 0
  && row < r_max
  && col >= 0
  && col < c_max
  && not (Char.( = ) (get_pos ~board ~row ~col ~r_max ~c_max) '#')
;;

let dir_offsets = [ 0, 1; 0, -1; 1, 0; -1, 0 ]

let get_neighbors ~board ~row ~col ~r_max ~c_max =
  List.filter_map dir_offsets ~f:(fun (r_delta, c_delta) ->
    let new_r = row + r_delta in
    let new_c = col + c_delta in
    match in_bounds ~board ~row:new_r ~col:new_c ~r_max ~c_max with
    | true -> Some (new_r, new_c)
    | false -> None)
;;

let rec dfs ~board ~path ~row ~col ~r_max ~c_max ~visited =
  Hash_set.add visited (row, col);
  match Char.( = ) (get_pos ~board ~row ~col ~r_max ~c_max) 'E' with
  | true -> Some (path @ [ row, col ])
  | false ->
    let child_moves = get_neighbors ~board ~row ~col ~r_max ~c_max in
    let path =
      List.filter_map child_moves ~f:(fun (child_row, child_col) ->
        if not (Hash_set.mem visited (child_row, child_col))
        then
          dfs
            ~board
            ~path:(path @ [ row, col ])
            ~row:child_row
            ~col:child_col
            ~r_max
            ~c_max
            ~visited
        else None)
    in
    (match path with [] -> None | head :: _rest -> Some head)
;;

let command ~input_file =
  let board = all_lines input_file in
  (* print_s [%message (board : char array array)]; *)
  let visited = Position.Hash_set.create () in
  let start_row, start_col = 1, 0 in
  let r_max = Array.length board in
  let c_max = Array.length (Array.get board 0) in
  match
    dfs ~board ~path:[] ~row:start_row ~col:start_col ~r_max ~c_max ~visited
  with
  | None -> []
  | Some path -> path
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let soln = command ~input_file in
        print_s [%message (soln : (int * int) list)]]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
