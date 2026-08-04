open Command
open OUnit2
open Play

let string_of_positions positions =
  positions
  |> List.map Pos.string
  |> String.concat ", "
  |> Printf.sprintf "[%s]"

let assert_positions_equal expected actual =
  assert_equal ~printer:string_of_positions
    (List.sort compare expected)
    (List.sort compare actual)

let test_initial_board _ctx =
  let board = init_board () in
  assert_equal 2 (count board Color.black);
  assert_equal 2 (count board Color.white);
  assert_positions_equal
    [ (4, 3); (3, 4); (6, 5); (5, 6) ]
    (valid_moves board Color.black)

let test_move_is_immutable_and_flips_stones _ctx =
  let board = init_board () in
  let after_d3 = do_move board (Mv (4, 3)) Color.black in
  assert_equal 2 (count board Color.black);
  assert_equal 2 (count board Color.white);
  assert_equal 4 (count after_d3 Color.black);
  assert_equal 1 (count after_d3 Color.white);
  assert_equal 3 (List.length (valid_moves after_d3 Color.white))

let test_pass_does_not_change_the_board _ctx =
  let board = init_board () in
  let after_pass = do_move board Pass Color.black in
  assert_equal 2 (count after_pass Color.black);
  assert_equal 2 (count after_pass Color.white)

let suite =
  "play bitboard"
  >::: [
         "initial board" >:: test_initial_board;
         "move is immutable and flips stones" >:: test_move_is_immutable_and_flips_stones;
         "pass preserves board" >:: test_pass_does_not_change_the_board;
       ]

let () = run_test_tt_main suite
