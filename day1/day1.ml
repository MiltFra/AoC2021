let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let get_input filename =
  let lines = read_file filename in
  List.map int_of_string lines

let rec firstk k xs =
  match xs with
  | [] -> failwith "firstk"
  | x :: xs -> if k = 1 then [ x ] else x :: firstk (k - 1) xs

let sum_first_k k xs =
  if List.length xs < k then 0
  else
    let window = firstk k xs in
    List.fold_left ( + ) 0 window

let rec count_increases k xs =
  let compare x y = if x < y then 1 else 0 in
  if List.length xs < k + 1 then 0
  else
    let tl = List.tl xs in
    let first = sum_first_k k xs in
    let second = sum_first_k k tl in
    compare first second + count_increases k tl

let () =
  let xs = get_input "day1.in" in
  let answer1 = count_increases 1 xs in
  let answer2 = count_increases 3 xs in
  Printf.printf "Problem 1: %d\nProblem 2: %d\n" answer1 answer2
