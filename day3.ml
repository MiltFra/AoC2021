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

(* ---------------------- Problem 1 ---------------------- *)

let problem1 =
  let rec to_bits x =
    match x with 0 -> [] | _ -> (x mod 2) :: to_bits (x / 2)
  in
  let rec from_bits l =
    match l with [] -> 0 | x :: xs -> x + (2 * from_bits xs)
  in
  let get_input filename =
    let lines = read_file filename in
    let char_to_int c = Char.code c mod 2 in
    let string_to_chars s = List.of_seq (String.to_seq s) in
    List.map
      (fun x -> from_bits (List.map char_to_int (string_to_chars x)))
      lines
  in
  let readings = get_input "day3.txt" in
  let count_bits xs =
    let rec add_to_count l y =
      match l with
      | [] -> to_bits y
      | x :: xs -> (x + (y mod 2)) :: add_to_count xs (y / 2)
    in
    List.fold_left add_to_count [] xs
  in
  let num_readings = List.length readings in
  let counted_bits = count_bits readings in
  let common_bits =
    List.map (fun x -> if x <= num_readings / 2 then 0 else 1) counted_bits
  in
  let gamma_rate readings = from_bits (List.rev common_bits) in
  let epsilon_rate readings =
    from_bits (List.rev (List.map (fun x -> 1 - x) common_bits))
  in
  gamma_rate readings * epsilon_rate readings

(* ---------------------- Problem 2 ---------------------- *)

let problem2 =
  let get_input filename =
    let char_to_bit c = Char.code c mod 2 in
    let string_to_chars s = List.of_seq (String.to_seq s) in
    let lines = read_file filename in
    let char_lists = List.map string_to_chars lines in
    List.map (List.map char_to_bit) char_lists
  in
  let sum l =
    let rec aux acc = function [] -> acc | h :: t -> aux (acc + h) t in
    aux 0 l
  in
  let readings = get_input "day3.txt" in
  let bits_at_k l k = List.map (fun x -> List.nth x k) l in
  let most_common_bit_at_k l k =
    if sum (bits_at_k l k) >= (List.length l + 1) / 2 then 1 else 0
  in
  let least_common_bit_at_k l k =
    if sum (bits_at_k l k) < (List.length l + 1) / 2 then 1 else 0
  in
  let bit_at_k_is l k bit = bit == List.nth l k in
  let filter_by_criteria criteria l =
    let rec filter_by_criteria_from_k criteria l k =
      let filtered = List.filter (criteria l k) l in
      let () =
        List.iter
          (fun x ->
            List.iter (Printf.printf "%d ") x;
            Printf.printf "\n")
          filtered;
        Printf.printf "----------------\n"
      in
      match filtered with
      | [] -> failwith "filter_by_criteria"
      | [ x ] -> x
      | xs -> filter_by_criteria_from_k criteria xs (k + 1)
    in
    filter_by_criteria_from_k criteria l 0
  in
  let rec from_bits l =
    match l with [] -> 0 | x :: xs -> x + (2 * from_bits xs)
  in
  let o2_rating =
    let () = Printf.printf "02 rating:\n\n" in
    let bits =
      filter_by_criteria
        (fun l k x -> bit_at_k_is x k (most_common_bit_at_k l k))
        readings
    in
    let () = Printf.printf "\n" in
    from_bits (List.rev bits)
  in
  let co2_rating =
    let () = Printf.printf "C02 rating:\n\n" in
    let bits =
      filter_by_criteria
        (fun l k x -> bit_at_k_is x k (least_common_bit_at_k l k))
        readings
    in
    let () = Printf.printf "\n" in
    from_bits (List.rev bits)
  in
  let () = Printf.printf "%d %d\n" o2_rating co2_rating in
  o2_rating * co2_rating

let () = Printf.printf "Problem 1: %d\nProblem 2: %d\n" problem1 problem2
