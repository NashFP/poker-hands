(*
  This is not a pretty solution. It does not define types to represent
  cards. Instead, it does some base-13 math to create a hand ranking
  where the higher number wins.
 *)

open Option

module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)

(* Since 5 cards would make a 5-digit base-13 number,
   the size of a hand is 13 ^ 5 *)
let hand_size = 13 * 13 * 13 * 13 * 13

(* High card is the lowest *)
let high_card = 0
(* The lowest one_pair hand is higher than the highest high_card,
   this is the same on down to straight flush. *)
let one_pair = hand_size
let two_pairs = 2 * hand_size
let three_of_a_kind = 3 * hand_size
let straight = 4 * hand_size
let flush = 5 * hand_size
let full_house = 6 * hand_size
let four_of_a_kind = 7 * hand_size
let straight_flush = 8 * hand_size

(* No need to define a royal flush, it's just the highest-valued straight flush *)

(* Don't care what value each suit has just as long as each suit has a unique value *)
let int_of_suit = function
  | 'H' -> 0
  | 'C' -> 1
  | 'S' -> 2
  | 'D' -> 3
  | _ -> failwith "Invalid suit"

(* Cards are numbered 0-12 instead of 2-14 to make the base-13 math easier *)
let int_of_rank r =
  match r with
  | 'A' -> 12
  | 'K' -> 11
  | 'Q' -> 10
  | 'J' -> 9
  | 'T' -> 8
  | '2' .. '9' -> (Char.code r) - (Char.code '2')
  | _ -> failwith "Invalid rank"

let parse_card s =
  (int_of_rank s.[0], int_of_suit s.[1])

(* Treat the list of card values as a base-13 number and convert it to an int *)
let cards_value lst =
  List.fold_left (fun acc x -> acc * 13 + x) 0 lst

let hand_value hand =
  (* This function is used to count the number of times a key is inserted in a map *)
  let map_sum = function
    | None -> Some 1
    | Some n -> Some (n+1)
  in
  let swap (a,b) = (b,a) in
  (* This function updates a key in a map, which just increments the value, which is the key count *)
  let update_map m (k,_) = IntMap.update k map_sum m in
  (* Create a map of card value to count *)
  let hand_map = List.fold_left update_map IntMap.empty hand in
  (* Create a list of (card_count, card_value) by converting the hand_map to a list,
     swapping each pair so the count is first, sorting, then reversing the sorted list
     so the highest is first *)
  let count_first = List.rev (List.sort Stdlib.compare (List.map swap (IntMap.to_list hand_map))) in
  (* Look at the first count in the list *)
  let first_count = fst (List.hd count_first) in
  (* Make a list of the card ranks where the card that occurs the most is first *)
  let ranks = (List.map snd count_first) in
  (* The number of unique cards is just the number of keys in the hand_map *)
  let num_unique = IntMap.cardinal hand_map in
  (* The hand is a flush if the set of all suits has a size of 1 *)
  let is_flush = 1 == IntSet.cardinal (IntSet.of_list (List.map snd hand)) in
  (* Since the ranks are in descending order, a hand is a straight if there are 5
     unique cards and the different between the first and last card is exactly 4 *)
  let is_straight = num_unique == 5 && (List.hd ranks) - (List.nth ranks 4) == 4 in
  (* Convert the list of ranks into a base-13 number *)
  let rank_value = cards_value ranks in

  (* Compute the type of hand *)
  let hand_type =
    match num_unique with
    | 5 when is_flush && is_straight -> straight_flush
    | 5 when is_flush -> flush
    | 5 when is_straight -> straight
    | 5 -> high_card
    | 4 when is_flush -> flush
    | 4 -> one_pair
    | 3 when first_count == 3 -> three_of_a_kind
    | 3 -> two_pairs
    | 2 when first_count == 4 -> four_of_a_kind
    | 2 -> full_house
    | _ -> failwith "invalid hand"
  in
  hand_type + rank_value

let p1_wins str =
  let cards = Str.split (Str.regexp " +") str in
  let p1_hand = List.map parse_card (List.take 5 cards) in
  let p2_hand = List.map parse_card (List.drop 5 cards) in
  let p1_value = hand_value p1_hand in
  let p2_value = hand_value p2_hand in
  p1_value > p2_value



let run () =
  let lines = In_channel.with_open_text "../data/p054_poker.txt" In_channel.input_lines in
  let p1_win_count = List.length (List.filter p1_wins lines) in
  Printf.printf "Player one wins %d times\n" p1_win_count;;

run ();;
