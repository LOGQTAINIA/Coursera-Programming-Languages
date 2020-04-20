(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, lst) =
    case lst of
        [] => NONE
      | x::xs => if same_string(x, str)
                 then SOME xs
                 else case all_except_option(str, xs) of
                          NONE => NONE
                        | SOME l => SOME(x::l)

fun get_substitutions1 (lst, str) =
    case lst of
        [] => []
      | x::xs => case all_except_option(str, x) of
                     NONE => get_substitutions1(xs, str)
                   | SOME l => l @ get_substitutions1(xs, str)

fun get_substitutions2 (lst, str) =
    let fun aux (lst, acc) =
            case lst of
                [] => acc
              | x::xs => case all_except_option(str, x) of
                             NONE => aux(xs, acc)
                           | SOME l => aux(xs, acc @ l)
    in
        aux(lst, [])
    end

fun similar_names (lst, name) =
    let val {first=x, middle=y, last=z} = name
        fun aux (names) =
            case names of
                [] => []
              | head::rest => {first=head, middle=y, last=z}::aux(rest)
    in
        name::aux(get_substitutions2(lst, x))
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove


(* put your solutions for problem 2 here *)
fun card_color (st, rk) =
    case st of
        Clubs => Black
      | Spades => Black
      | _ => Red

fun card_value (st, rk) =
    case rk of
        Num i => i
      | Ace => 11
      | _ => 10

fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
      | x::xs => if x=c
                 then xs
                 else x::remove_card(xs, c, e)

fun all_same_color (cs) =
    case cs of
        [] => true
      | _::[] => true
      | head::(neck::rest) => (card_color head=card_color neck)
                              andalso all_same_color(neck::rest)

fun sum_cards (cs) =
    let fun aux (lst, acc) =
            case lst of
                [] => acc
              | x::xs => aux(xs, acc + card_value(x))
    in
        aux(cs, 0)
    end

fun score (cs, goal) =
    let val diff = sum_cards cs - goal
        val pre_score = if diff > 0 then diff * 3 else ~diff
    in
        if all_same_color cs then pre_score div 2 else pre_score
    end

fun officiate (cards, moves, goal) =
    let fun aux (cs, ms, hs) =
            case (cs, ms, hs) of
                ([], Draw::xs, _) => score(hs, goal)
              | (_, [], _) => score(hs, goal)
              | (chd::ctl, Draw::xs, hs) => if sum_cards(chd::hs) > goal
                                            then score(chd::hs, goal)
                                            else aux(ctl, xs, chd::hs)
              | (cs, Discard c::xs, hs) => aux(cs, xs, remove_card(hs, c, IllegalMove))
    in
        aux(cards, moves, [])
    end

(* put your solutions for problem 3 here *)
fun nums_of_Aces lst =
    case lst of
        [] => 0
      | (_, Ace)::rest => 1 + nums_of_Aces rest
      | _::rest => nums_of_Aces rest

fun score_challenge (cs, goal) =
    let fun cal_pre_score (diff, aces) =
            let val score = if diff > 0 then diff * 3 else ~diff
            in
                if diff < 0 orelse aces=0
                then score
                else Int.min(score, cal_pre_score(diff - 10, aces - 1))
            end
        val diff = sum_cards cs - goal
        val pre_score = cal_pre_score(sum_cards cs - goal, nums_of_Aces cs)
    in
        if all_same_color cs then pre_score div 2 else pre_score
    end

fun officiate_challenge (cards, moves, goal) =
    let fun sum_least cs =
            sum_cards cs - 10 * (nums_of_Aces cs)
        fun aux (cs, ms, hs) =
            case (cs, ms, hs) of
                ([], Draw::xs, _) => score_challenge(hs, goal)
              | (_, [], _) => score_challenge(hs, goal)
              | (chd::ctl, Draw::xs, hs) => if sum_least(chd::hs) > goal
                                            then score_challenge(chd::hs, goal)
                                            else aux(ctl, xs, chd::hs)
              | (cs, Discard c::xs, hs) => aux(cs, xs, remove_card(hs, c, IllegalMove))
    in
        aux(cards, moves, [])
    end

fun careful_player (cards, goal) =
    let fun find_card (cards, num) =
            case cards of
                [] => NONE
              | c::cs => if card_value c = num
                         then SOME c
                         else find_card(cs, num)
        fun aux (cs, hs, ms) =
            if sum_cards hs - goal = 0
            then ms
            else case cs of
                     [] => ms
                   | c::cs' => let val diff = card_value c + sum_cards hs - goal
                               in
                                   if diff <= 0
                                   then aux (cs', c::hs, ms @ [Draw])
                                   else case find_card(hs, diff) of
                                            NONE => ms
                                          | SOME rc => ms @ [Discard rc, Draw]
                               end
    in
        aux(cards, [], [])
    end
        
