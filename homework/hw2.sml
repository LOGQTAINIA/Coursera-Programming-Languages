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
      | x::xs => case all_except_option(str, xs) of
                     NONE => if same_string(x, str)
                             then SOME xs
                             else NONE
                   | SOME l => SOME (x::l)

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

fun similar_names (lst, {first=x, middle=y, last=z}) =
    let fun aux (names) =
            case names of
                [] => []
              | head::rest => {first=head, middle=y, last=z}::aux(rest)
    in
        {first=x, middle=y, last=z}::aux(get_substitutions2(lst, x))
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
                 else x::remove_card(xs, c, e) handle IllegalMove => raise e

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
        
