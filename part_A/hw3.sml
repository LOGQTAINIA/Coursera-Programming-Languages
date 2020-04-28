
(* Coursera Programming Languages, Homework 3 *)
infix |>
fun x |> f = f x

(* problem 1 *)
val only_capitals =
    List.filter (fn s => (s, 0) |> String.sub |> Char.isUpper)

(* problem 2 *)
val longest_string1 =
    foldl (fn (s1, s2) => if String.size s1 > String.size s2
                          then s1
                          else s2)
          ""

(* problem 3 *)
val longest_string2 =
    foldl (fn (s1, s2) => if String.size s1 >= String.size s2
                          then s1
                          else s2)
          ""

(* problem 4 *)
fun longest_string_helper cmp =
    foldl (fn (s1, s2) => if cmp (String.size s1, String.size s2)
                          then s1
                          else s2)
          ""

val longest_string3 = longest_string_helper (fn (a, b) => a > b)
val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

(* problem 5 *)
val longest_capitalized =
    longest_string1 o only_capitals

(* problem 6 *)
val rev_string =
    implode o rev o explode

exception NoAnswer
(* problem 7 *)
fun first_answer f lst =
    case lst of
        [] => raise NoAnswer
      | x::xs => case f x of
                     NONE => first_answer f xs
                   | SOME x => x

(* problem 8 *)
fun all_answers f lst =
    let fun aux (acc, xs) =
            case xs of
                [] => SOME acc
              | x::xs' => case f x of
                              NONE => NONE
                            | SOME l => aux(acc @ l, xs')
    in
        aux([], lst)
    end


(**** for the problem 9-12 ****)

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(* problem 9 *)
val count_wildcards =
    g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths =
    g (fn _ => 1) (String.size)

fun count_some_var (s, p) =
    g (fn _ => 0) (fn x => if x=s then 1 else 0) p

(* problem 10 *)
fun check_pat p =
    let fun gen_var_list p =
            case p of
                Variable x => [x]
              | TupleP ps => List.foldl (fn (p, l) => (gen_var_list p ) @ l)
                                        [] ps
              | ConstructorP(_, p) => gen_var_list p
              | _ => []
        val check_rep =
            List.exists (fn x => count_some_var (x, p) > 1)
    in
        (not o check_rep o gen_var_list) p
    end

(* problem 11 *)
fun match (v, p) =
    case (v, p) of
        (_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const x1, ConstP x2) => if x1=x2 then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if length vs=length ps
                                 then all_answers match (ListPair.zip(vs, ps))
                                 else NONE
      | (Constructor(s1, v), ConstructorP(s2, p)) => if s1=s2
                                                     then match(v, p)
                                                     else NONE
      | _ => NONE

(* problem 12 *)
fun first_match v ps =
    SOME (first_answer (fn p => match(v, p)) ps) handle NoAnswer
                                                        => NONE

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
