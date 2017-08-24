(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, strlist) =
  case strlist of
      [] => NONE
    | head::tail => if same_string(str, head)
		    then SOME tail
		    else case all_except_option(str, tail) of
			     NONE => NONE
			   | SOME xs => SOME (head::xs)

fun get_substitutions1 (strlists, str) =
  case strlists of
      [] => []
    | head::tail => case all_except_option(str, head) of
			NONE => get_substitutions1(tail, str)
		      | SOME a => a @ get_substitutions1(tail, str) 
					     
fun get_substitutions2 (strlists, str) =
  let fun sub (strlists, acc) =
	case strlists of
	    [] => acc
	  | head::tail => case all_except_option(str, head) of
			      NONE => sub(tail, acc)
			    | SOME a => sub(tail, acc @ a)
  in
      sub(strlists, [])
  end

fun similar_names (strlists, fullname) =
  let val {first=a, middle=b, last=c} = fullname
      fun generate_names sub_lst =
	case sub_lst of
	    [] => []
	  | head::tail =>
	    {first=head, middle=b, last=c}::generate_names(tail)
  in
      fullname::generate_names(get_substitutions1(strlists, a))
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

fun card_color (suit, rank) =
  case suit of
      Spades => Black
    | Clubs => Black
    | Diamonds => Red
    | Hearts => Red

fun card_value (suit, rank) =
  case rank of
      Jack => 10
    | Queen => 10
    | King => 10
    | Ace => 11
    | Num n=> n
	      
fun remove_card (cs, c, e) =
  case cs of
      [] => raise e
    | head::tail => if head = c
		    then tail
		    else head::remove_card(tail, c, e)

fun all_same_color (cards) =
  case cards of
      [] => true
    | _::[] => true
    | head::(neck::tail) => card_color(head) = card_color(neck) andalso
			    all_same_color(neck::tail)

fun sum_cards (cards) =
  let fun aux (lst, acc) =
	case lst of
	    [] => acc
	       | head::tail => aux(tail, card_value(head) + acc)
  in
      aux(cards, 0)
  end

fun score (cards, goal) =
  let val sum = sum_cards(cards)
      fun sum_pre (sum, goal) =
	if sum > goal
	then 3 * (sum-goal)
	else goal-sum		      
  in
      if all_same_color(cards)
      then sum_pre(sum, goal) div 2
      else sum_pre(sum, goal)		  
  end


  
