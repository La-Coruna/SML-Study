
datatype suit = Club | Diamond | Heart | Spade
datatype card_value = Jack | Queen | King 
                    | Ace | Num of int
datatype card =  Card of suit * card_value

(*
datatype card = Jack of suit |
                Queen of suit |
                King of suit |
                Ace of suit |
                Num of suit * int
*)


val hands = [Card(Club, Jack), Card(Club, Num(10)), Card(Club, Ace)]
val hands2 = [Card(Club, Jack), Card(Diamond, Num(10)), Card(Club, Ace)]
val hands3 = [Card(Diamond, Num(10)), Card(Club, Ace), Card(Club, Jack)]
val hands4 = [Card(Diamond, Num(10)), Card(Club, Ace), Card(Club, Jack), Card(Spade, Ace)]

fun is_flush (hand: card list) =
  let
    val Card(suit,_) = hd hand
    fun isSameSuit c =
      case c of
        Card(x,_) => x <> suit
  in
    not (List.exists isSameSuit hand)
  end

(* fun is_flush (hand) =
  case hand of
    [] => 
  |  Card(suit,val) *)


fun hasAce (hand: card list) =
  case hand of
    [] => false
  | Card(_,Ace)::xs => true
  | _::xs => hasAce(xs)

fun removeFirstAce (hand: card list) =
  case hand of
    [] => []
  | Card(_,Ace)::xs => xs
  | x::xs => x::(removeFirstAce(xs))

fun simpleSum(hand: card list) =
  case hand of 
        Card (_, Ace)::rest => 11 + simpleSum(rest)
      | Card (_, Num(i))::rest => i + simpleSum(rest)
      | Card (_, _)::rest => 10 + simpleSum(rest)
      | _ => 0


fun blackjack(hand: card list) =
let val sum = simpleSum(hand)
in
    if sum < 21
    then sum
    else if hasAce(hand)
         then 1+blackjack(removeFirstAce hand)
         else sum
end


fun blackjack (hand: card list) =
let fun simpleSum (hand: card list) =
        case hand of 
            Card (_, Ace)::rest => 11 + simpleSum(rest)
           | Card (_, Num(i))::rest => i + simpleSum(rest)
           | Card (_, _)::rest => 10 + simpleSum(rest)
           | _ => 0
  val score = simpleSum(hand)
in
  if score > 21 andalso hasAce hand
  then 1+blackjack(removeFirstAce hand)
  else score
end




fun blackjack (score: int, hand: card list) =
  case hand of
      Card (_, Ace)::rest => let val try1 = blackjack(score+11, rest)
                             in
                               if try1 <= 21 
                               then try1
                               else blackjack(score+1, rest)
                             end
      | Card (_, Num(i))::rest => blackjack(score+i, rest)
      | Card (_, _)::rest => blackjack(score+10, rest)
      | _ => score




fun blackjack2 (score: int, hand: card list) =
  case hand of
      Card (_, Ace)::rest => let val rest_score = blackjack2(score, rest)
                             in
                               if rest_score >= 11 
                               then rest_score+1
                               else rest_score+11
                             end
      | Card (_, Num(i))::rest => blackjack2(score+i, rest)
      | Card (_, _)::rest => blackjack2(score+10, rest)
      | _ => score 
