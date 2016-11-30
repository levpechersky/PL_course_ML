
fun occurences (element, lst) = length(List.filter (fn x => element=x) lst);

fun common_aux (common_x, max_occurences, []) = common_x
| common_aux (common_x, max_occurences, x::xs) = let
    val x_occurences = occurences(common_x, x::xs)
  in
  if x_occurences > max_occurences then
    common_aux(x, x_occurences, xs)
  else
    common_aux(common_x, max_occurences, xs)
  end;

fun common (x::xs) = common_aux(x, 1, x::xs);

fun take (0, _) = []
| take (i, x::xs) = x :: take (i-1, xs);

fun drop (0, xs) = xs
| drop (i, _::xs) = drop (i-1, xs);

fun first_seq [] = []
| first_seq [x] = [x]
| first_seq (x::y::xs) = if y>x then x::first_seq(y::xs) else [x];

fun break_to_seq [] = []
|break_to_seq lst = let
    val seq = first_seq lst
  in
    seq::break_to_seq(drop(length(seq), lst))
  end;

fun max(_, [x]) = x
| max(less, x::xs) = let
  val tail_largest = max(less,xs)
in
  if less(x, tail_largest) then tail_largest else x
end;

fun sequence [] = []
| sequence lst = max((fn(x,y) => length(x)<length(y)), break_to_seq(lst));

fun thin [] = []
| thin(x::xs) = (x, occurences(x,x::xs))::thin(List.filter (fn y => not (y=x)) xs)
