(* Lev Pechersky 333815546 levpechersky@campus.technion.ac.il Kfir Taizi 208044610 kfirtaizi@campus.technion.ac.il *)

(*  Gets 2-tuple uncurried function and returns equal curried function *)
fun curry f = fn x => fn y => f(x,y);

(*  Gets 2 parameter curried function and returns equal uncurried function *)
fun uncurry f = fn (x, y) => f x y;

(* Gets single character, returns string of 2 appearances of that character *)
fun dubchar c = str(c) ^ str(c);

(* Gets a char -> 'a function, n - index in string, and a string s and applies f
   on n-th character in string s *)
fun apply_on_nth_char f n s = if n < 0 orelse n >= size(s)
    then
       f(#"!")
    else
       f(String.sub(s, n));

(* Checks whether string s contains valid sequence of parentheses *)
local
  fun head s = String.sub(s, 0);

  fun tail s = String.substring(s, 1, size s - 1);

  fun braces_to_int #")" = ~1
      | braces_to_int #"(" = 1
      | braces_to_int c = 0

  fun balance_aux ("", counter) = counter=0
      | balance_aux (s, counter) = counter>=0 andalso
          balance_aux(tail s, counter + (braces_to_int(head s)));
in
  fun balance s = balance_aux(s, 0);
end;

(* 'a->'b->('a * 'b ->'b)->'b *)
fun sig1 x y (f : 'a * 'b -> 'b) = f(x, y);

(* int * real -> (real -> string) -> bool *)
fun sig2 (n, x) f = f(x/2.0)=str(chr(n));

(* ('a -> 'b -> 'c) -> 'a -> 'b -> 'd -> 'c *)
fun sig3 f x y z = f x y;

(* 'a -> 'b -> int -> int -> int *)
fun sig4 x y m n = m+n;

(* ('a -> 'b) -> 'a -> ('b * 'b -> 'c) -> 'c *)
fun sig5 f x g = g(f x, f x);

(* unit -> unit -> int *)
fun sig6 () () = 1;
