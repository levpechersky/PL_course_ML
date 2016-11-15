(* Raises exception for empty list *)
fun last [m] = m
	| last (_::rest) = last(rest);

(* Raises exception for empty list *)
fun first (m::_) = m;

fun length [] = 0
	| length (_::rest) = 1 + length(rest);

fun empty [] = true
	| empty (_::_) = false;

fun pop_front [] = []
	| pop_front [_] = []
	| pop_front (_::rest) = rest;

fun first_n ([], _) = []
	| first_n (_, 0) = []
	| first_n (x::xs, n) = x::first_n(xs, n-1);
	
fun pop_back [] = []
	| pop_back [_] = []
	| pop_back (xs) = first_n(xs, length(xs) - 1);


(*	
fun last_n ([], _) = []
	| last_n (_, 0) = []
	| last_n (x::xs, n) = last_n(xs, n-1);
	
	
fun push_back([], x) = [x]
	| 
*)
(*
fun reverse [] = []
	| reverse [m] = [m]
	| reverse 
*)







