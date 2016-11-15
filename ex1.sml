(* Lev Pechersky 333815546 levpechersky@campus.technion.ac.il Kfir Taizi 208044610 kfirtaizi@campus.technion.ac.il *)

fun posInt2Str 0 = ""
    | posInt2Str x = (posInt2Str(x div 10)) ^ str(chr(x mod 10 + ord(#"0")));

fun int2string x = if x<0 then
    "-" ^ posInt2Str(~x)
  else
    (if x=0 then "0" else posInt2Str(x));


fun isLegalStringAux (f, s, i) = if i>=size(s) then
   true
 else
   (f(String.sub(s, i)) andalso isLegalStringAux(f, s, i+1));

fun isLegalString (f, s) = isLegalStringAux(f, s, 0);


fun reverseStringAux (s, index) = if index >= size(s) then
    ""
  else
    reverseStringAux(s, index+1) ^ str(String.sub(s, index));

fun reverseString s = reverseStringAux(s, 0);
