
(* My own substring with allowed functions and assumption,
   that from and to are valid *)
fun substr(s, from, to) = if from>to then ""
    else str(String.sub(s,from)) ^ substr(s, from+1, to);
