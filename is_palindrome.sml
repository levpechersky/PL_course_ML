local
	fun head s = String.sub(s, 0);
	fun tail s = String.substring(s, 1, size s - 1);

    fun reverse "" = ""
      | reverse s = (reverse (tail s)) ^ (str(head s));

    fun apply _ "" = ""
      | apply f s = (str (f (head s))) ^ (apply f (tail s));

    fun lower_c c = if #"A" <= c andalso c <= #"Z" then chr (ord c + 0x20) else c;

    val lower = apply lower_c;
in
    fun is_palindrome s = 
	    let
			val lower_s = lower s
		in
			reverse lower_s = lower_s
		end;
end; 
   
   
   











