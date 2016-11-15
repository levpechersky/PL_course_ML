
local
	val pm = (12, 0);
	
	fun add((x1, x2), (y1, y2)) = (x1+y1, x2+y2);

	fun to24h(h, m, ampm) = 
		if ampm="AM" orelse ampm="am" then (h,m) else add((h,m),pm);

	fun less_lexicographic((x1, x2), (y1, y2)) = 
		if x1=y1 then x2<y2 else x1<y1;
in

	(* @param time1, time2 - time in format h,m,am/pm, where:
	 *     h - hour
	 *     m - minute
	 *     am/pm - a string "AM", "am" (and every other is treated as pm)
	 * @return earliest of time1 and time2
	 *
	 * example: earlier((4,20,"PM"),(4,15,"AM")); 
	 *     returns (4,20,"AM") : int * int * string
	 *)
	fun earlier(time1, time2) = 
		if less_lexicographic(to24h(time1), to24h(time2)) then 
			time1 
		else 
			time2;
			
end;











