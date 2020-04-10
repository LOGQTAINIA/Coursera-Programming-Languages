
fun is_older (dt1 : int*int*int, dt2 : int*int*int) =
    if (#1 dt1) <> (#1 dt2)
    then (#1 dt1) < (#1 dt2)
    else if (#2 dt1) <> (#2 dt2)
         then (#2 dt1) < (#2 dt2)
         else (#3 dt1) < (#3 dt2)
			
fun number_in_month (dts: (int*int*int) list, month: int) = 
    if null dts
    then 0
    else let
	     val cur = if (#2 (hd dts))=month then 1 else 0
	 in
	     cur + number_in_month(tl dts, month)
	 end
	 
fun number_in_months (dts: (int*int*int) list, months: int list) = 
    if null dts
    then 0
    else let
	     (* whether hd dts in the months *)
	     fun in_month_list (ms: int list) =
		 not (null ms) andalso ((#2 (hd dts))=(hd ms) orelse in_month_list(tl ms))
	     val cur = if in_month_list(months) then 1 else 0
	 in
	     cur + number_in_months(tl dts, months)
	 end

fun dates_in_month (dts: (int*int*int) list, month: int) = 
    if null dts
    then []
    else let
	     val rest = dates_in_month(tl dts, month)
         in
	     if (#2 (hd dts))=month then hd dts::rest else rest
	 end
	 
fun dates_in_months (dts: (int*int*int) list, months: int list) = 
    if null dts
    then []
    else let
	     (* whether hd dts in the months *)
	     fun in_month_list (ms: int list) =
		 not (null ms) andalso ((#2 (hd dts))=(hd ms) orelse in_month_list(tl ms))
	     val rest = dates_in_months(tl dts, months)
	 in
	     if in_month_list(months) then hd dts::rest else rest
	 end

fun get_nth (sl: string list, n: int) =
    if n=1
    then hd sl
    else get_nth(tl sl, n-1)

fun date_to_string (dt: int*int*int) =
    let
	val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months, #2 dt) ^ " " ^ Int.toString(#3 dt) ^ ", " ^ Int.toString(#1 dt)
    end

fun number_before_reaching_sum (sum: int, nums: int list) =
    if sum - (hd nums) <= 0
    then 0
    else 1 + number_before_reaching_sum(sum-(hd nums), tl nums)

fun what_month (day: int) =
    let
	val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	1 + number_before_reaching_sum(day, months)
    end

fun month_range (day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1 + 1, day2)

fun oldest (dts: (int*int*int) list) =
    if null dts
    then NONE
    else let
	fun oldest_nonempty (dts: (int*int*int) list) =
	    if null (tl dts)
	    then hd dts
	    else let val tl_res = oldest_nonempty(tl dts)
		 in
		     if is_older(hd dts, tl_res)
		     then hd dts
		     else tl_res
		 end
    in
	SOME (oldest_nonempty dts)
    end

fun number_in_months_challenge (dts: (int*int*int) list, months: int list) =
    number_in_months(dts, months)

fun dates_in_months_challenge (dts: (int*int*int) list, months: int list) =
    dates_in_months(dts, months)

fun reasonable_date (dt: int*int*int) =
    let
	val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	fun get_nth (sl: int list, n: int) =
	    if n=1
	    then hd sl
	    else get_nth(tl sl, n-1)
	fun is_leap_year (y: int) =
	    y mod 400 = 0 orelse (y mod 4 = 0 andalso y mod 100 <> 0)
	val valid_year = (#1 dt) > 0
	val valid_month = (#2 dt) >= 1 andalso (#2 dt) <= 12
	val days = if not valid_month
		   then 30
		   else let val d = get_nth(months, #2 dt)
			in
			    if is_leap_year(#1 dt) andalso (#2 dt)=2
			    then d + 1
			    else d
			end
	val valid_day = (#3 dt) >= 1 andalso (#3 dt) <= days
    in
	valid_year andalso valid_month andalso valid_day
    end
	

