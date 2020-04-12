fun is_older (dt1 : int*int*int, dt2 : int*int*int) =
    let
        val y1 = #1 dt1
        val y2 = #1 dt2
        val m1 = #2 dt1
        val m2 = #2 dt2
        val d1 = #3 dt1
        val d2 = #3 dt2
    in
        y1 < y2 orelse (y1=y2 andalso m1 < m2)
        orelse (y1=y2 andalso m1=m2 andalso d1 < d2)
    end

fun number_in_month (dts: (int*int*int) list, month: int) =
    if null dts
    then 0
    else
        (if (#2 (hd dts))=month then 1 else 0) + number_in_month(tl dts, month)

fun number_in_months (dts: (int*int*int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dts, hd months) + number_in_months(dts, tl months)

fun dates_in_month (dts: (int*int*int) list, month: int) =
    if null dts
    then []
    else
        if (#2 (hd dts))=month then hd dts::dates_in_month(tl dts, month) else dates_in_month(tl dts, month)

fun dates_in_months (dts: (int*int*int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dts, hd months) @ dates_in_months(dts, tl months)

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
    if sum <= hd nums
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
    if null dts
    then 0
    else let
        (* whether hd dts in the months *)
        fun in_month_list (ms: int list) =
            not (null ms) andalso ((#2 (hd dts))=(hd ms) orelse in_month_list(tl ms))
    in
        (if in_month_list(months) then 1 else 0) + number_in_months_challenge(tl dts, months)
    end


fun dates_in_months_challenge (dts: (int*int*int) list, months: int list) =
    if null dts
    then []
    else let
        (* whether hd dts in the months *)
        fun in_month_list (ms: int list) =
            not (null ms) andalso ((#2 (hd dts))=(hd ms) orelse in_month_list(tl ms))
        val rest = dates_in_months_challenge(tl dts, months)
    in
        if in_month_list(months) then hd dts::rest else rest
    end

fun reasonable_date (dt: int*int*int) =
    let
        fun get_nth (sl: int list, n: int) =
            if n=1
            then hd sl
            else get_nth(tl sl, n-1)
        val y = #1 dt
        val m = #2 dt
        val d = #3 dt
        val is_leap = (y mod 400 = 0) orelse (y mod 4 = 0 andalso y mod 100 <> 0)
        val feb_len = if is_leap then 29 else 28
        val month_len = [31, feb_len, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        y > 0 andalso m >= 1 andalso m <= 12
        andalso d >= 1 andalso d <= get_nth(month_len, m)
    end
