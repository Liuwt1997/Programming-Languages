fun is_older (date1: int*int*int, date2: int*int*int) =
  (#1 date1 < #1 date2)
  orelse (#1 date1 = #1 date2 andalso #2 date1 < #2 date2)
  orelse (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)
  
fun number_in_month (dates: (int*int*int) list, month: int) =
  if null dates then 0
  else
      let val rest_dates = number_in_month((tl dates), month)
      in
	  if #2 (hd dates) = month
	  then 1 + rest_dates
	  else rest_dates		   
      end	  
	   
fun number_in_months (dates: (int*int*int) list, months: int list) = 
  if null dates then 0
  else
      let val rest_dates = number_in_months(tl dates, months)
      in
	  let fun same_months (dates_month: int, month: int list) =
		if null month then 0
		else
		    if dates_month = hd month then 1
		    else same_months (dates_month, tl month)
	  in
	      rest_dates + same_months (#2 (hd dates), months)
	  end	      
      end

fun dates_in_month (dates: (int*int*int) list, month: int) =
  if null dates then []
  else
      let val rest_dates = dates_in_month((tl dates), month)
      in
	  if #2 (hd dates) = month
	  then (hd dates) :: rest_dates
	  else rest_dates
      end

fun dates_in_months (dates: (int*int*int) list, months: int list) =
  if null dates then []
  else
      dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))

fun get_nth (a: string list, n: int) =
  if n=1 then hd a
  else get_nth((tl a), n-1)

fun date_to_string (date: int*int*int) =
  let
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end
							  
fun number_before_reaching_sum (sum: int, numbers: int list) =
  if hd numbers >= sum then 0
  else 1 + number_before_reaching_sum(sum - (hd numbers), (tl numbers))

fun what_month (the_day: int) =
  let
      val days=[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      1 + number_before_reaching_sum(the_day, days)
  end

fun month_range (day1: int, day2: int) =
  if day1>day2 then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (dates: (int*int*int) list) =
  if null dates then NONE
  else
      let val rest_oldest = oldest(tl dates)
      in
	  if isSome rest_oldest andalso is_older((valOf rest_oldest), (hd dates))
	  then rest_oldest
	  else SOME (hd dates)
      end
	  
