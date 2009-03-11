(* PROBLEM --
 * You are given the following information, but you may prefer to do some
 * research for yourself.
 *
 *    * 1 Jan 1900 was a Monday.
 *
 *    * Thirty days has September,
 *      April, June and November.
 *      All the rest have thirty-one,
 *      Saving February alone,
 *      Which has twenty-eight, rain or shine.
 *      And on leap years, twenty-nine.
 *
 *    * A leap year occurs on any year evenly divisible by 4,
 *      but not on a century unless it is divisible by 400.
 *
 * How many Sundays fell on the first of the month during the twentieth
 * century (1 Jan 1901 to 31 Dec 2000)?
 *
 * SOLUTION --
 * Since 1900 is not a leap year, 1 Jan 1901 is a Tuesday and 6 Jan 1901 is the
 * first Sunday of the year.
 *)

type day_of_the_week = Sunday | Monday | Tuesday | Wednesday |
  Thursday | Friday | Saturday
type day = int
type year = int
type month = January | February | March | April | May | June |
  July | August | September | October | November | December
type date = Date of day * month * year

let int_to_month i = match i with
  | 1  -> January | 2  -> February | 3  -> March | 4  -> April | 5  -> May
  | 6  -> June | 7  -> July | 8  -> August | 9  -> September | 10 -> October
  | 11 -> November | 12 -> December
  | _ -> failwith "Invalid numerical month value"

let month_of_int = int_to_month

let month_to_int m = match m with
  | January -> 1 | February -> 2 | March -> 3 | April -> 4 | May -> 5
  | June -> 6 | July -> 7 | August -> 8 | September -> 9 | October -> 10
  | November -> 11 | December -> 12

let int_of_month = month_to_int

(* OrderedType neccessities *)
type t = date

let compare (Date(fd,fm,fy)) (Date(sd,sm,sy)) =
  if fy - sy <> 0 then fy - sy
  else if (int_of_month fm) - (int_of_month sm) <> 0 then
    (int_of_month fm) - (int_of_month sm)
  else fd - sd
(* End OrderedType neccessities *)

let succ_month m = 
  match m with 
  | December -> January
  | _ -> month_of_int ((int_of_month m) + 1)

let base_number_of_days m =
  match m with
  | September -> 30 | April -> 30 | June -> 30
  | November -> 30 | February -> 28 | _ -> 31

let is_leap_year year = 
  let mod4 = year mod 4 in
  let mod100 = year mod 100 in
  let mod400 = year mod 400 in
  (mod4 = 0) && (mod100 <> 0 || mod400 = 0)

let number_of_days m y =
  match m with
  | February when is_leap_year y -> 29
  | _ -> base_number_of_days m

let rec coerce (Date(d,m,y)) =
  let max_d = number_of_days m y in
  if d <= max_d then Date(d,m,y)
  else if m = December then coerce(Date(d-max_d, January, y+1))
  else coerce(Date(d-max_d, succ_month m, y))

let to_date d m y = coerce(Date(d,month_of_int m,y))

let is_first_of_month (Date(d,m,y)) = d = 1

let succ_n n (Date(d,m,y)) = coerce(Date(d+n,m,y))

let add_week = succ_n 7

let seq fdate ldate n =
  let next = succ_n n in
  let rec helper curr dates =
    if compare curr ldate > 0 then List.rev dates
    else helper (next curr) (curr::dates)
  in
  helper fdate []

let count_fm_sundays =
  let first_sunday = to_date 6 1 1901 in
  let last_date = to_date 31 12 2000 in
  let rec helper curr_date count =
    let is_fm = is_first_of_month curr_date in
    if compare curr_date last_date > 0 then count
    else helper (add_week curr_date) (if is_fm then count+1 else count)
  in
  helper first_sunday 0

let solution = count_fm_sundays
