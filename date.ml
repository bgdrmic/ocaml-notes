type date = { month:int; day:int }
let make_date month day = {month; day}
let get_month d = d.month
let get_day d = d.day
let to_string d = (string_of_int d.day) ^ "/" ^ (string_of_int d.month)
let format formatter d = Format.fprintf formatter "%d/%d" d.day d.month