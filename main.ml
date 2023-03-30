open Printf;;

 let concat dest str n = 
  let rec aux buffer counter = 
    if counter = 0
    then buffer
    else aux (buffer ^ str) (counter - 1)
  in aux dest n;;

let to_roman number =
  let map_size = 13 in
      let roman_keys = [1000; 900;500;400;100;90;50;40;10;9;5;4;1] in
      let roman_values = ["M";"CM";"D";"CD";"C";"XC";"L";"XL";"X";"IX";"V";"IV";"I"] in
      let rec iter n roman_numeral counter =
        let key = List.nth roman_keys (counter - 1) in
        let value = List.nth roman_values (counter - 1) in
        let dif = n - (n mod key) in
        let amount = (dif / key) in
        if counter = map_size
        then (concat roman_numeral value amount)
        else iter (n - dif) (concat roman_numeral value amount) (counter + 1)
  in iter number "" 1;;
        
printf "%s\n" (to_roman 2844);;
