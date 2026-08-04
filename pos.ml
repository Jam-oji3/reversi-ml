type t = int * int 

let string (i, j) = 
  let ci = char_of_int (i + int_of_char 'A' - 1) in
  let cj = char_of_int (j + int_of_char '1' - 1) in
  let s  = Bytes.make 2 ' ' in
  let _  = ( Bytes.set s 0 ci; Bytes.set s 1 cj) in
  Bytes.to_string s

