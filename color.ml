type t = int 
let none     = 0
let white    = 1 
let black    = 2
let sentinel = 3 

let opposite c = (2-c) + 1

let string c = 
  if c = white then "White"
  else              "Black"


let print c = 
  if      c = white then print_string "O" 
  else if c = black then print_string "X"
  else if c = none  then print_string " " 
  else ()
