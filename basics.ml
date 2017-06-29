(*
    File: basics.ml
    Author: Max D3
*)



(** BASIC FUNCTIONS AND SHORTCUTS **)

let hd = List.hd
let tl = List.tl

let (+=) a b = (a := (!a + b))
let (-=) a b = (a := (!a - b))

let (@-) = List.nth

let (@@) = Array.append

(* tab, string and fast string concatenation *)
let rec ( *@ ) arr n = if n <= 0 then [||] else arr @@ (arr *@ (n-1)) 
let rec ( *^ ) str n = if n <= 0 then  ""  else str ^  (str *^ (n-1)) 
let rec ( **^) str n =
    if n < 1 then ""
    else (str ^ str) **^ (n / 2) ^ if n mod 2 = 0 then "" else str

let foi = float_of_int
let iof = int_of_float

let rand () = Random.float 1.0

let pause secs =
    let t = Sys.time () in
    while Sys.time () < (t +. secs) do
        ()
    done



(** TYPES **)

type polarity = Norm | Inv

type joust_issue = Timeout | Capture | Exit

type winner = Left | Tie | Right

type bot = instr list
and instr = | P | M | L | R | W
            | Lp of instr list

type mutation = Insert | Delete | Permut
