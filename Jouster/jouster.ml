(*
    File : jouster.ml
    Version : 3.0
    Author : max
*)



(** TYPAGE **)

type polarite = Norm | Inv

type fin_combat = Timeout | Capture | Sortie

type gagnant = Gauche | Nul | Droite

(* delta * nb_cycles *)
type resultat = gagnant * fin_combat * int * int


(** FONCTIONS PRATIQUES **)

let hd l = List.hd l
let tl l = List.tl l

let (+=) a b = (a := (!a + b))
let (-=) a b = (a := (!a - b))

let (@@) array1 array2 = Array.append array1 array2

let rec ( *@ ) arr n = if n <= 0 then [||] else arr @@ (arr *@ (n-1)) 
let rec ( *^ ) str n = if n <= 0 then ""   else str  ^ (str *^ (n-1))

let foi i = float_of_int i
let iof f = int_of_float f



(* COMBAT DE BOTS! *)

let rec jump bot i c = match bot.[i] with
    | '[' -> jump bot (i+1) (c+1)
    | ']' -> if c = 1 then i else jump bot (i+1) (c-1)
    |  _  -> jump bot (i+1) c


let reverse_bot str_bot =
    let copy = Bytes.copy str_bot in
    let len = String.length(copy) in
    for i = 0 to len - 1 do
        match copy.[i] with
            |'>' -> Bytes.set copy i '<'
            |'<' -> Bytes.set copy i '>'
            | _  -> ()
    done ; copy


let reverse_pol_bot str_bot =
    let copy = Bytes.copy str_bot in
    let len = String.length(copy) in
    for i = 0 to len - 1 do
        match copy.[i] with
            |'+' -> Bytes.set copy i '-'
            |'-' -> Bytes.set copy i '+'
            | _  -> ()
    done ; copy


let battle bot1 bot2 size pol =
    let bot2 = reverse_bot (if pol = Inv then reverse_pol_bot bot2 else bot2) in
    let mem = [|128|] @@ ([|0|] *@ (size - 2)) @@ [|128|] in
    let len1, len2 = String.length bot1, String.length bot2 in
    let exec bot p i l = match bot.[i] with
        | '>' -> (p + 1, i + 1, l)
        | '<' -> (p - 1, i + 1, l)
        | '+' -> (if mem.(p) = 128 then mem.(p) <- (-127)
                  else mem.(p) <- mem.(p) + 1 ; (p, i+1, l))
        | '-' -> (if mem.(p) = (- 127) then mem.(p) <- 128
                  else mem.(p) <- mem.(p) - 1 ; (p, i + 1, l))
        | '.' -> (p, i + 1, l)
        | '[' -> if mem.(p) = 0 then (p, 1 + (jump bot i 0), l)
                 else (p, i + 1, (i+1)::l)
        | ']' -> if mem.(p) = 0 then (p, i + 1, (tl l))
                 else (p, (hd l), l)
        |  _  -> (p, i + 1, l)
    in let rec fight cycle p1 p2 i1 i2 l1 l2 z1 z2 =
        let delta = (abs mem.(0) - abs mem.(size - 1)) in
        if cycle > 1000 then
            let gagnant = if delta > 0 then Gauche else
                          if delta < 0 then Droite else Nul in
            (gagnant, Timeout, delta, cycle)
        else
            let (p1, i1, l1) = exec bot1 p1 (if i1 >= len1 then 0 else i1) l1 in
            let (p2, i2, l2) = exec bot2 p2 (if i2 >= len2 then 0 else i2) l2 in
        let zz1, zz2 = (mem.(0) = 0), (mem.(size - 1) = 0) in
        if (zz1 && z1) || (zz2 && z2) || p1 < 0 || p2 < 0 || p1 = size || p2 = size then
            begin
                if (zz1 && z1) && (zz2 && z2) then (Nul, Capture, 0, cycle) else
                if (abs (p1 - p2)) = (size + 1) then (Nul, Sortie, 0, cycle) else
                if (zz1 && z1) then (Droite, Capture, delta, cycle) else
                if (zz2 && z2) then (Gauche, Capture, delta, cycle) else
                if (p1 < 0) || (p1 = size) then (Droite, Sortie, delta, cycle) else
                if (p2 < 0) || (p2 = size) then (Gauche, Sortie, delta, cycle) else
                (Nul, Timeout, 4789127, 00984120)
            end
        else fight (cycle + 1) p1 p2 i1 i2 l1 l2 (mem.(0) = 0) (mem.(size-1) = 0)
    in fight 0 0 (size -1) 0 0 [] [] false false


(** EVALUATIONS MULTIPLES **)

let score_battle bot1 bot2 n pol = match battle bot1 bot2 n pol with
    | (Gauche,_,_,_) -> 1
    | (Droite,_,_,_) -> -1
    | (Nul,_,_,_) -> 0


let ( *>> ) bot1 bot2 =
    let pts = ref 0 in
    print_string "NORMAL :" ;
    for i = 10 to 30 do
        let s = (score_battle bot1 bot2 i Norm) in
        pts += s ;
        print_string (" " ^ if s < 0 then "" else " ") ;
        print_int s
    done ;
    print_string "\nINVERSE:" ;
    for i = 10 to 30 do
        let s = (score_battle bot1 bot2 i Inv) in
        pts += s ;
        print_string (" " ^ if s < 0 then "" else " ") ;
        print_int s
    done ;
    print_string "\n" ;
    !pts


let ( *> ) bot1 bot2 =
    let pts = ref 0 in
    for i = 10 to 30 do
        pts += (score_battle bot1 bot2 i Norm) ;
        pts += (score_battle bot1 bot2 i Inv) 
    done ; !pts



(** SPEED TESTS **)

(* 584 *) let bot_MickeyV4              = "++>------>->---<<<------------->------>->---->------------->>--->------<----------------<------<-<<--<------------->--------<-->------>------->----------->-------------->-------->------->----------------[>[--[-[+]]]>[--[+]]-]-------[>[--[-[+]]]>[--[+]]-]<--<------>------->----------------[>[--[-[+]]]>[--[+]]-]<--<---------------------->------>->-<-----"
(* 512 *) let bot_CounterPunch          = "++++++>------------>>>>>>><------------<++++++++++++<------------<++++++++++++<------------<++++++++++++>>>>>>>" ^ ("[-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]]>" *^ 21)
(* 493 *) let bot_Bigger                = "+>->+>+>->------------------>++++++++++++++++++>------------------>++++++++++++++++++" ^ (">[++++++++++++++++++[-][-[+]]][++++++++++++++++++[-][-[+]]]" *^ 21)


(** Speedtest sur 420 combats de tous types *)
let speedtest bot1 bot2 =
    let t = Sys.time() in
    for i = 1 to 10 do
        ignore (bot1 *> bot2)
    done ;
    let cps = iof (foi 420 /. (Sys.time () -. t)) in
    Printf.printf "%d combats par seconde !\n" cps


let runtime time f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx
