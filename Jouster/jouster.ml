(*
    File : jouster.ml
    Version : 3.0
    Author : Max D3
*)


(** TYPAGE **)

type polarite = Norm | Inv

type fin_combat = Timeout | Capture | Sortie

type gagnant = Gauche | Nul | Droite



(** FONCTIONS STANDARD **)

let hd l = List.hd l
let tl l = List.tl l

let (+=) a b = (a := (!a + b))
let (-=) a b = (a := (!a - b))

let (@@) array1 array2 = Array.append array1 array2

let rec ( *@ ) arr n = if n <= 0 then [||] else arr @@ (arr *@ (n-1)) 
let rec ( *^ ) str n = if n <= 0 then ""   else str ^  (str *^ (n-1))

let foi i = float_of_int i
let iof f = int_of_float f



(* COMBAT DE BOTS! *)

(** saute jusqu'au crochet fermant la position actuelle pos,
    la variable prf permet de ne pas prendre en compte les sous-boucles *)
let rec jump str_bot i prf = match str_bot.[i] with
    | '[' -> jump str_bot (i+1) (prf+1)
    | ']' -> if prf = 1 then i else jump str_bot (i+1) (prf-1)
    |  _  -> jump str_bot (i+1) prf


(** renvoie une copie inversée du bot, les string sont mutables !*)
let rev_bot str_bot =
    let copy = Bytes.copy str_bot in
    let len = String.length(copy) in
    for i = 0 to len - 1 do
        match copy.[i] with
            |'>' -> Bytes.set copy i '<'
            |'<' -> Bytes.set copy i '>'
            | _  -> ()
    done ; copy


(** renvoie une copie polarité inversée du bot *)
let rev_pol_bot str_bot =
    let copy = Bytes.copy str_bot in
    let len = String.length(copy) in
    for i = 0 to len - 1 do
        match copy.[i] with
            |'+' -> Bytes.set copy i '-'
            |'-' -> Bytes.set copy i '+'
            | _  -> ()
    done ; copy

(** fonction de calcul du combat entre deux bots
    le bot droit est inversé et, si nécessaire, polairement inversé
    CONDITIONS D'ARRÊT: - Un drapeau est nul deux tours de suite (Capture)
                        - Un bot sort de la zone de jeu (Sortie) 
                        - Plus de 2000 cycles ont eu lieu sans victoire directe
                    *)
let battle bot1 bot2 taille pol =
    let bot2 = rev_bot (if pol = Inv then rev_pol_bot bot2 else bot2) in
    let mem = [|128|] @@ ([|0|] *@ (taille - 2)) @@ [|128|] in
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
        let delta = (abs mem.(0) - abs mem.(taille - 1)) in
        if cycle > 2000 then
            let gagnant = if delta > 0 then Gauche else
                          if delta < 0 then Droite else Nul in
            (gagnant, Timeout, delta, cycle)
        else
            let (p1, i1, l1) = exec bot1 p1 (if i1 >= len1 then 0 else i1) l1 in
            let (p2, i2, l2) = exec bot2 p2 (if i2 >= len2 then 0 else i2) l2 in
            let zz1, zz2 = (mem.(0) = 0), (mem.(taille - 1) = 0) in
            if (zz1 && z1) && (zz2 && z2) then (Nul, Capture, 0, cycle) else
            if (abs (p1 - p2)) = (taille + 1) then (Nul, Sortie, 0, cycle) else
            if (zz1 && z1) then (Droite, Capture, delta, cycle) else
            if (zz2 && z2) then (Gauche, Capture, delta, cycle) else
            if (p1 < 0) || (p1 = taille) then (Droite, Sortie, delta, cycle) else
            if (p2 < 0) || (p2 = taille) then (Gauche, Sortie, delta, cycle) else
            fight (cycle + 1) p1 p2 i1 i2 l1 l2 (mem.(0) = 0) (mem.(taille-1) = 0)
    in fight 0 0 (taille -1) 0 0 [] [] false false



(** EVALUATIONS GROUPÉES **)

let score_battle bot1 bot2 n pol = match battle bot1 bot2 n pol with
    | (Gauche,_,_,_) -> 1
    | (Droite,_,_,_) -> -1
    | (Nul,_,_,_) -> 0


(** joue tous les combats entre deux bots *)
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
    print_newline () ;
    print_string "Score : " ;
    print_int !pts ;
    print_newline ()


let ( *> ) bot1 bot2 =
    let pts = ref 0 in
    for i = 10 to 30 do
        pts += (score_battle bot1 bot2 i Norm) ;
        pts += (score_battle bot1 bot2 i Inv) 
    done ; !pts



(** SPEEDTEST **)

(** Speedtest sur 420 combats de tous types *)
let speedtest bot1 bot2 =
    Printf.printf "\nBenchmark..." ;
    print_newline() ;
    let t = Sys.time() in
    for i = 1 to 20 do
        ignore (bot1 *> bot2)
    done ;
    let cps = iof (foi 840 /. (Sys.time () -. t)) in
    Printf.printf "%d combats par seconde !\n" cps


let runtime time f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx
