(*
    File : evo koth.ml
    Version : 1.0
    Author : max
*)


(** SÉLÉCTION : KING OF THE HILL **)

let totalfight bot1 bot2 =
    if bot1 = [] then -1000 else
    if bot2 = [] then 1000 else
    let sbot1, sbot2 = sob bot1, sob bot2 in
    (fight sbot1 sbot2 30 Norm)


let tri_comparatif pop = List.rev (List.stable_sort totalfight pop)

(* tri_comparatif [[W] ; [A 1] ; [M (-1)] ; [M (-1) ; W ; W] ; [M 1]] *)


let best_n_of_pop n pop =
    let rec aux n l = if n = 0 then []
    else (hd l) :: (aux (n - 1) (tl l))
in aux n (tri_comparatif pop)


(* 1 population = 100 individus *)
(* 13*2 parents *)
(* 78*2 enfants *)
(* 18 random *)


let rec nextgen gen = 
    let parents = best_n_of_pop 13 gen in
    let rec aux i j =
        if j = 13 then [] else
        if i = 13 then aux (j+1) (j+1) else begin
            let enfant = croise (parents @- i) (parents @- j) in
            let enfant_m = mutate (croise (parents @- i) (parents @- j)) 0.01 in
            if (enfant, enfant_m) = ([], []) then [M (-1)] :: [M (-1)] :: (aux (i+1) j)
            else if enfant = [] then [M (-1)] :: enfant_m :: (aux (i+1) j)
            else if enfant_m = [] then [M (-1)] :: enfant :: (aux (i+1) j)
            else enfant :: enfant_m :: (aux (i+1) j)
        end
    in (aux 0 0) @ (firstgen 18)


let evolution n =
    let t = Sys.time() in
    let rec aux i gen = if i = 0 then hd gen
    else (
        battle_gui (sob (hd gen)) "." 30 Norm 0. ;
        print_float (Sys.time () -. t) ;
        print_string "  - Generation n°" ;
        print_int (n - i) ;
        print_string ("  " ^ (sob (List.hd gen))) ;
        print_newline () ;
        aux (i-1) (nextgen gen)
    ) in let result = aux n (firstgen 200) in
    print_newline () ;
    print_string (sob result) ;
    print_newline () ;
    battle_gui (sob result) "." 30 Norm 0.001
