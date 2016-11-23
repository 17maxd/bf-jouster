(*
    File : evo simple.ml
    Version : 1.0
    Author : max
*)


(** CIBLES **)

let bMickeyV4  = [A 2;M 1;A (-6);M 1;A (-1);M 1;A (-3);M (-3);A (-13);M 1;A (-6);M 1;A (-1);M 1;A (-4);M 1;A (-13);M 2;A (-3);M 1;A (-6);M (-1);A (-16);M (-1);A (-6);M (-1);A (-1);M (-2);A (-2);M (-1); A (-13);M 1;A (-8);M (-1);A (-2);M 1;A (-6);M 1;A (-7);M 1;A (-11);M 1;A (-14);M 1;A (-8);M 1;A (-7);M 1;A (-16);L [M 1;L [A (-2);L [A (-1);L [A 1]]];M 1;L [A (-2);L [A 1]];A (-1)];A (-7);L [M 1;L [A (-2);L [A (-1);L [A 1]]];M 1;L [A (-2);L [A 1]];A (-1)];M (-1);A (-2);M (-1);A (-6);M 1;A (-7);M 1;A (-16);L [M 1;L [A (-2);L [A (-1);L [A 1]]];M 1;L [A (-2);L [A 1]];A (-1)];M (-1);A (-2);M (-1);A (-22);M 1;A (-6);M 1;A (-1);M 1;A (-1);M (-1);A (-5)]
let bDoNothing = [W]

let bot_MickeyV4bis = ">------>->---<<------>->---->------------->>--->------<----------------<------<-<<--<------------->--------<-->------>------->----------->-------------->-------->------->----------------[>[--[-[+]]]>[--[+]]-]-------[>[--[-[+]]]>[--[+]]-]<--<------>------->----------------[>[--[-[+]]]>[--[+]]-]<--<---------------------->------>->-<-----"


(** SÉLÉCTION : GÉNÉTIQUE STANDARD **)

let fitness bot =
    (fight (sob bot) bot_MickeyV4bis 30 Norm) +
    (fight (sob bot) bot_MickeyV4bis 30 Inv)

let rec fitness_list = function
    | [] -> []
    | t::q -> (t, fitness t) :: (fitness_list q)

let comparaison a b = match a, b with (_,x), (_,y) -> x-y

let tri pop =
    let rec aux = function
        | [] -> []
        | (x,_)::q -> x :: (aux q)
    in aux (List.rev (List.stable_sort comparaison (fitness_list pop)))

(* tri [[W] ; [A 1] ; [M (-1)] ; [M (-1) ; W ; W] ; [M 1]] *)


let best_n_of_pop n pop =
    let rec aux n l =
        if n = 0 then []
        else (hd l) :: (aux (n - 1) (tl l))
    in aux n (tri pop)




(** DEBUG **)

let rec somme f = function
    | [] -> 0
    | t::q -> (f t) + (somme f q)

let long bot = String.length (sob bot)

let longmoy pop = (somme long pop) / (List.length pop)

let ecart moy bot = abs ((String.length (sob bot)) - moy)

let ecartype pop = (somme (ecart (longmoy pop)) pop) / (List.length pop)



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
            let enfant_m = mutate (croise (parents @- i) (parents @- j)) 0.1 in
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
        print_string "Generation n°" ;
        print_int (n - i) ;
        print_newline () ;
        print_string "  > rTime : " ;
        print_int (iof (Sys.time () -. t)) ;
        print_string "  - Long moy. :" ;
        print_int (longmoy gen) ;
        print_string "  - E.type² :" ;
        print_int (ecartype gen) ;
        print_newline () ;
        print_string "  > Current Best: " ;
        print_string (sob (List.hd gen)) ;
        print_newline () ;
        ignore (battle_gui (sob (hd gen)) bot_MickeyV4bis 30 Norm 0.) ;
        aux (i-1) (nextgen gen)
    ) in let result = aux n (firstgen 200) in
    print_newline () ;
    print_string (sob result) ;
    print_newline () ;
    battle_gui (sob result) bot_MickeyV4bis 30 Norm 0.01


let brackets bot =
    let bot = sob bot in
    let rec clean i =
        if i = String.length bot then ""
        else match bot.[i] with
            | '[' -> "[" ^ (clean (i+1))
            | ']' -> "]" ^ (clean (i+1))
            | _ -> clean (i+1)
    in clean 0

