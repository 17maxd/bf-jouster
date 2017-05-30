



(* TYPAGE *)

type bot = instr list
and instr = | P | M | L | R | W
            | Lp of instr list

type polarite = Norm | Inv

type mutation = Insert | Delete | Permut

type fin_combat = Timeout | Capture | Sortie

type gagnant = Gauche | Nul | Droite

type resultat = gagnant * fin_combat * int * int



(* VARIABLES GLOBALES *)

let instructions = [|P ; M ; P ; M ; L ; R ; W ; Lp[]|]
let instructions_std = [|P ; M ; P ; M ; L ; R ; W|]



(* FONCTIONS PRATIQUES *)

let (+=) a b = (a := (!a + b))
let (-=) a b = (a := (!a - b))

let rand () = Random.float 1.0

let (@-) l n = List.nth l n

let rec ( **^) str n =
    if n < 1 then ""
    else (str ^ str) **^ (n / 2) ^ if n mod 2 = 0 then "" else str



(* CONVERSIONS *)

let rec string_of_bot = function
    | [] -> ""
    | P :: q -> "+" ^ string_of_bot q
    | M :: q -> "-" ^ string_of_bot q
    | L :: q -> "<" ^ string_of_bot q
    | R :: q -> ">" ^ string_of_bot q
    | W :: q -> "." ^ string_of_bot q
    | Lp bot :: q -> "[" ^ (string_of_bot bot) ^ "]" ^ string_of_bot q


let sob bot = string_of_bot bot



(* PARCOURS *)

let rec parcours = function
    | [] -> []
    | P :: q -> P :: parcours q
    | M :: q -> M :: parcours q
    | L :: q -> L :: parcours q
    | R :: q -> R :: parcours q
    | W :: q -> W :: parcours q
    | Lp bot :: q -> parcours bot @ parcours q


let longueur bot =
    List.length (parcours bot)



(* GÉNÉRATION ALÉATOIRE *)

let instr_alea () = instructions.(Random.int 8)
let instr_alea_std () = instructions_std.(Random.int 7)

let rec bot_alea s d =
    let rec aux i =
        if i = 0 then [] else
        match instr_alea () with
            | Lp _ -> if d < 0 then aux i else
                      Lp (bot_alea (s/2) (d-1)) :: (aux (i-1))
            | x -> x :: (aux (i-1))
    in aux (1 + Random.int s)


let rec pop_alea size (s, d) = match size with
    | 0 -> []
    | n -> (bot_alea s d) :: (pop_alea (size - 1) (s, d))



(* MUTATIONS *)

let mutation_alea () = [|Insert ; Delete ; Permut|].(Random.int 3)

let rec mutate bot taux =
    let rec aux = function
        | Lp l :: q -> Lp (mutate l taux) :: (aux q)
        | t :: Lp l :: q -> t :: Lp (mutate l taux) :: (aux q)
        | t :: t' :: q -> (
            if rand () > taux
            then t::(aux (t'::q)) else
            match mutation_alea () with
                | Insert -> t :: (instr_alea_std ()) :: (aux (t'::q))
                | Delete -> aux (t'::q)
                | Permut -> t'::t::(aux q)
            )
        | x -> x
    in aux bot



(* CROISEMENTS *)
(* À reprendre, avec une vision textuelle du croisement !! *)


let rec n_derniers n liste =
    if n >= List.length liste then liste else
    match liste with
        | t::q -> n_derniers n q
        | _ -> []

let n_premiers n liste = List.rev (n_derniers n (List.rev liste))

let croise bot1 bot2 =
    let pos1 = Random.int (List.length bot1) in
    let pos2 = max ((List.length bot2) - pos1) 0 in
    if Random.bool () then
         (n_premiers pos1 bot1) @ (n_derniers pos2 bot2)
    else (n_premiers pos2 bot2) @ (n_derniers pos1 bot1) 



(* FITNESS *)

let bot_MickeyV4_m     = ">------>->---<<------>->---->------------->>--->------<----------------<------<-<<--<------------->--------<-->------>------->----------->-------------->-------->------->----------------[>[--[-[+]]]>[--[+]]-]-------[>[--[-[+]]]>[--[+]]-]<--<------>------->----------------[>[--[-[+]]]>[--[+]]-]<--<---------------------->------>->-<-----"
let bot_CounterPunch_m = ">------------>>>>>>><------------<++++++++++++<------------<++++++++++++<------------<++++++++++++>>>>>>>" ^ ("[-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]]>" **^ 21)
let bot_Bigger_m       = ">->+>+>->------------------>++++++++++++++++++>------------------>++++++++++++++++++" ^ (">[++++++++++++++++++[-][-[+]]][++++++++++++++++++[-][-[+]]]" **^ 21)


let score bot1 bot2 taille pol =
    match battle bot1 bot2 taille pol with
    | (Nul, _, _, _) -> 0
    | (Gauche, Timeout, _, _) -> 1
    | (Droite, Timeout, _, _) -> -1
    | (Gauche, Capture, d, n) -> d/10 + 8 - n/200
    | (Droite, Capture, d, n) -> d/10 - 8 + n/200
    | (Gauche, Sortie,  _, _) -> 20
    | (Droite, Sortie,  _, _) -> -20


let bot_objectif = bot_CounterPunch_m

let fitness_s bot =
   ((score (sob bot) bot_objectif 11 Norm) +
    (score (sob bot) bot_objectif 21 Norm) +
    (score (sob bot) bot_objectif 24 Norm) +
    (score (sob bot) bot_objectif 29 Norm) +
    (score (sob bot) bot_objectif 30 Norm) +
    (score (sob bot) bot_objectif 13 Inv ) +
    (score (sob bot) bot_objectif 17 Inv ) +
    (score (sob bot) bot_objectif 20 Inv ) +
    (score (sob bot) bot_objectif 25 Inv ) +
    (score (sob bot) bot_objectif 29 Inv )) / 10



(* let fitness_s bot = score (sob bot) bot_MickeyV4_m 30 Norm) *)


(* ALGORITHME GÉNÉTIQUE SIMPLE *)


let rec fitness_pop_s = function
    | [] -> []
    | t::q -> (t, fitness_s t) :: (fitness_pop_s q)


(* à score égal le bot le plus court l'emporte *)
let comparaison_s a b = match a, b with (b1,x), (b2,y) -> y - x


let tri_s pop =
    let rec aux = function
        | [] -> []
        | (x,_)::q -> x :: (aux q)
    in aux (List.sort comparaison_s (fitness_pop_s pop))

(* tri [[W] ; [P] ; [R;W] ; [L] ; [R;W;W] ; [R]] *)


let best_n_of_pop_s n pop =
    let rec aux n l =
        if n = 0 then []
        else (hd l) :: (aux (n - 1) (tl l))
    in aux n (tri_s pop)



(* ÉVOLUTION *)

(*  80 individus :
    --------------
    - 11 parents
    - 55 enfants
    - 14 random    *)

let rec nextgen_s parents taux_m = 
    let rec aux i j =
        if j = 11 then [] else
        if i = 11 then aux (j+1) (j+1) else
        begin
            let enfant = mutate (croise (parents @- i) (parents @- j)) taux_m in
            if enfant = [] then [L] :: (aux (i+1) j)
            else enfant :: (aux (i+1) j)
        end
    in (aux 0 0) @ (pop_alea 20 (20,3))


let evolution_s nb_gen taux_m =
    print_int (Random.int 10000) ;
    print_newline () ;
    let rec aux i gen =
        if i <= 0 then hd (tri_s gen)
        else begin
            print_string "Generation n°" ;
            print_int (nb_gen - i) ;
            print_string " (f:" ;
            let parents = best_n_of_pop_s 11 gen in
            let best = hd parents in
            print_int (fitness_s best) ;
            print_string ") : " ;
            print_string (sob best) ;
            print_newline () ;
            aux (i-1) (nextgen_s parents taux_m)
        end
    in let result = sob (aux nb_gen (pop_alea 100 (25, 3))) in
        print_newline () ;
        print_string ("Résultat :  " ^ result) ;
        print_string "\n\nBot objectif :\n----------\n" ;
        print_int (result *>> bot_objectif)


let evolution_s_bis nb_gen taux_m =
    print_int (Random.int 10000) ;
    print_newline () ;
    let rec aux i gen =
        if i <= 0 then hd (tri_s gen)
        else begin
            if i mod 20 = 0 then begin
            print_string "Generation n°" ;
            print_int (nb_gen - i) ;
            print_string " (f:" end ;
            let parents = best_n_of_pop_s 11 gen in
            if i mod 20 = 0 then begin
            let best = hd parents in
            print_int (fitness_s best) ;
            print_string ") : " ;
            print_string (sob best) ;
            print_newline () end ;
            aux (i-1) (nextgen_s parents taux_m)
        end
    in let result = sob (aux nb_gen (pop_alea 100 (25, 3))) in
        print_newline () ;
        print_string ("Résultat :  " ^ result) ;
        print_string "\n\nBot objectif :\n----------\n" ;
        print_int (result *>> bot_objectif)


(* DEBUG *)

let evolve () =
    for i = 0 to 20 do
        evolution_s_bis 200 0.2 ;
    done
