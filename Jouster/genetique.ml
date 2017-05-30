(*
    File : genetique.ml
    Version : 2.0
    Author : Max D3
*)


(* TYPAGE *)

type bot = instr list
and instr = | P | M | L | R | W
            | Lp of instr list

type polarite = Norm | Inv

type mutation = Insert | Delete | Permut

type fin_combat = Timeout | Capture | Sortie

type gagnant = Gauche | Nul | Droite



(* FONCTIONS STANDARD *)

let (+=) a b = (a := (!a + b))
let (-=) a b = (a := (!a - b))

let rand () = Random.float 1.0

let (@-) l n = List.nth l n

let rec ( **^) str n =
    if n < 1 then ""
    else (str ^ str) **^ (n / 2) ^ if n mod 2 = 0 then "" else str

let p_i i = print_int i
let p_s s = print_string s
let p_n () = print_newline ()


(* MANIPULATIONS DE BOTS *)

let rec string_of_bot = function
    | [] -> ""
    | P :: q -> "+" ^ string_of_bot q
    | M :: q -> "-" ^ string_of_bot q
    | L :: q -> "<" ^ string_of_bot q
    | R :: q -> ">" ^ string_of_bot q
    | W :: q -> "." ^ string_of_bot q
    | Lp bot :: q -> "[" ^ (string_of_bot bot) ^ "]" ^ string_of_bot q

let sob bot = string_of_bot bot


let rec longueur bot =
    let rec aux = function
        | [] -> 0
        | P :: q -> 1 + aux q
        | M :: q -> 1 + aux q
        | L :: q -> 1 + aux q
        | R :: q -> 1 + aux q
        | W :: q -> 1 + aux q
        | Lp bot :: q -> 2 + (longueur bot) + (aux q)
    in aux bot



(* 1. FITNESS *)

let bot_MickeyV4_m     = ">------>->---<<------>->---->------------->>--->------<----------------<------<-<<--<------------->--------<-->------>------->----------->-------------->-------->------->----------------[>[--[-[+]]]>[--[+]]-]-------[>[--[-[+]]]>[--[+]]-]<--<------>------->----------------[>[--[-[+]]]>[--[+]]-]<--<---------------------->------>->-<-----"
let bot_CounterPunch_m = ">------------>>>>>>><------------<++++++++++++<------------<++++++++++++<------------<++++++++++++>>>>>>>" ^ ("[-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]]>" **^ 21)
let bot_Bigger_m       = ">->+>+>->------------------>++++++++++++++++++>------------------>++++++++++++++++++" ^ (">[++++++++++++++++++[-][-[+]]][++++++++++++++++++[-][-[+]]]" **^ 21)

let bot_objectif = bot_Bigger_m


let score bot1 bot2 taille pol =
    match battle bot1 bot2 taille pol with
    | (Nul, _, _, _) -> 0
    | (Gauche, Timeout, _, _) -> 1
    | (Droite, Timeout, _, _) -> -1
    | (Gauche, Capture, d, n) -> d/10 + 8 - n/200
    | (Droite, Capture, d, n) -> d/10 - 8 + n/200
    | (Gauche, Sortie,  _, _) -> 20
    | (Droite, Sortie,  _, _) -> -20


let fitness bot =
    let bot_s = sob bot in
   ((score bot_s bot_objectif 11 Norm) +
    (score bot_s bot_objectif 21 Norm) +
    (score bot_s bot_objectif 24 Norm) +
    (score bot_s bot_objectif 29 Norm) +
    (score bot_s bot_objectif 30 Norm) +
    (score bot_s bot_objectif 13 Inv ) +
    (score bot_s bot_objectif 17 Inv ) +
    (score bot_s bot_objectif 20 Inv ) +
    (score bot_s bot_objectif 25 Inv ) +
    (score bot_s bot_objectif 29 Inv )) / 10



(* 2. GÉNÉRATION ALÉATOIRE *)

let instr_tab = [|P ; M ; P ; M ; L ; R ; W ; Lp[]|]
let instr_std_tab = [|P ; M ; P ; M ; L ; R ; W|]


let instr_alea () = instr_tab.(Random.int 8)
let instr_std_alea () = instr_std_tab.(Random.int 7)


(** génère un bot aléatoire de taille = ±len et de profondeur max p_max *)
let rec bot_alea len p_max =
    let rec aux i =
        if i = 0 then [] else
        match instr_alea () with
            | Lp _ -> if p_max < 0 then aux i else
                      Lp (bot_alea (len/2) (p_max-1)) :: (aux (i-1))
            | x -> x :: (aux (i-1))
    in aux (1 + Random.int len)


(** un individu est un tuple (fit, bot) *)
let rec ind_alea len p_max =
    let bot = bot_alea len p_max in (fitness bot, bot)


let rec pop_alea taille = match taille with
    | 0 -> []
    | n -> (ind_alea 25 3) :: (pop_alea (taille - 1))



(* 3. MUTATIONS *)

let mutation_alea () = [|Insert ; Delete ; Permut|].(Random.int 3)


let rec muter_bot bot taux =
    let rec aux = function
        | Lp l :: q -> Lp (muter_bot l taux) :: (aux q)
        | t :: Lp l :: q -> t :: Lp (muter_bot l taux) :: (aux q)
        | t :: t' :: q -> (
            if rand () > taux then t::(aux (t'::q))
            else match mutation_alea () with
                    | Insert -> t :: (instr_std_alea ()) :: (aux (t'::q))
                    | Delete -> aux (t'::q)
                    | Permut -> t'::t::(aux q)
            )
        | x -> x
    in aux bot


let muter ind taux =
    let mutant = muter_bot (snd ind) taux in
    (fitness mutant, mutant)


let rec muter_pop pop taux = match pop with
    | t::q -> (muter t taux) :: (muter_pop q taux)
    | [] -> []



(* 4. CROISEMENTS *)

let rec n_derniers n liste =
    if n >= List.length liste then liste else
    match liste with
        | t::q -> n_derniers n q
        | _ -> []

let rec n_premiers n liste = match (n, liste) with
    | 0, _ -> []
    | _, t::q -> t :: (n_premiers (n-1) q)
    | _, [] -> failwith "liste trop courte"


(** croisements entre deux bots, avec un enfant de la longueur de bot2 *)
let croise_bot bot1 bot2 =
    let pos1 = Random.int (List.length bot1) in
    let pos2 = max ((List.length bot2) - pos1) 0 in
    if Random.bool () then
         (n_premiers pos1 bot1) @ (n_derniers pos2 bot2)
    else (n_premiers pos2 bot2) @ (n_derniers pos1 bot1) 


let croise ind1 ind2 =
    let enfant = croise_bot (snd ind1) (snd ind2) in
    (fitness enfant, enfant)



(* 5. ALGORITHME GÉNÉTIQUE STANDARD *)

(** comparaison : d'abord la fitness puis la longueur.
    on evite bien les trios se battant tous mutuellement *)

let compare ind1 ind2 =
    if ind1 = ind2 then 0 else
        if (fst ind2) = (fst ind1)
        then longueur (snd ind2) - longueur (snd ind1)
    else (fst ind2) - (fst ind1)


(** sélection des meilleurs individus sans doublons*)
let meilleurs n pop =
    let pop_triee = List.sort_uniq compare pop in
    n_premiers n (pop_triee @ (pop_alea n))


(** réalise tous les croisements deux à deus possibles *)
let rec augmente pop = match pop with
    | [] -> []
    | t::q -> (List.map (croise t) q) @ (augmente q)
    

(** la génération suivante est obtenue de la sorte :
        1. Sélection des 13 meilleurs
        2. Croisements (nouveau total = 91)
        3. Mutations
        4. Ajout de 9 individus aléatoires
    Total = 100 individus *)
let gen_suivante pop taux =
    let parents = meilleurs 10 pop in
    parents @ (muter_pop (augmente parents) taux) @ (pop_alea 9)


let evolution_s nb_gen taux =
    p_n () ; p_i (Random.int 10000) ; p_n () ; (* random ID *)
    let pop = ref (pop_alea 100) in
    for i = 0 to nb_gen do
        p_s "Generation n°" ; p_i i ; p_s " (f:" ;
        let best = hd (meilleurs 1 !pop) in
        p_i (fst best) ; p_s ") : " ; p_s (sob (snd best)) ; p_n () ;
        pop := gen_suivante !pop taux
    done ;
    let resultat = sob (snd (hd (meilleurs 1 !pop))) in
    p_n () ;
    p_s ("Résultat :  " ^ resultat) ;
    p_n () ; p_n () ;
    resultat *>> bot_objectif

    
let evolution_s_quiet nb_gen taux =
    p_n () ; p_i (Random.int 10000) ; p_n () ; (* random ID *)
    let pop = ref (pop_alea 100) in
    for i = 0 to nb_gen do
        if i mod 20 = 0 then begin
            p_s "Generation n°" ; p_i i ; p_s " (f:" ;
            let best = hd (meilleurs 1 !pop) in
            p_i (fst best) ; p_s ") : " ; p_s (sob (snd best)) ; p_n ()
        end ;
        pop := gen_suivante !pop taux
    done ;
    let resultat = sob (snd (hd (meilleurs 1 !pop))) in
    p_n () ;
    p_s ("Résultat :  " ^ resultat) ;
    p_n () ; p_n () ;
    resultat *>> bot_objectif


let evolve () =
    for i = 0 to 20 do
        evolution_s_bis 200 0.1 ;
        p_n ()
    done
    
    

(*
let evolution_s nb_gen taux_m =
    print_newline () ;
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
    print_newline () ;
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

*)