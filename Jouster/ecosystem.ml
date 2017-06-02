(*
    File : ecosysteme.ml
    Version : 1.0
    Author : Max D3
*)



(* TYPES *)

type bot = instr list
and instr = | P | M | L | R | W
            | Lp of instr list

type polarite = Norm | Inv

type mutation = Insert | Delete | Permut

type fin_combat = Timeout | Capture | Exit

type gagnant = Left | Tie | Right

type individual = { mutable x : int ;
                    mutable y : int ;
                    mutable vie : int ;
                    mutable code : bot }



(** BASIC FUNCTIONS AND SHORTCUTS **)

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



(* ABOUT:ROBOTS *)

let rec string_of_bot = function
    | [] -> ""
    | P :: q -> "+" ^ string_of_bot q
    | M :: q -> "-" ^ string_of_bot q
    | L :: q -> "<" ^ string_of_bot q
    | R :: q -> ">" ^ string_of_bot q
    | W :: q -> "." ^ string_of_bot q
    | Lp bot :: q -> "[" ^ (string_of_bot bot) ^ "]" ^ string_of_bot q

let sob bot = string_of_bot bot


(** calculates the length of the string representation of a bot *)
let rec length bot =
    let rec aux = function
        | [] -> 0
        | Lp bot :: q -> 2 + (length bot) + (aux q)
        | _ :: q -> 1 + aux q
    in aux bot



(* 1. FITNESS *)


(** slightly edited versions of bots found on codegolf,
    the self flag reduction was removed in order to avoid
    ties being counted as a victory *)
let bot_MickeyV4_m     = ">------>->---<<------>->---->------------->>--->------<----------------<------<-<<--<------------->--------<-->------>------->----------->-------------->-------->------->----------------[>[--[-[+]]]>[--[+]]-]-------[>[--[-[+]]]>[--[+]]-]<--<------>------->----------------[>[--[-[+]]]>[--[+]]-]<--<---------------------->------>->-<-----"
let bot_CounterPunch_m = ">------------>>>>>>><------------<++++++++++++<------------<++++++++++++<------------<++++++++++++>>>>>>>" ^ ("[-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]]>" **^ 21)
let bot_Bigger_m       = ">->+>+>->------------------>++++++++++++++++++>------------------>++++++++++++++++++" ^ (">[++++++++++++++++++[-][-[+]]][++++++++++++++++++[-][-[+]]]" **^ 21)


(** chooses against which bot the algorithm will learn to fight *)
let objective_bot = bot_Bigger_m


let score bot1 bot2 size pol =
    match joust bot1 bot2 size pol with
    | (Tie, _, _, _) -> 0
    | (Left, Timeout, _, _) -> 1
    | (Right, Timeout, _, _) -> -1
    | (Left, Capture, d, n) -> d/10 + 8 - n/200
    | (Right, Capture, d, n) -> d/10 - 8 + n/200
    | (Left, Exit,  _, _) -> 20
    | (Right, Exit,  _, _) -> -20


let fitness bot =
    let bot_s = sob bot in
   ((score bot_s objective_bot 11 Norm) +
    (score bot_s objective_bot 21 Norm) +
    (score bot_s objective_bot 24 Norm) +
    (score bot_s objective_bot 29 Norm) +
    (score bot_s objective_bot 30 Norm) +
    (score bot_s objective_bot 13 Inv ) +
    (score bot_s objective_bot 17 Inv ) +
    (score bot_s objective_bot 20 Inv ) +
    (score bot_s objective_bot 25 Inv ) +
    (score bot_s objective_bot 29 Inv )) / 10


let duree_vie bot = let x = (fitness bot + 20) in x*x



(* 2. GÉNÉRATION ALÉATOIRE *)

let instr_tab = [|P ; M ; P ; M ; L ; R ; W ; Lp[]|]
let instr_std_tab = [|P ; M ; P ; M ; L ; R ; W|]


let rand_instr () = instr_tab.(Random.int 8)
let rand_instr_std () = instr_std_tab.(Random.int 7)


(** creates a random bot with a length around 'len' *)
let rec rand_bot len max_depth =
    let rec aux i =
        if i = 0 then [] else
        match rand_instr () with
            | Lp _ -> if max_depth < 0 then aux i else
                      Lp (rand_bot (len/2) (max_depth-1)) :: (aux (i-1))
            | x -> x :: (aux (i-1))
    in aux (1 + Random.int len)


let rec rand_ind len max_depth =
    let bot = rand_bot len max_depth in
    {x = Random.int 99; y = Random.int 99; vie = duree_vie bot; code = bot}


let rec rand_pop size = match size with
    | 0 -> []
    | n -> (rand_ind 25 3) :: (rand_pop (size - 1))



(* 3. MUTATIONS *)

let rand_mutation () = [|Insert ; Delete ; Permut|].(Random.int 3)


let rec muter_bot bot mut_prob =
    let rec aux = function
        | Lp l :: q -> Lp (muter_bot l mut_prob) :: (aux q)
        | t :: Lp l :: q -> t :: Lp (muter_bot l mut_prob) :: (aux q)
        | t :: t' :: q -> (
            if rand () > mut_prob then t::(aux (t'::q))
            else match rand_mutation () with
                    | Insert -> t :: (rand_instr_std ()) :: (aux (t'::q))
                    | Delete -> aux (t'::q)
                    | Permut -> t'::t::(aux q)
            )
        | x -> x
    in aux bot


let muter ind mut_prob =s
    let mutant = muter_bot ind.code mut_prob in
    {x = ind.x; y = ind.y; vie =  duree_vie mutant; code = mutant}


let rec muter_pop pop mut_prob = match pop with
    | t::q -> (muter t mut_prob) :: (muter_pop q mut_prob)
    | [] -> []



(* 4. DÉPLACEMENTS *)

let deplace ind =
    if ind.x = 0  then ind.x <- ind.x + Random.int 3 else
    if ind.x = 99 then ind.x <- ind.x - Random.int 3 else
    ind.x <- ind.x + (Random.int 7) - 3 ;
    if ind.y = 0  then ind.y <- ind.y + Random.int 3 else
    if ind.y = 99 then ind.y <- ind.y - Random.int 3 else
    ind.y <- ind.y + (Random.int 7) - 3


let rec deplace_pop = function
    | t::q -> (deplace t ; deplace_pop q)
    | [] -> ()



(* 5. CROISEMENTS *)

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
    let enfant = croise_bot ind1.code ind2.code in
    {x = ind1.x; y = ind1.y; vie = duree_vie enfant; code = enfant}



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
    n_premiers n (pop_triee @ (rand_pop n))


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
let gen_suivante pop mut_prob =
    let parents = meilleurs 10 pop in
    parents @ (muter_pop (augmente parents) mut_prob) @ (rand_pop 9)


let evolution_s nb_gen mut_prob =
    p_n () ; p_i (Random.int 10000) ; p_n () ; (* random ID *)
    let pop = ref (rand_pop 100) in
    for i = 0 to nb_gen do
        p_s "Generation n°" ; p_i i ; p_s " (f:" ;
        let best = hd (meilleurs 1 !pop) in
        p_i (fst best) ; p_s ") : " ; p_s (sob (snd best)) ; p_n () ;
        pop := gen_suivante !pop mut_prob
    done ;
    let resultat = sob (snd (hd (meilleurs 1 !pop))) in
    p_n () ;
    p_s ("Résultat :  " ^ resultat) ;
    p_n () ; p_n () ;
    resultat *>> bot_objectif



(** affiche seulement une génération sur 20 *)    
let evolution_s_quiet nb_gen mut_prob =
    p_n () ; p_i (Random.int 10000) ; p_n () ; (* random ID *)
    let pop = ref (rand_pop 100) in
    for i = 0 to nb_gen do
        if i mod 20 = 0 then begin
            p_s "Generation n°" ; p_i i ; p_s " (f:" ;
            let best = hd (meilleurs 1 !pop) in
            p_i (fst best) ; p_s ") : " ; p_s (sob (snd best)) ; p_n ()
        end ;
        pop := gen_suivante !pop mut_prob
    done ;
    let resultat = sob (snd (hd (meilleurs 1 !pop))) in
    p_n () ;
    p_s ("Résultat :  " ^ resultat) ;
    p_n () ; p_n () ;
    resultat *>> bot_objectif


let evolve () =
    for i = 0 to 20 do
        evolution_s_quiet 200 0.1 ;
        p_n ()
    done
