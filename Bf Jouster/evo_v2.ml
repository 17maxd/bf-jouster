



(* TYPAGE *)

type bot = instr list
and instr = | P | M | L | R | W
            | Lp of instr list

type polarite = Norm | Inv

type mutation = Insert | Delete | Switch

type fin_combat = Timeout | Capture | Sortie

type gagnant = Gauche | Nul | Droite

type resultat = gagnant * fin_combat * int * int




(* VARIABLES GLOBALES *)

let instructions = [|P ; M ; P ; M ; L ; R ; W ; Lp[]|]
let instructions' = [|P ; M ; P ; M ; L ; R ; W|]


let pop_size = 100
let taux_mutation = 0.01
let nb_enfants = 2 (* Le nombre d'enfants que deux parents créent *)
let min_program_size = 10
let max_program_size = 500



(* FONCTIONS PRATIQUES *)

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
let instr_alea' () = instructions'.(Random.int 7)

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

let mutation_alea () = [|Insert ; Delete ; Switch|].(Random.int 3)

let rec mutate bot taux =
    let rec aux = function
        | Lp l :: q -> Lp (mutate l taux) :: (aux q)
        | t :: Lp l :: q -> t :: Lp (mutate l taux) :: (aux q)
        | t :: t' :: q -> (
            if rand () > taux
            then t::(aux (t'::q)) else
            match mutation_alea () with
                | Insert -> t :: (instr_alea' ()) :: (aux (t'::q))
                | Delete -> aux (t'::q)
                | Switch -> t'::t::(aux q)
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

let bot_MickeyV4bis = ">------>->---<<------>->---->------------->>--->------<----------------<------<-<<--<------------->--------<-->------>------->----------->-------------->-------->------->----------------[>[--[-[+]]]>[--[+]]-]-------[>[--[-[+]]]>[--[+]]-]<--<------>------->----------------[>[--[-[+]]]>[--[+]]-]<--<---------------------->------>->-<-----"
let bot_FastClearBot = ">>>>>>>>>" ^ ("[+++[-]]>" **^ 21)


let score bot1 bot2 n pol =
    match battle bot1 bot2 n pol with
    | (Nul, _, _, _) -> 0
    | (Gauche, Timeout, _, _) -> 3
    | (Droite, Timeout, _, _) -> -3
    | (Gauche, Capture, d, n) -> d/10 + 8 - n/200
    | (Droite, Capture, d, n) -> d/10 - 8 + n/200
    | (Gauche, Sortie,  _, _) -> 20
    | (Droite, Sortie,  _, _) -> -20



(* ALGORITHME GÉNÉTIQUE SIMPLE *)

(* let fitness_s bot =
    let sbot = sob bot in
    let pts = ref 0 in
    for i = 10 to 30 do
        pts := !pts + score sbot bot_MickeyV4bis i Inv ;
        pts := !pts + score sbot bot_MickeyV4bis i Norm
    done ; !pts / 42 *)

let fitness_s bot =
   (score (sob bot) bot_MickeyV4bis 30 Norm +
    score (sob bot) bot_MickeyV4bis 21 Norm +
    score (sob bot) bot_MickeyV4bis 10 Norm +
    score (sob bot) bot_MickeyV4bis 25 Inv  +
    score (sob bot) bot_MickeyV4bis 17 Inv  +
    score (sob bot) bot_MickeyV4bis 13 Inv) / 6  

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

(*  13 parents
    78 enfants
    20 random    *)

let rec nextgen_s parents = 
    let rec aux i j =
        if j = 13 then [] else
        if i = 13 then aux (j+1) (j+1) else
        begin
            let enfant = mutate (croise (parents @- i) (parents @- j)) 0.1 in
            if enfant = [] then [L] :: (aux (i+1) j)
            else enfant :: (aux (i+1) j)
        end
    in (aux 0 0) @ (pop_alea 20 (20,3))


let evolution_s n =
    print_int (Random.int 10000) ;
    print_newline () ;
    let rec aux i gen = if i = 0 then hd (tri_s gen)
    else (
        print_string "Generation n°" ;
        print_int (n - i) ;
        print_string " (" ;
        let parents = best_n_of_pop_s 13 gen in
        let best = hd parents in
        print_int (fitness_s best) ;
        print_string ") : " ;
        print_string (sob best) ;
        print_newline () ;
        (* ignore (battle_gui (sob (hd gen)) bot_MickeyV4bis 30 Norm 0.) ; *)
        aux (i-1) (nextgen_s parents)
    ) in let result = sob (aux n (pop_alea 100 (20,3))) in
    print_newline () ;
    print_string result ;
    print_newline () ;
    battle_gui result bot_MickeyV4bis 30 Norm 0.01 ;
    result *>> bot_MickeyV4bis


(* DEBUG *)

let testbot1 = [W;R;P;M;P;P;L;Lp[P;W;Lp[M];R];M;R;P;M;R;P;M;Lp[W;Lp[L;Lp[L]]];P]

