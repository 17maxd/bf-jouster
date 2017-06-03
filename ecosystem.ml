(*
    File: ecosystem.ml
    Version: 1.0
    Author: Max D3
*)



(* TYPES *)

type bot = instr list
and instr = | P | M | L | R | W
            | Lp of instr list

type polarity = Norm | Inv

type mutation = Insert | Delete | Permut

type joust_issue = Timeout | Capture | Exit

type winner = Left | Tie | Right

type individual = { x : int ; y : int ; life : int ; code : bot }



(** BASIC FUNCTIONS AND SHORTCUTS **)

let (+=) a b = (a := (!a + b))
let (-=) a b = (a := (!a - b))

let rand () = Random.float 1.0

let (@-) = List.nth

let rec ( **^) str n =
    if n < 1 then ""
    else (str ^ str) **^ (n / 2) ^ if n mod 2 = 0 then "" else str

let p_i = print_int
let p_s = print_string
let p_n = print_newline



(* ABOUT:ROBOTS *)

let rec string_of_bot = function
    | [] -> ""
    | P :: q -> "+" ^ string_of_bot q
    | M :: q -> "-" ^ string_of_bot q
    | L :: q -> "<" ^ string_of_bot q
    | R :: q -> ">" ^ string_of_bot q
    | W :: q -> "." ^ string_of_bot q
    | Lp bot :: q -> "[" ^ (string_of_bot bot) ^ "]" ^ string_of_bot q

let sob = string_of_bot


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


let life_expectancy bot = let x = (fitness bot + 20) in x*x



(* 2. RANDOM BOTS GENERATION *)

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
    {x = Random.int 99; y = Random.int 99; life = life_expectancy bot; code = bot}


let rec rand_pop size = match size with
    | 0 -> []
    | n -> (rand_ind 25 3) :: (rand_pop (size - 1))



(* 3. MUTATIONS *)

let rand_mutation () = [|Insert ; Delete ; Permut|].(Random.int 3)


let rec mutate_bot bot mut_prob =
    let rec aux = function
        | Lp l :: q -> Lp (mutate_bot l mut_prob) :: (aux q)
        | t :: Lp l :: q -> t :: Lp (mutate_bot l mut_prob) :: (aux q)
        | t :: t' :: q -> (
            if rand () > mut_prob then t::(aux (t'::q))
            else match rand_mutation () with
                    | Insert -> t :: (rand_instr_std ()) :: (aux (t'::q))
                    | Delete -> aux (t'::q)
                    | Permut -> t'::t::(aux q)
            )
        | x -> x
    in aux bot


let mutate ind mut_prob =
    let mutant = mutate_bot ind.code mut_prob in
    {x = ind.x; y = ind.y; life = life_expectancy mutant; code = mutant}


let rec mutate_pop pop mut_prob = match pop with
    | t::q -> (mutate t mut_prob) :: (mutate_pop q mut_prob)
    | [] -> []



(* 4. MOVING *)

let move ind =
    if ind.x = 0  then ind.x <- ind.x + Random.int 3 else
    if ind.x = 99 then ind.x <- ind.x - Random.int 3 else
    ind.x <- ind.x + (Random.int 7) - 3 ;
    if ind.y = 0  then ind.y <- ind.y + Random.int 3 else
    if ind.y = 99 then ind.y <- ind.y - Random.int 3 else
    ind.y <- ind.y + (Random.int 7) - 3


let rec move_pop = function
    | t::q -> (move t ; move_pop q)
    | [] -> ()



(* 5. BREEDING *)

(** outputs the first n elements from a list.
    if len(list) > n, outputs the whole list. *)
let rec first_n n list =
    if n <= 0 then [] else
    if n >= List.length list then list else
    (hd list) :: (first_n (n-1) (tl list))


(** outputs the last n elements from a list.
    if len(list) > n, outputs the whole list. *)
let rec last_n n list =
    if n <= 0 then [] else
    if n >= List.length list then list else
    last_n n (tl list)


(** mating two bots, giving a child having the same length as bot2 *)
let mate_bot bot1 bot2 =
    let pos1 = Random.int (List.length bot1) in
    let pos2 = max ((List.length bot2) - pos1) 0 in
    if Random.bool () then
         (first_n pos1 bot1) @ (last_n pos2 bot2)
    else (first_n pos2 bot2) @ (last_n pos1 bot1) 


let mate ind1 ind2 =
    let child = mate_bot ind1.code ind2.code in
    {x = ind1.x; y = ind1.y; life = life_expectancy enfant; code = enfant}



