(*
    File: genetic.ml
    Author: Max D3
*)



(* TYPES *)

type individual = { fit : int ; code : bot }



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


(** Calculates the length of the string representation of a bot. *)
let rec length bot =
    let rec aux = function
        | [] -> 0
        | Lp bot :: q -> 2 + (length bot) + (aux q)
        | _ :: q -> 1 + aux q
    in aux bot



(* 1. FITNESS *)


(** Slightly edited versions of bots found on codegolf,
    the self flag reduction was removed in order to avoid
    ties being counted as a victory. *)
let bot_MickeyV4_m     = ">------>->---<<------>->---->------------->>--->------<----------------<------<-<<--<------------->--------<-->------>------->----------->-------------->-------->------->----------------[>[--[-[+]]]>[--[+]]-]-------[>[--[-[+]]]>[--[+]]-]<--<------>------->----------------[>[--[-[+]]]>[--[+]]-]<--<---------------------->------>->-<-----"
let bot_CounterPunch_m = ">------------>>>>>>><------------<++++++++++++<------------<++++++++++++<------------<++++++++++++>>>>>>>" ^ ("[-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]]>" **^ 21)
let bot_Bigger_m       = ">->+>+>->------------------>++++++++++++++++++>------------------>++++++++++++++++++" ^ (">[++++++++++++++++++[-][-[+]]][++++++++++++++++++[-][-[+]]]" **^ 21)


(** Chooses against which bot the algorithm will learn to fight. *)
let objective_bot = bot_MickeyV4_m


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
    let points = score (sob bot) objective_bot in
    ( (points 11 Norm) + (points 21 Norm) + (points 24 Norm) + (points 29 Norm)
    + (points 30 Norm) + (points 13 Inv ) + (points 17 Inv ) + (points 20 Inv )
    + (points 25 Inv ) + (points 29 Inv )) / 10


    
(* 2. RANDOM BOTS GENERATION *)

let instr_tab = [|P ; M ; P ; M ; L ; R ; W ; Lp[]|]
let instr_std_tab = [|P ; M ; P ; M ; L ; R ; W|]


let rand_instr () = instr_tab.(Random.int 8)
let rand_instr_std () = instr_std_tab.(Random.int 7)


(** Creates a random bot with a length around 'len' *)
let rec rand_bot len max_depth =
    let rec aux i =
        if i = 0 then [] else
        match rand_instr () with
            | Lp _ -> if max_depth < 0 then aux i else
                      Lp (rand_bot (len/2) (max_depth-1)) :: (aux (i-1))
            | x -> x :: (aux (i-1))
    in aux (1 + Random.int (max len 1))


let rec rand_ind len max_depth =
    let bot = rand_bot len max_depth in
    {fit = fitness bot ; code = bot}


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
                    | Insert -> if length bot > 80 then t::t'::q else
                                t :: (rand_instr_std ()) :: (aux (t'::q))
                    | Delete -> aux (t'::q)
                    | Permut -> t'::t::(aux q)
            )
        | x -> x
    in aux bot


let mutate ind mut_prob =
    let mutant = mutate_bot ind.code mut_prob in
    {fit = fitness mutant; code = mutant}


let rec mutate_pop pop mut_prob = match pop with
    | t::q -> (if rand () < mut_prob then (mutate t mut_prob) else t)
                :: (mutate_pop q mut_prob)
    | [] -> []



(* 4. BREEDING *)

(** Outputs the first n elements from a list.
    If len(list) > n, outputs the whole list. *)
let rec first_n n list =
    if n <= 0 then [] else
    if n >= List.length list then list else
    (hd list) :: (first_n (n-1) (tl list))


(** Outputs the last n elements from a list.
    If len(list) > n, outputs the whole list. *)
let rec last_n n list =
    if n <= 0 then [] else
    if n >= List.length list then list else
    last_n n (tl list)


(** Mates two bots, with len(child) in [len(1), len(2)] *)
let rec mate_bot bot1 bot2 =
    let l1, l2 = length bot1, length bot2 in
    if l2 >= l1 then
        let p1 = Random.int (max l1 1) in
        let p2 = Random.int (max (l2 - p1) 1) in
        (first_n p1 bot1) @ (last_n p2 bot2)
    else
        mate_bot bot2 bot1

let mate ind1 ind2 =
    let child = mate_bot ind1.code ind2.code in
    {fit = fitness child ; code = child}


(** mates every possible couple of bots from pop *)
let rec mate_pop = function
    | [] -> []
    | t::q -> (List.map (mate t) q) @ (mate_pop q)



(* 5. STANDARD GENETIC ALGORITHM *)


(** Selects the n best individuals, removing doubles.
    If len(pop)<n, random individuals are added to replace the missing ones.
    The indvidials are sorted according to their fitness, and if equal, the
    shorter is the better. *)
let best_n n pop =
    let compare ind1 ind2 =
        if ind2.fit = ind1.fit
        then length ind1.code - length ind2.code
        else ind2.fit - ind1.fit
    in let sorted_pop = List.sort_uniq compare pop in
    if n <= List.length sorted_pop
        then first_n n sorted_pop
        else first_n n (sorted_pop @ (rand_pop n))


(** The next generation is obtained by: :
        1. Selecting the top 13 bots
        2. Mating all possible couples from those 13 bots
        3. Mutating the obtained childs plus their parents
        4. Introduction of 9 random bots
    Total = 100 individuals *)
let next_generation pop mut_prob =
    let parents = best_n 13 pop in
    parents @ (mutate_pop (mate_pop parents) mut_prob) @ (rand_pop 9)


(* Runs the genetic algorithm *)
let evolution nb_gen mut_prob =
    Printf.printf "\n%d" (Random.int 10000) ; (* random ID *)
    print_newline () ;
    let pop = ref (rand_pop 100) in
    for i = 0 to nb_gen do
        let best = hd (best_n 1 !pop) in
        let fit, code = best.fit, (sob best.code) in
        Printf.printf "Generation n°%d (f:%d) : %s" i fit code ;
        print_newline () ;
        pop := next_generation !pop mut_prob
    done ;
    let result = sob (hd (best_n 1 !pop)).code in
    Printf.printf "\nResult: %s\n\n" result ;
    result *>> objective_bot


(** Same as the previous, displays the current best only every 20 generations *)
let evolution_quiet nb_gen mut_prob =
    Printf.printf "\n%d\n" (Random.int 10000) ; (* random ID *)
    print_newline () ;
    let pop = ref (rand_pop 100) in
    for i = 0 to nb_gen do
        if i mod 20 = 0 then begin
            let best = hd (best_n 1 !pop) in
            let fit, code = best.fit, (sob best.code) in
            Printf.printf "Generation n°%d (f:%d) : %s" i fit code ;
            print_newline ()
        end ;
        pop := next_generation !pop mut_prob
    done ;
    let result = sob (hd (best_n 1 !pop)).code in
    Printf.printf "\nResult: %s\n\n" result ;
    result *>> objective_bot


(** Same as the previous, displays nothing but the final result *)
let evolution_silent nb_gen mut_prob =
    let pop = ref (rand_pop 100) in
    for i = 0 to nb_gen do
        pop := next_generation !pop mut_prob
    done ;
    let result = sob (hd (best_n 1 !pop)).code in
    Printf.printf "\nResult: %s\n\n" result ;
    result *>> objective_bot


let multiple_evolutions n nb_gen mut_prob =
    for i = 0 to n do
        evolution_quiet nb_gen mut_prob
    done
