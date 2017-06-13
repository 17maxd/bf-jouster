(*
    File: ecosystem.ml
    Version: 1.0
    Author: Max D3
*)



(** GRAPHICS **)

#load "graphics.cma" ;;
open Graphics ;;
Graphics.open_graph "" ;;



(* TYPES *)

type bot = instr list
and instr = | P | M | L | R | W
            | Lp of instr list

type polarity = Norm | Inv

type mutation = Insert | Delete | Permut

type joust_issue = Timeout | Capture | Exit

type winner = Left | Tie | Right

type individual = { x : int ; y : int ; fit : int ; life : int ; code : bot }



(** BASIC FUNCTIONS AND SHORTCUTS **)

let (+=) a b = (a := (!a + b))
let (-=) a b = (a := (!a - b))

let rand () = Random.float 1.0

let (@-) = List.nth

let rec ( **^) str n =
    if n < 1 then ""
    else (str ^ str) **^ (n / 2) ^ if n mod 2 = 0 then "" else str

let pause secs =
    let t = Sys.time () in
    while Sys.time () < (t +. secs) do
        ()
    done

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
    ties being counted as a victory *)
let bot_MickeyV4_m     = ">------>->---<<------>->---->------------->>--->------<----------------<------<-<<--<------------->--------<-->------>------->----------->-------------->-------->------->----------------[>[--[-[+]]]>[--[+]]-]-------[>[--[-[+]]]>[--[+]]-]<--<------>------->----------------[>[--[-[+]]]>[--[+]]-]<--<---------------------->------>->-<-----"
let bot_CounterPunch_m = ">------------>>>>>>><------------<++++++++++++<------------<++++++++++++<------------<++++++++++++>>>>>>>" ^ ("[-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]]>" **^ 21)
let bot_Bigger_m       = ">->+>+>->------------------>++++++++++++++++++>------------------>++++++++++++++++++" ^ (">[++++++++++++++++++[-][-[+]]][++++++++++++++++++[-][-[+]]]" **^ 21)


(** Chooses against which bot the algorithm will learn to fight *)
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


let rec calc_fitness_pop = function
    | t::q -> {x=t.x; y=t.y; fit=fitness t.code; life=t.life; code=t.code}::(calc_fitness_pop q)
    | [] -> []


(** Value : 
        * 0 if fit < -4
        * 10 if fit < -2
        * else 10 + (fit+5)^6 *)
let life_exp fit =
    if fit < -4 then 0 else
    let x = max fit (-4) in
    let y = (x + 5) in 10 + (y*y*y*y*y*y)/1000


let reduce_life ind =
    { x = ind.x ; y = ind.y; fit = ind.fit; life = ind.life - 1; code = ind.code }


let rec reduce_life_pop = function
    | t::q -> (reduce_life t) :: (reduce_life_pop q)
    | _ -> []



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
    let fitn = fitness bot in
   {x = Random.int 99 ;
    y = Random.int 99 ;
    fit = fitn ;
    life = life_exp fitn ;
    code = bot}


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
   {x = ind.x ; y = ind.y ;
    fit = fitness mutant ;
    life = ind.life ;
    code = mutant}


let rec mutate_pop pop mut_prob = match pop with
    | t::q -> (if rand () < mut_prob then (mutate t mut_prob) else t)
                :: (mutate_pop q mut_prob)
    | [] -> []



(* 4. MOVING *)

let rd_int = Random.int


let move ind =
    { x = max 0 (min (ind.x - 2 + rd_int 5) 99) ;
      y = max 0 (min (ind.y - 2 + rd_int 5) 99) ;
      fit = ind.fit; life = ind.life ; code = ind.code}


let rec move_pop = function
    | t::q -> (move t) :: (move_pop q)
    | [] -> []



(* 5. BREEDING *)

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
    let fitn = fitness child in
   {x = Random.int 99 ;
    y = Random.int 99 ;
    fit = fitn ;
    life = life_exp fitn ;
    code = child}



(* 6. POPULATION SORT *)

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


(** Sorts the population by lexicographic order on (x, y) *)
let sort_pop pop =    
    let compare ind1 ind2 =
        if ind1.x = ind2.x
        then  ind1.x - ind2.x
        else ind1.x - ind2.x
    in List.sort compare pop



(** 7. TRACE **)

let dark = rgb 39 40 34 ;;
let grey = rgb 200 200 200 ;;
let magenta = rgb 240 100 240 ;;


let trace_init () =
    Graphics.set_window_title "Ecosystem simulation" ;
    set_color dark ;
    fill_rect 0 0 600 500 ;
    moveto 174 380 ;
    set_color white ;
    set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1" ;
    draw_string "Ecosystem Simulation" ;
    moveto 8 430 ;
    set_font "9x15" ;
    set_color grey ;
    draw_string "version 1.0" ;
    for m = 11 to 15 do
        moveto (50-m) (30-m) ;
        lineto (50-m) (330+m) ;
        lineto (550+m) (330+m) ;
        lineto (550+m) (30-m) ;
        lineto (50-m) (30-m)
    done


let clear () =
    set_color dark ;
    let m = 11 in
    fill_rect (50-(m-1)) (30-(m-1)) (500+2*(m-1)) (300+2*(m-1))


let trace_ind ind =
    set_color white ;
    draw_circle (5 * ind.x + 50) (3 * ind.y + 30) (max 1 (ind.life / 2))


(* traces the five best individuals in red **)
let rec trace_pop = function
    | t::q -> (trace_ind t ; trace_pop q)
    | _ -> ()


(* 8. ECOSYSTEM SIMULATION *)

let close_enough ind1 ind2 = (ind1.x = ind2.x)


let ecosystem_gui nb_gen mut_prob =
    trace_init () ;
    print_string "Initialisation..." ;
    print_newline () ;
    let pop = ref (rand_pop 300) in
    let rec kill = function
        | t::q -> (if t.life > 0 then t::(kill q) else kill q)
        | [] -> []
    in let rec breed = function
        | t::t'::q -> (if close_enough t t'
                        then t :: (mate t t') :: (breed (t'::q))
                        else t :: (breed (t'::q)))
        | x -> x
    in for i = 0 to nb_gen do
        pop := move_pop !pop ;
        if (List.length !pop) > 80 then (pop := reduce_life_pop !pop) ;
        pop := kill !pop ;
        if (List.length !pop) < 150 then (pop := breed !pop) ;
        if (List.length !pop) < 50 then (pop := !pop @ rand_pop 100) ;
        pop := mutate_pop !pop mut_prob ;
        if i mod 50 = 0 then begin
            let best = hd (best_n 1 !pop) in
            let fit, code = best.fit, (sob best.code) in
            Printf.printf "Generation n°%d (f:%d) : %s" i fit code ;
            print_newline ()
        end ;
        clear () ;
        trace_pop !pop ;
        pause 0.02
    done ;
    let result = sob (hd (best_n 1 !pop)).code in
    Printf.printf "\nResult: %s\n\n" result ;
    result *>> objective_bot


let ecosystem nb_gen mut_prob =
    let pop = ref (rand_pop 500) in
    let rec kill = function
        | t::q -> (if t.life > 0 then t::(kill q) else kill q)
        | [] -> []
    in let rec breed = function
        | t::t'::q -> (if close_enough t t'
                        then t :: (mate t t') :: (breed (t'::q))
                        else t :: (breed (t'::q)))
        | x -> x
    in for i = 0 to nb_gen do
        pop := move_pop !pop ;
        if (List.length !pop) > 50 then (pop := reduce_life_pop !pop) ;
        pop := kill !pop ;
        if (List.length !pop) < 100 then (pop := breed !pop) ;
        pop := mutate_pop !pop mut_prob ;
        if i mod 50 = 0 then begin
            let best = hd (best_n 1 !pop) in
            let fit, code = best.fit, (sob best.code) in
            Printf.printf "Generation n°%d (f:%d) : %s" i fit code ;
            print_newline ()
        end ;
    done ;
    let result = sob (hd (best_n 1 !pop)).code in
    Printf.printf "\nResult: %s\n\n" result ;
    result *>> objective_bot
