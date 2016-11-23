(*
    File : jouster_gui.ml
    Version : 3.0
    Author : max
*)



(** APPELS GRAPHIQUES **)

#load "graphics.cma" ;;

open Graphics ;;
Graphics.open_graph "" ;;
Graphics.set_window_title "BrainF*ck Bots Battle!" ;;



(** TYPAGE **)

type bot = instr list
and instr = | A of int
            | M of int
            | W
            | L of instr list

type mutation = Insert | Delete | Switch

type polarite = Norm | Inv



(** FONCTIONS PRATIQUES **)

let hd l = List.hd l
let tl l = List.tl l

let (+=) a b = (a := (!a + b))
let (-=) a b = (a := (!a - b))

let (++) a = 3

let (@@) array1 array2 = Array.append array1 array2

let rec ( *@) arr n = if n <= 0 then [||] else arr @@ (arr *@ (n-1)) 
let rec ( *^) str n = if n <= 0 then ""   else str  ^ (str *^ (n-1))

let rec (@-) l n = List.nth l n

let foi i = float_of_int i
let iof f = int_of_float f
let soi i = string_of_int i

let rand x = Random.float 1.0

let pause secs =
    let t = Sys.time () in
    while Sys.time () < (t +. secs) do
        ()
    done



(** GRAPHICS **)

let white = rgb 248 248 242
let dark = rgb 39 40 34
let lighter_gray = rgb 200 200 200
let magenta = rgb 240 100 240
let blue = rgb 120 98 255
let red = rgb 249 38 79
let light_blue = rgb 150 129 255
let light_red = rgb 249 38 114


let traceTexte g =
    set_color dark ; fill_rect 0 0 600 500 ;
    set_color (rgb 60 60 60) ;
    fill_rect g 208 (598 - 2*g) 0 ;
    fill_rect g 191 (598 - 2*g) 0 ;
    moveto 174 380 ;
    set_color white ;
    set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1" ;
    draw_string "BrainF*ck Bots Battle" ;
    moveto 8 430 ;
    set_font "9x15" ;
    set_color lighter_gray ;
    draw_string "version 3.0"


let traceColone g pos valeur couleur =
    set_color couleur ;
    if valeur > 0 then
        fill_rect (g + 17*pos) 209 15 (valeur - 1)
    else if valeur < 0 then
        fill_rect (g + 17*pos) (191 + valeur) 15 (-valeur - 1)


(* Un ajout appartient à {-1 , 1} *)
let editeColone g pos val_init ajout couleur =
    let g' = g + 17*pos in
    set_color couleur ;
    if (abs ajout) > 2 then
        if ajout < 0 then
        (traceColone g pos 128 dark ; traceColone g pos (-127) couleur)
        else
        (traceColone g pos (-127) dark ; traceColone g pos 128 couleur)
    else if val_init > 0 then
        if ajout > 0 then
            fill_rect g' (209 + val_init) 15 0
        else
            (set_color dark ; fill_rect g' (209 + val_init - 1) 15 0)
    else if val_init < 0 then
        if ajout = (-1) then
            fill_rect g' (191 + val_init - 1) 15 0
        else
            (set_color dark ; fill_rect g' (191 + val_init) 15 0)
    else begin (* cas ou val_init = 0 *)
        traceColone g pos ajout couleur
    end


let traceBot g pos couleur =
    set_color couleur ;
    fill_rect (g + 17*pos) 192 15 15 


let traceInstant mem bot1 bot2 =
    let g = (600 - ((Array.length mem) * 17)) / 2 in (
    clear_graph () ;
    traceTexte g ;
    let len = Array.length mem in
    for i = 0 to (len -1) do
        let couleur = if i = 0 then light_blue
                    else if i = (len -1) then light_red
                    else white in
        traceColone g i mem.(i) couleur
    done ;
    if bot1 = bot2 then traceBot g bot1 magenta
    else (traceBot g bot1 blue ; traceBot g bot2 red)
    )


(* Hypothèse simplificatrice : les colones sont modifiées de 1, 0 ou -1 *)
let traceVariation mem1 mem2 bi bf ri rf =
    let g = (600 - ((Array.length mem1) * 17)) / 2 in (
    let len = Array.length mem2 in
    for i = 0 to (len -1) do
        if mem1.(i) == mem2.(i) then () else
        let couleur = if i = 0 then light_blue
                    else if i = (len -1) then light_red
                    else white in
        editeColone g i mem1.(i) (mem2.(i) - mem1.(i)) couleur
    done ;
    match (bf-bi, rf-ri, bf-rf, bi-ri) with
        | 0,0,_,_ -> ()
        | 0,_,0,_ -> (traceBot g ri dark ; traceBot g rf magenta)
        | 0,_,_,0 -> (traceBot g bi blue     ; traceBot g rf red)
        | 0,_,_,_ -> (traceBot g ri dark ; traceBot g rf red)
        | _,0,0,_ -> (traceBot g bi dark ; traceBot g bf magenta)
        | _,0,_,0 -> (traceBot g ri red      ; traceBot g bf blue)
        | _,0,_,_ -> (traceBot g bi dark ; traceBot g bf blue)
        | _ -> (traceBot g bi dark ; traceBot g ri dark ;
                traceBot g bf blue ; traceBot g rf red)
    )


let traceWinner id =
    moveto (match id with 0 -> 235 | 1 -> 241 | _ -> 247) 20 ;
    set_color white ;
    set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1" ;
    draw_string ( if id = 0 then "It's a tie!"
                else if id = 1 then "Blue wins!" else "Red wins!")



(* COMBAT DE BOTS! *)

let rec jump bot i c = match bot.[i] with
    | '[' -> jump bot (i+1) (c+1)
    | ']' -> if c = 1 then i else jump bot (i+1) (c-1)
    |  _  -> jump bot (i+1) c


let reverse_bot str_bot =
    let copy = Bytes.copy str_bot in
    let len = String.length(copy) in
    for i = 0 to len - 1 do
        match copy.[i] with
            |'>' -> Bytes.set copy i '<'
            |'<' -> Bytes.set copy i '>'
            | _  -> ()
    done ; copy


let reverse_pol_bot str_bot =
    let copy = Bytes.copy str_bot in
    let len = String.length(copy) in
    for i = 0 to len - 1 do
        match copy.[i] with
            |'+' -> Bytes.set copy i '-'
            |'-' -> Bytes.set copy i '+'
            | _  -> ()
    done ; copy


let battle_gui bot1 bot2 size pol secs = let w = (
    let bot2 = reverse_bot (if pol = Inv then reverse_pol_bot bot2 else bot2) in
    let mem = [|128|] @@ ([|0|] *@ (size - 2)) @@ [|128|] in
    let len1, len2 = String.length bot1, String.length bot2 in
    traceInstant mem 0 (size - 1) ; pause 0.1 ;
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
        if cycle > 5000 then
            let delta = (abs mem.(0) - abs mem.(size - 1)) in
            if delta > 0 then 1 else if delta < 0 then -1 else 0
        else
            let mem_i, p1_i = Array.copy mem, p1 in
            let (p1, i1, l1) = exec bot1 p1 (if i1 >= len1 then 0 else i1) l1 in
                traceVariation mem_i mem p1_i p1 p2 p2 ;
            let mem_i, p2_i = Array.copy mem, p2 in
            let (p2, i2, l2) = exec bot2 p2 (if i2 >= len2 then 0 else i2) l2 in
                traceVariation mem_i mem p1 p1 p2_i p2 ;
                pause secs ;
        let zz1, zz2 = (mem.(0) = 0), (mem.(size - 1) = 0) in
        if (zz1 && z1) || (zz2 && z2) || p1 < 0 || p2 < 0 || p1 = size || p2 = size then
            begin
                if (zz1 && z1) && (zz2 && z2) then 0 else
                if (abs (p1 - p2)) = (size + 1) then 0 else
                if (zz1 && z1) || (p1 < 0) || (p1 = size) then (-1)
                else 1
            end
        else fight (cycle + 1) p1 p2 i1 i2 l1 l2 (mem.(0) = 0) (mem.(size-1) = 0)
    in fight 0 0 (size -1) 0 0 [] [] false false
    ) in traceWinner w ; w


let all_battles_gui bot1 bot2 secs =
    let score = ref 0 in
    for i = 10 to 30 do
        score -= (battle_gui bot1 bot2 i Norm secs) ;
        score -= (battle_gui bot1 bot2 i Inv secs) ;
    done ; !score




(** CONVERSIONS **)

let rec string_of_bot = function
  | [] -> ""
  | t::q -> (match t with
    | A i -> (if i>0 then "+" else "-") *^ (abs i)
    | M i -> (if i>0 then ">" else "<") *^ (abs i)
    | W -> "."
    | L bot -> "[" ^ (string_of_bot bot) ^ "]"
  ) ^ string_of_bot q

let sob bot = string_of_bot bot


let rec c_string_of_bot = function
  | [] -> ""
  | t::q -> (match t with
    | A i when i > 0 -> "(+)*" ^ (soi i)
    | A i when i < 0 -> "(-)*" ^ (soi i)
    | A  _ -> ""
    | M i when i > 0 -> "(>)*" ^ (soi i)
    | M i when i < 0 -> "(<)*" ^ (soi i)
    | M _ -> ""
    | W -> "."
    | L bot -> "[" ^ (c_string_of_bot bot) ^ "]"
  ) ^ c_string_of_bot q

let csob bot = c_string_of_bot bot



(** EVALUATION **)

let rec jump bot i c = match bot.[i] with
    | '[' -> jump bot (i+1) (c+1)
    | ']' -> if c = 1 then i else jump bot (i+1) (c-1)
    |  _  -> jump bot (i+1) c

let reverse_bot str_bot =
    let copy = Bytes.copy str_bot in
    let len = Bytes.length(copy) in
    for i = 0 to len - 1 do
        match copy.[i] with
            |'>' -> Bytes.set copy i '<'
            |'<' -> Bytes.set copy i '>'
            | _  -> ()
    done ; copy

let reverse_pol_bot str_bot =
    let copy = Bytes.copy str_bot in
    let len = Bytes.length(copy) in
    for i = 0 to len - 1 do
        match copy.[i] with
            |'+' -> Bytes.set copy i '-'
            |'-' -> Bytes.set copy i '+'
            | _  -> ()
    done ; copy


(* Comptage des points : en cas de victoire par : *)
(* Match nul : 0 points *)
(* Destruction du flag : ±5 ±{1, 12} *)
(* Sortie de grille : ±20 points *)
(* Timeout : ± 1 point *)


let fight bot1 bot2 size pol =
    let bot2 = reverse_bot (if pol = Inv then reverse_pol_bot bot2 else bot2) in
    let mem = [|128|] @@ ([|0|] *@ (size - 2)) @@ [|128|] in
    let len1, len2 = Bytes.length bot1, Bytes.length bot2 in
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
        if cycle > 1000 then
            let delta = (abs mem.(0) - abs mem.(size - 1)) in
            if delta > 0 then 1 else if delta < 0 then -1 else 0
        else
            let (p1, i1, l1) = exec bot1 p1 (if i1 >= len1 then 0 else i1) l1 in
            let (p2, i2, l2) = exec bot2 p2 (if i2 >= len2 then 0 else i2) l2 in
        let zz1, zz2 = (mem.(0) = 0), (mem.(size - 1) = 0) in
        if (zz1 && z1) || (zz2 && z2) || p1 < 0 || p2 < 0 || p1 = size || p2 = size then
            begin
                if (zz1 && z1) && (zz2 && z2) then 0 else
                if (abs (p1 - p2)) = (size + 1) then 0 else
                if (p1 < 0) || (p1 = size) then -20 else
                if (p2 < 0) || (p2 = size) then 20  else
                if zz1 && z1 then
                - ((abs mem.(size - 1) / 10) + 5) else
                (abs mem.(0) / 10) + 5
            end
        else fight (cycle + 1) p1 p2 i1 i2 l1 l2 (mem.(0) = 0) (mem.(size-1) = 0)
    in fight 0 0 (size -1) 0 0 [] [] false false



(** GÉNÉRATION ALÉATOIRE **)

let norm_rand r = (Random.int (1 + r*2) + Random.int (1 + r*2)) / 2 - r

let rec norm_rand_nonzero r =
    let result = norm_rand r in
    if result = 0 then norm_rand_nonzero r
    else result 


let rec rand_bot max_depth max_L_size =
    let size = 1 + Random.int max_L_size in
    let rec aux i depth =
        if i = 0 then [] else
        match Random.int 5 with
            | 0 -> A (norm_rand_nonzero 5) :: (aux (i-1) depth)
            | 1 -> A (norm_rand_nonzero 5) :: (aux (i-1) depth)
            | 2 -> M (norm_rand_nonzero 5) :: (aux (i-1) depth)
            | 3 -> M (norm_rand_nonzero 5) :: (aux (i-1) depth)
            | _ -> if depth = max_depth then aux (i+1) depth else
    L (rand_bot (max_depth - 1) (max_L_size /  2)) :: (aux (i-1) (depth + 1))
            (*| _ -> W :: (aux (i-1) depth)*)
    in aux size 0



(** MUTATION **)

(* On peut muter de plusieurs manières :
 *   - déletion
 *   - insertion
 *   - échange avec le suivant 
 *)

let rand_mutation x = [|Insert ; Delete ; Switch|].(Random.int 3)

let rec mutate bot taux =
    let rec aux = function
        | L l :: q -> L (mutate l taux) :: (aux q)
        | t :: L l :: q -> t :: L (mutate l taux) :: (aux q)
        | t :: t' :: q -> (
            if rand () > taux
            then t::(aux (t'::q)) else
            match rand_mutation () with
                | Insert -> t :: (rand_bot 0 1) @ (aux (t'::q))
                | Delete -> aux (t'::q)
                | Switch -> t'::t::(aux q)
            )
        | t :: q -> [t]
        | _ -> []
    in aux bot


(* CROISEMENT *)


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



(** SÉLÉCTION **)

let totalfight bot1 bot2 =
    if bot1 = [] then -1000 else
    if bot2 = [] then 1000 else
    let sbot1, sbot2 = sob bot1, sob bot2 in
    (fight sbot1 sbot2 30 Norm)


let tri_comparatif pop = List.rev (List.stable_sort totalfight pop)

(* tri_comparatif [[W] ; [A 1] ; [M (-1)] ; [M (-1) ; W ; W] ; [M 1]] *)


let rec firstgen gensize =
    if gensize = 0 then []
    else (rand_bot 3 20) :: (firstgen (gensize - 1))


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
