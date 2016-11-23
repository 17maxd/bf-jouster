(*
    File : evo_std.ml
    Version : 1.0
    Author : max
*)



(** TYPAGE **)

type bot = instr list
and instr = | A of int
            | M of int
            | W
            | L of instr list

type polarite = Norm | Inv

type mutation = Insert | Delete | Switch



(** FONCTIONS PRATIQUES **)

let hd l = List.hd l
let tl l = List.tl l

let (+=) a b = (a := (!a + b))
let (-=) a b = (a := (!a - b))

let (@@) array1 array2 = Array.append array1 array2

let rec ( *@ ) arr n = if n <= 0 then [||] else arr @@ (arr *@ (n-1)) 
let rec ( *^ ) str n = if n <= 0 then ""   else str  ^ (str *^ (n-1))

let rec (@-) l n = List.nth l n

let foi i = float_of_int i
let iof f = int_of_float f
let soi i = string_of_int i

let rand x = Random.float 1.0



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
        match Random.int 6 with
            | 0 -> A (norm_rand_nonzero 5) :: (aux (i-1) depth)
            | 1 -> A (norm_rand_nonzero 5) :: (aux (i-1) depth)
            | 2 -> M (norm_rand_nonzero 5) :: (aux (i-1) depth)
            | 3 -> M (norm_rand_nonzero 5) :: (aux (i-1) depth)
            | 4 -> if depth = max_depth then aux (i+1) depth else
    L (rand_bot (max_depth - 1) (max_L_size /  2)) :: (aux (i-1) (depth + 1))
            | _ -> W :: (aux (i-1) depth)
    in aux size 0


let rec firstgen gensize =
    if gensize = 0 then []
    else (rand_bot 3 20) :: (firstgen (gensize - 1))



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

