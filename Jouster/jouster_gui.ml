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

type polarite = Norm | Inv



(** FONCTIONS PRATIQUES **)

let hd l = List.hd l
let tl l = List.tl l

let (@@) array1 array2 = Array.append array1 array2

let rec ( *@) arr n = if n <= 0 then [||] else arr @@ (arr *@ (n-1)) 
let rec ( *^) str n = if n <= 0 then ""   else str  ^ (str *^ (n-1))

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


let battle_gui bot1 bot2 taille pol secs =
    traceWinner (
    let bot2 = reverse_bot (if pol = Inv then reverse_pol_bot bot2 else bot2) in
    let mem = [|128|] @@ ([|0|] *@ (taille - 2)) @@ [|128|] in
    let len1, len2 = String.length bot1, String.length bot2 in
    traceInstant mem 0 (taille - 1) ; pause 2.0 ;
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
            let delta = (abs mem.(0) - abs mem.(taille - 1)) in
            if delta > 0 then 1 else if delta < 0 then -1 else 0
        else
            let mem_i, p1_i = Array.copy mem, p1 in
            let (p1, i1, l1) = exec bot1 p1 (if i1 >= len1 then 0 else i1) l1 in
                traceVariation mem_i mem p1_i p1 p2 p2 ;
            let mem_i, p2_i = Array.copy mem, p2 in
            let (p2, i2, l2) = exec bot2 p2 (if i2 >= len2 then 0 else i2) l2 in
                traceVariation mem_i mem p1 p1 p2_i p2 ;
                pause secs ;
        let zz1, zz2 = (mem.(0) = 0), (mem.(taille - 1) = 0) in
        if (zz1 && z1) || (zz2 && z2) || p1 < 0 || p2 < 0 || p1 = taille || p2 = taille then
            begin
                if (zz1 && z1) && (zz2 && z2) then 0 else
                if (abs (p1 - p2)) = (taille + 1) then 0 else
                if (zz1 && z1) || (p1 < 0) || (p1 = taille) then (-1)
                else 1
            end
        else fight (cycle + 1) p1 p2 i1 i2 l1 l2 (mem.(0) = 0) (mem.(taille-1) = 0)
    in fight 0 0 (taille -1) 0 0 [] [] false false
    )


let all_battles_gui bot1 bot2 secs =
    for i = 10 to 30 do
        battle_gui bot1 bot2 i Norm secs ;
        battle_gui bot1 bot2 i Inv secs ;
    done



(** TESTS GRAPHIQUES **)

let randrange amplitude = Random.int(2*amplitude + 1) - amplitude

let rec random_mem taille amp = if taille = 0 then [||]
    else [|(randrange amp/2)+(randrange amp/2)|] @@ (random_mem (taille - 1) amp)

let random_disp () =
    traceInstant ([|123|] @@ (random_mem 28 50) @@ [|119|]) (Random.int 30) (Random.int 30)


;;

random_disp ()