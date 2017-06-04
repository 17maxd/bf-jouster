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



(** TYPES **)

type polarity = Norm | Inv



(** BASIC FUNCTIONS AND SHORTCUTS **)

let hd = List.hd
let tl = List.tl

let (@@) = Array.append

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


let trace_text g =
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


let traceColumn g pos heigth color =
    set_color color ;
    if heigth > 0 then
        fill_rect (g + 17*pos) 209 15 (heigth - 1)
    else if heigth < 0 then
        fill_rect (g + 17*pos) (191 + heigth) 15 (-heigth - 1)


(* edits the column vith a variation delta \in {-1 , 1} *)
let editeColumn g pos val_init delta color =
    let g' = g + 17*pos in
    set_color color ;
    if (abs delta) > 2 then
        if delta < 0 then
        (traceColumn g pos 128 dark ; traceColumn g pos (-127) color)
        else
        (traceColumn g pos (-127) dark ; traceColumn g pos 128 color)
    else if val_init > 0 then
        if delta > 0 then
            fill_rect g' (209 + val_init) 15 0
        else
            (set_color dark ; fill_rect g' (209 + val_init - 1) 15 0)
    else if val_init < 0 then
        if delta = (-1) then
            fill_rect g' (191 + val_init - 1) 15 0
        else
            (set_color dark ; fill_rect g' (191 + val_init) 15 0)
    else begin (* cas ou val_init = 0 *)
        traceColumn g pos delta color
    end


let trace_bot g pos color =
    set_color color ;
    fill_rect (g + 17*pos) 192 15 15 


let trace_instant mem bot1 bot2 =
    let g = (600 - ((Array.length mem) * 17)) / 2 in (
    clear_graph () ;
    trace_text g ;
    let len = Array.length mem in
    for i = 0 to (len -1) do
        let color = if i = 0 then light_blue
                    else if i = (len -1) then light_red
                    else white in
        traceColumn g i mem.(i) color
    done ;
    if bot1 = bot2 then trace_bot g bot1 magenta
    else (trace_bot g bot1 blue ; trace_bot g bot2 red)
    )


(** bot1 (blue) went from pos bi to bf
    bot2 (red) went from pos ri to rf *)
let trace_variation mem1 mem2 bi bf ri rf =
    let g = (600 - ((Array.length mem1) * 17)) / 2 in (
    let len = Array.length mem2 in
    for i = 0 to (len -1) do
        if mem1.(i) == mem2.(i) then () else
        let color = if i = 0 then light_blue
                    else if i = (len -1) then light_red
                    else white in
        editeColumn g i mem1.(i) (mem2.(i) - mem1.(i)) color
    done ;
    match (bf-bi, rf-ri, bf-rf, bi-ri) with
        | 0,0,_,_ -> ()
        | 0,_,0,_ -> (trace_bot g ri dark ; trace_bot g rf magenta)
        | 0,_,_,0 -> (trace_bot g bi blue ; trace_bot g rf red)
        | 0,_,_,_ -> (trace_bot g ri dark ; trace_bot g rf red)
        | _,0,0,_ -> (trace_bot g bi dark ; trace_bot g bf magenta)
        | _,0,_,0 -> (trace_bot g ri red  ; trace_bot g bf blue)
        | _,0,_,_ -> (trace_bot g bi dark ; trace_bot g bf blue)
        | _ ->       (trace_bot g bi dark ; trace_bot g ri dark ;
                      trace_bot g bf blue ; trace_bot g rf red)
    )


let trace_winner id =
    moveto (match id with 0 -> 235 | 1 -> 241 | _ -> 247) 20 ;
    set_color white ;
    set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1" ;
    draw_string ( if id = 0 then "It's a tie!"
                else if id = 1 then "Blue wins!" else "Red wins!")



(* BOTS FIGHTS! *)

(** jumps to the matching ']'-bracket after the i-th character in the bot,
    the 'depth' variable allows to avoid sub-loops *)
let rec jump str_bot i depth = match str_bot.[i] with
    | '[' -> jump str_bot (i+1) (depth+1)
    | ']' -> if depth = 1 then i else jump str_bot (i+1) (depth-1)
    |  _  -> jump str_bot (i+1) depth


(** returns an inverted copy of the bot *)
let rev_bot str_bot =
    let copy = Bytes.copy str_bot in
    let len = String.length(copy) in
    for i = 0 to len - 1 do
        match copy.[i] with
            |'>' -> Bytes.set copy i '<'
            |'<' -> Bytes.set copy i '>'
            | _  -> ()
    done ; copy


(** returns an polarity-reverted copy of the bot *)
let rev_pol_bot str_bot =
    let copy = Bytes.copy str_bot in
    let len = String.length(copy) in
    for i = 0 to len - 1 do
        match copy.[i] with
            |'+' -> Bytes.set copy i '-'
            |'-' -> Bytes.set copy i '+'
            | _  -> ()
    done ; copy


let joust_gui bot1 bot2 size pol secs =
    let bot2 = rev_bot (if pol = Inv then rev_pol_bot bot2 else bot2) in
    let mem = [|128|] @@ ([|0|] *@ (size - 2)) @@ [|128|] in
    let len1, len2 = String.length bot1, String.length bot2 in
    trace_instant mem 0 (size - 1) ; pause 0.5 ;
    let exec bot p i l = match bot.[i] with
        | '>' -> (p+1, i+1, l)
        | '<' -> (p-1, i+1, l)
        | '+' -> (if mem.(p) = 128 then mem.(p) <- (-127)
                  else mem.(p) <- mem.(p) + 1 ; (p, i+1, l))
        | '-' -> (if mem.(p) = (- 127) then mem.(p) <- 128
                  else mem.(p) <- mem.(p) - 1 ; (p, i+1, l))
        | '.' -> (p, i+1, l)
        | '[' -> if mem.(p) = 0 then (p, 1 + (jump bot i 0), l)
                 else (p, i+1, (i+1)::l)
        | ']' -> if mem.(p) = 0 then (p, i+1, (tl l))
                 else (p, (hd l), l)
        |  _  -> (p, i+1, l)
    in let rec fight cycle p1 p2 i1 i2 l1 l2 z1 z2 =
        if (cycle > 5000) || (i1 >= len1 && i2 >= len2) then
            let delta = (abs mem.(0) - abs mem.(size - 1)) in
            if delta > 0 then 1 else if delta < 0 then -1 else 0
        else
            let (p1, i1, l1) =
                if i1 >= len1 then (p1, i1, l1)
                else let mem_i, p1_i = Array.copy mem, p1 in
                     let (p1, i1, l1) = exec bot1 p1 i1 l1 in
                     trace_variation mem_i mem p1_i p1 p2 p2 ; pause secs ;
                     (p1, i1, l1) in
            let (p2, i2, l2) =
                if i2 >= len2 then (p2, i2, l2)
                else let mem_i, p2_i = Array.copy mem, p2 in
                     let (p2, i2, l2) = exec bot2 p2 i2 l2 in
                     trace_variation mem_i mem p1 p1 p2_i p2 ; pause secs ;
                     (p2, i2, l2) in
            let zz1, zz2 = (mem.(0) = 0), (mem.(size - 1) = 0) in
                (if (zz1 && z1) && (zz2 && z2) then 0
                                else if abs (p1 - p2) > size then 0
                                else if (zz1 && z1) || (p1 < 0) || (p1 = size) then (-1)
                                else if (zz2 && z2) || (p2 < 0) || (p2 = size) then 1
                                else fight (cycle + 1) p1 p2 i1 i2 l1 l2 zz1 zz2)
    in fight 0 0 (size -1) 0 0 [] [] false false


let all_battles_gui bot1 bot2 secs =
    for i = 10 to 30 do
        pause 0.5 ;
        joust_gui bot1 bot2 i Norm secs ;
        pause 0.5 ;
        joust_gui bot1 bot2 i Inv secs ;
    done



(** GRAPHIC DEBUG **)

let randrange amplitude = Random.int(2*amplitude + 1) - amplitude

let rec random_mem size amp = if size = 0 then [||]
    else [|(randrange amp/2)+(randrange amp/2)|] @@ (random_mem (size - 1) amp)

let random_disp () =
    trace_instant ([|123|] @@ (random_mem 28 50) @@ [|119|]) (Random.int 30) (Random.int 30)

;;

random_disp ()
