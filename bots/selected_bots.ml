(** Concaténation rapide *)
let rec ( **^) str n =
    if n < 1 then ""
    else (str ^ str) **^ (n / 2) ^ if n mod 2 = 0 then "" else str


let bot_MickeyV4_m     = ">------>->---<<------>->---->------------->>--->------<----------------<------<-<<--<------------->--------<-->------>------->----------->-------------->-------->------->----------------[>[--[-[+]]]>[--[+]]-]-------[>[--[-[+]]]>[--[+]]-]<--<------>------->----------------[>[--[-[+]]]>[--[+]]-]<--<---------------------->------>->-<-----"
let bot_CounterPunch_m = ">------------>>>>>>><------------<++++++++++++<------------<++++++++++++<------------<++++++++++++>>>>>>>" ^ ("[-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]][-[------[+.]][------[+.]][------[+.]][------[+.]][------[+.]]]>" **^ 21)
let bot_Bigger_m       = ">->+>+>->------------------>++++++++++++++++++>------------------>++++++++++++++++++" ^ (">[++++++++++++++++++[-][-[+]]][++++++++++++++++++[-][-[+]]]" **^ 21)

let bot_G1             = ">.>+-.<+>-+<+++>+++++>>+.>-+<+>-+<+++<++.-.[[+]>.+.+>.]+>.[[+]>.+.+>.]"
let bot_G2             = "[>[<[>>[+]]]--]"
let bot_G3             = "++[+++.>+--]-[>--+-[+]--+]"

let bot_E1             = ">-[[-->[+]--][++[>-+]-]+[.-]+<.]<"
let bot_E2             = "[[>[>[-]]--]..+.]"
let bot_E3             = "-[[->-]+-[[+]>]]-[[->-]--+-[[+]>]]--"

let bot_P              = "-->->-+->-->-+->->-+->-->+-->-+-[.[+++[>>-].]-.[>[+]-]++<]"

let tab_bots = [|bot_MickeyV4_m; bot_CounterPunch_m; bot_Bigger_m; bot_G1; bot_G2; bot_G3; bot_E1; bot_E2; bot_E3; bot_P |]
let tab_noms = [|"Mv4"; "CP"; "Bg"; "G1"; "G2"; "G3"; "E1"; "E2"; "E3"; "P" |]


let rec tab_scores tab names =
    let l = Array.length tab in
    for i = 0 to (l-1) do
    print_newline () ;
        let bot1 = tab.(i) in
        for j = 0 to (l-1) do
            let bot2 = tab.(j) in
            let score = (bot1 *> bot2) in
            if j = 0 then print_string ("\\bl{" ^ names.(i) ^ "}") ;
            print_string " &" ;
            print_string (if score<0 then "\\rg"
                          else if score >0 then "\\vr "
                          else "\\gr ") ;
            print_int score ;
            print_string (if (abs score) > 9 then "" else " ")
        done ;
        print_string "\\\\"
    done ;
    print_newline ()


let rec tab_scores_clean tab =
    let l = Array.length tab in
    for i = 0 to (l-1) do
    print_newline () ;
        let bot1 = tab.(i) in
        for j = 0 to (l-1) do
            let bot2 = tab.(j) in
            let score = (bot1 *> bot2) in
            print_string (if score <0 then "" else " ") ;
            print_int score ;
            print_string (if (abs score) > 9 then " " else "  ")
        done
    done ;
    print_newline ()
