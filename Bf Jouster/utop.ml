#use "jouster.ml";;
#use "evo_v2.ml";;

print_string "\nt_m = 0.01\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 0.01
done ;
print_string "\nt_m = 0.05\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 0.05
done ;

print_string "\nt_m = 0.1\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 0.1
done ;

print_string "\nt_m = 0.2\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 0.2
done ;

print_string "\nt_m = 0.3\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 0.3
done ;

print_string "\nt_m = 0.4\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 0.4
done ;

print_string "\nt_m = 0.5\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 0.5
done ;

print_string "\nt_m = 0.75\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 0.75
done ;

print_string "\nt_m = 1.0\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 1.0
done ;

print_string "\n\nFINI!" ;;