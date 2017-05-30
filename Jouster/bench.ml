#use "jouster.ml";;
#use "jouster_gui.ml";;
#use "evo_simple.ml";;

print_string "\n   -- EVOLUTION BENCHMARK --\n" ;

print_string "\nTaux de mutation : 0.01\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 0.01
done ;
print_string "\nTaux de mutation : 0.05\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 0.05
done ;

print_string "\nTaux de mutation : 0.1\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 0.1
done ;

print_string "\nTaux de mutation : 0.2\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 0.2
done ;

print_string "\nTaux de mutation : 0.3\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 0.3
done ;

print_string "\nTaux de mutation : 0.4\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 0.4
done ;

print_string "\nTaux de mutation : 0.5\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 0.5
done ;

print_string "\nTaux de mutation : 0.75\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 0.75
done ;

print_string "\nTaux de mutation : 1.0\n" ;

for i = 0 to 19 do
    evolution_s_bis 100 1.0
done ;

print_string "\n\nFINI!" ;;
