% TIPE
% Maximilien Dupont de Dinechin
% 17 novembre 2016

I. Évolution simple
===================

- Objectif : `MickeyV4bis`

Il s'agit d'une variation de `MickeyV4` qui garde son drapeau à 128. Cela permet
de donner un score nul aux bots qui induisent un dépassement de cycle.

Nous allons faire varier certains paramètres en gardant les autres fixes pour
comprendre leur influence, et essayer de déterminer les valeurs qui donnent les
résultats les plus satisfaisants.

Un score est un entier compris entre -42 et 42, et qui indique le nombre de
victoire et défaites d'un bot dans toutes les configurations possibles face à
`MickeyV4bis`.

Les paramètres standard sont :

- Taux de mutation : $t_m = 0.1$
- Composition d'une population : $13$ parents, $78$ enfants, $n_a = 20$ aléatoires
- Crossover : un seul point
- Évaluation : `Norm (10, 21, 30)` ; `Inv (13, 17, 25)`
- Générations : $n_g = 100$


a. Variation du taux de mutation
--------------------------------

|$t_m$| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10| 11| 12| 13| 14| 15| 16| 17| 18| 19| 20|moy|  
|----:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|  
|     |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |  
| 0.5 | 10|-22|-15| 2 | 21| 23|-16|-42|-16|-40|-13|-22| 7 |-26| 0 |-42| 18|-42| 17| 18| -9|  
| 0.4 |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |  
| 0.3 |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |  
| 0.2 |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |  
| 0.1 | 17|-42| 26| 18| -4| 9 |-22|-22| 9 |-12|   |   |   |   |   |   |   |   |   |   |   |  
| 0.05|   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |  
| 0.01|   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |  
| 0.  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |  

<!-- 
| 0.25  | -18 |  4  |  3  |  0  |  1  | 18  |     |  
| 0.01  | -15 | -16 | 22  | -22 | -24 | -42 |     |  
 -->

b. Variation du nombre d'aléatoires ajoutés
-------------------------------------------

| $n_a$ | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10| 11| 12| 13| 14| 15| 16| 17| 18| 19| 20|moy|  
| ----- |:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|  
|       |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |  
|  0.5  |10 |-22|-15| 2 | 21| 23|-16|-42|-16|-40|-13|-22| 7 |-26| 0 |-42| 18|-42| 17| 18|   |  
|  0.4  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |  
|  0.3  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |  
|  0.2  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |  
|  0.1  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |  
|  0.05 |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |  
|  0.01 |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |  