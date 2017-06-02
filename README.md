
# Bf Jousting for Dummies...


This project aims to obtain an almost unbeatable robot at Bf Joust (see [Esolang - BF Joust](https://esolangs.org/wiki/BF_Joust)).

For this we will implement a standard genetic algorithm, then two variations: an ecosystem simulation and a parallel evolution strategy.


## 0. Jousting



## 1. Standard genetic algorithm



## 2.1 Ecosystem simulation : fixed goal


The following rules determine the simulation:
- The ecosystem begins with a set of 100 random bots on a 100x100 grid.
- At each new cycle, every alive bot sees its life decreased by 1, and moves to a random positions int the 5x5 pixels square around him.
- When the life of a bot reaches zero, he dies.
- If two bots are on the exact same spot, they mate: their child joins the game.
- Two special rules can be applied : if the population gets too low, everyone becomes immortal. If the population gets too high, no more childs are created.
- At the end of each cycle, some bots are genetically mutated.



## 2.2 Ecosystem simulation : no fixed goal !

The following rules determine the simulation:
- The ecosystem begins with a set of 100 random bots on a 100x100 grid.
- If two bots are on the exact same spot, they can either **fight** or **mate**, with equal probability.
- The loser of a fight (if any) dies.
- To ensure the stability of the population, some phases can be activated. A ****purge phase** means that any encounter between two bots is a fight, a **breeding phase** means that it is a mate.
- After a cetain number of years, a purge phase is activated until one last bot only is alive. To force their meetings, the grid is progressively reduced to a single point.
- At the end of each cycle, some bots are genetically mutated.

For more fun, it is possible to add some serious bots to the game to see how the others evolve.

Some more rules could be added : (yet to come...)
