
# Bf Jousting for Dummies...

This project aims to obtain an almost unbeatable robot at Bf Joust (see [Esolang - BF Joust](https://esolangs.org/wiki/BF_Joust)).

For this we will implement a standard genetic algorithm, then two variations: an ecosystem simulation and a parallel evolution strategy.

Those can be loaded in `utop` by, from the `Jouster/` folder, launching respectively `utop -init utop_G.ml`, `utop -init utop_E.ml`, or `utop -init utop_P.ml` 


## 0. Jousting (`jouster.ml` and `jouster_gui.ml`)

(From [codegolf](https://codegolf.stackexchange.com/questions/36645/brainfedbotsforbattling-a-brainf-tournament)).

### Description of a joust

Two bots (Brainfuck programs) are fighting each other in an arena which is represented by a memory tape. Each cell can hold values from -127 up to 128 and wrap at their limits (so 128 + 1 = -127).

Valid instructions are similiar to regular Brainfuck, which means:

> + : Increment cell at your pointer's location by 1
> - : Decrement cell at your pointer's location by 1
> \> : Move your memory pointer by 1 cell towards the enemy flag
> < : Move your memory pointer by 1 cell away from the enemy flag
> [ : Jump behind the matching ']'-bracket if the cell at your pointer's location equals 0
> ] : Jump behind the matching '['-bracket if the cell at your pointer's location is not 0
> . : Do nothing

The arena has a size of 10 to 30 cells. At both ends is a 'flag' located which has an initial value of 128, while all other cells are zeroed. A bot's goal is to zero the enemy's flag for 2 consecutive cycles before he zeroes your own flag.

Each bot starts at his own flag, which is cell [0] from his own perspective. The opponent is located on the other side of the tape.

```
[ 128 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 128 ]
   ^                                             ^
my bot                                       other bot
```

Both bots execute their action simultaneously, this is considered one cycle. The game ends after 10000 cycles or as soon as one of the winning conditions is reached. If one of the programs reaches its end, it simply stops doing anthing until the end of the game, but can still win.

### Winning conditions

A bot wins under one of the following conditions:

- The enemy's flag is zeroed before yours
- The enemy moves his pointer out of the tape (executes > on your flag or < on his own)
- Its flag's value is more far away from 0 than the value of his opponent's flag after 2000 cycles.

### `jouster.ml`

The function `joust` file computes the issue of a fight between two bots, on a given arena size and polarity.

```ocaml
joust bot_MickeyV4_m bot_Bigger_m 25 Norm;;
- : winner * joust_issue * int * int = (Left, Capture, 115, 1618)

The function `( *>> )` outputs the issues from fights on all ranges and polarities between two bots

```ocaml
# bot_MickeyV4_m *>> bot_Bigger_m;;
NORMAL : -1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
INVERSE: -1  1  1  1  1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1  1 -1 -1 -1 -1 -1
Score : 8
- : unit = ()
```

### `jouster_gui.ml`

The function `joust_gui` displays an animation of a joust between two bots. The last parameter is the number of seconds between two frames.

```ocaml
joust_gui bot_MickeyV4_m bot_Bigger_m 25 Norm 0.01;;
```

![joust_gui](/images/joust_gui.gif)

## 1. Standard genetic algorithm



## 2 Ecosystem simulation (`ecosystem.ml`)

An ecosystem simulation is a more realistic variation of genetic algorithms. Instead of killing all individuals at the end of a cycle, the individuals die either of great age or by being killed during a joust.

Individuals are now moving randomly on a 100x100 grid. In the first variation, when two of them 'meet' (by being at the same position at the same time), then breed, in the second one they can either breed or engage a death match by jousting.

After a certain amount of cycles, the simulation can be stopped by stopping any breeding. The size of the population is then forced to decrease, and the last individual alive is the winner of the simulation.

### 2) First variant : standard ecosystem

(corresponds to the function `ecosystem_std` in `ecosystem.ml`)

The following rules determine the simulation:

- The ecosystem begins with a set of 100 random bots in the grid.
- At each new *cycle*, every alive bot has its life decreased by 1, and moves to a random position int the 5x5 pixels square surrounding him.
- When the life of a bot reaches zero, he dies.
- If two bots are on the same spot, they breed: their child joins the game.
- At the end of each cycle, some bots are genetically mutated.

Two special rules can be temporarily be applied in specific cases: if the population gets too low (below 80), the bots life stops decreasing. If the population gets too high (over 120), no more childs are created.

After a fixed number of cycles, 

### 2) Second variant : death match ecosystem

(corresponds to the function `ecosystem_dm` in `ecosystem.ml`)

The following rules determine the simulation:

- The ecosystem begins with a set of 100 random bots in the grid.
- At each new *cycle*, every alive bot moves to a random position int the 5x5 pixels square surrounding him.
- If two bots are on the same spot, they can either **fight** or **breed** with equiprobability.
- The loser of a fight (if any) dies. When the fight ends up in a tie, both bots get flagged, and if their next match also ends up in a tie, they are eliminated.
- At the end of each cycle, some bots are genetically mutated.
- After a fixed number of years, all breeding are stopped until one bot only is alive This bot is the result of the simulation. (To force the meeting of the last alive bots, the grid is progressively reduced to a single point).

Two special rules can be temporarily be applied in specific cases: if the population gets too low (below 80), an encounter ends up in a fight. If the population gets too high (over 120), a meeting is necessarily a breeding.