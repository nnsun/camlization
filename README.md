# Camlization
A game inspired by Sid Meier's Civilization, written in OCaml. 

CS 3110 Fall 2017 Final Project

© 2017 Alex Strandberg, Daniel Li, Ning Ning Sun, and Cynthia Tu.

![Screenshot](docs/game.png?raw=true "In game screenshot")

## Installation

Note: we assume that you are running on the CS 3110 VM. Please follow these [instructions](http://www.cs.cornell.edu/Courses/cs3110/2017fa/install.html) if you need to install the VM, or these [instructions](http://www.cs.cornell.edu/courses/cs3110/2020sp/install.html) to install OCaml natively.

Camlization requires the [Notty](https://github.com/pqwy/notty) terminal graphics library. 

To install it using OPAM, run `opam install notty` and follow the prompts.

## Playing the Game
Simply run the Makefile with no options to play, by entering the code directory and running the command `make`. This will compile the code and run the game.  

You can play the game again by running `make play`.

Note: Please maximize your terminal window or enter full-screen mode before running the game. The GUI will resize according to terminal window size and font size.

## Gameplay

Overall, gameplay is very similar to that of Civilization V. Each player starts out with a worker, which should be used to found a city. Cities can then be used to build military units that are used to attack and defend against other civilizations. Cities generate some amount of science per turn, which can then be used to research technoologies. These technologies can unlock more advanced units such as the fearsome Swordsman or Horseman. Some, such as Agriculture or Mining, can also reveal resources such as Wheat or Gems on the map, which give improved tile yields. Tile yields can furthermore be improved with improvements such as Farms, Mines, Fishing Boats, and Pastures, which can be built by Workers or Work Boats. 
