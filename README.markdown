# Small tetris implementation using ocaml-allegro. #

## Author ##

Martin DeMello <martindemello@gmail.com>

## Design ##
-  `tetris.ml`: Handle and update the game state
-  `main.ml`: Display the board and interact with the user

The game-state and game-interaction parts have been separated as much as possible; the goal is to be able to port to a different game engine or gui toolkit simply by replacing main.ml

## Install ##

Requires:  

-  `ocaml-batteries`
-  `ocaml-allegro` [[download](http://www.linux-nantes.org/~fmonnier/ocaml/Allegro/)]
-  GNU `make`

The `makefile` assumes everything has been installed via `findlib` (if you have installed ocaml and batteries via godi this should hold good).

A basic META file (`META.ocaml-allegro`) is included to install ocaml-allegro (see the [ocamlfind documentation](http://projects.camlcity.org/projects/dl/findlib-1.2.5/doc/ref-html/r17.html#OCAMLFIND.INSTALL)). The following should work:

     $ cp META.ocaml-allegro /path/to/ocaml-allegro/META
     $ cd /path/to/ocaml-allegro/META
     $ ocamlfind install allegro *

## Credits ##

Based heavily on the [mltetris](http://abaababa.ouvaton.org/caml/) implementation by Oguz Berke DURAK

Background image from [NASA Images](http://www.nasaimages.org/luna/servlet/detail/NVA2~4~4~5724~106250:Stars-Young-and-Old)

## Contributing ##

Patches gladly accepted. Main TODO items:

- Better fonts for the text elements
- Instructions to compile under Windows
- Other frontends

## License ##

Licensed under the GPL (see `LICENSE_GPL.txt`)
