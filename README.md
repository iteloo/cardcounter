# cardcounter
A simple CL program that counts cards and advises on whether to hit or not in a game of
[十點半](https://zh.wikipedia.org/wiki/十點半).

## Usage
The program keeps track of a deck and your hand. There are two modes
  - Hit Mode: card codes result in the corresponding card being added to the hand and removed from the deck
  - Normal Mode: card codes result in the corresponding card being removed from the deck only
While in Hit Mode, the program will advise you on whether to hit or not when a new card is added to the deck.
Normal Mode is used to keep the deck up to date when cards are dealt to other players.  

The program uses a distribution monad to model the probabilistic computation following Oleg Kiselyov's
[lecture](http://flolac.iis.sinica.edu.tw/flolac16/special.html) at FLOLAC'16.

## Run
Make sure you have `stack` installed on your system, then run

    stack build

to install the necessary dependencies and create the executable. Start the CL program with

    stack exec cardcounter-cl

## Available commands
  - `A` Toggle between Hit Mode or Normal Mode
  - `R` Restart Deck
  - `F` Flush Hand
  - `D` Show Deck
  - `H` Show Hand
  - `U` Undo previous command

## Card Codes
  - `1` A
  - `2` 2
  - ...
  - `9` 9
  - `0` 10
  - `j` J
  - `q` Q
  - `k` King
  - `l` Joker
