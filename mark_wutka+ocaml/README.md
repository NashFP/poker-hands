# Ocaml Poker Hand Evaluator

This solution uses a more mathematical solution, so
it does not define Ocaml types to represent a hand.
Instead, it converts a list of rank values, numbered
from 0-12 into a base-13 number. The final hand value
is basically a base-13 number where the first digit
is the type of the hand, where high card = 0 and
straight_flush = 8.

To run it, just do:
dune exec poker
