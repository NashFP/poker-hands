# Poker hand evaluator in Fortran

Since my Ocaml solution seemed very mathematical and
just used integers to represent cards, it seemed like
it wasn't that different from how I might have done it
in Fortran years ago, so I made a Fortran version.

## To build:
```bash
make
```

or (using gfortran-13):
```bash
gfortran-13 -o poker poker.f90
```

## To run:
./poker
