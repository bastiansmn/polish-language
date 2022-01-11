Projet PF5 2021 : Polish
========================

## Sujet de projet

Voir [projet.pdf](projet.pdf)

## Prérequis à installer

  - ocaml
  - dune et make

## Compilation et lancement

Par défaut, `make` est seulement utilisé pour abréger les commandes `dune` (voir `Makefile` pour plus de détails):

  - `make` sans argument lancera la compilation `dune` de `polish.exe`,
    c'est-à-dire votre programme en code natif.

  - `make clean` pour effacer le répertoire provisoire `_build` 
    produit par `dune` lors de ses compilations.

Enfin pour lancer votre programme: `./run arg1 arg2 ...`

Usage : 
```
Polish : analyse statique d'un mini-langage
usage: 
  /run --reprint path/to/file.p
    Affiche le fichier Polish tel qu'il est vu pas eval.ml
  ./run --eval path/to/file.p
    Execute le programme Polish
  ./run --vars path/to/file.p
    Réalise une analyse statique des variables lues et non-initialisées.
  ./run --simpl path/to/file.p
    Simplifie le programme si il possède des éléments triviaux
```