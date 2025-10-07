#   Haskell Battleship Guessing Strategy

This project implements a **logical guessing strategy** for a Battleship-style game on a 4×8 grid using **functional programming in Haskell**.  
The algorithm attempts to **locate three hidden ships** by making strategic guesses and refining its game state based on feedback from previous moves.

---

## Features

-  **Functional implementation** of a guessing strategy without mutable state  
-  Grid-based ship search using **Chebyshev distance** feedback  
-  Intelligent guess selection using **expected value minimization**  
-  Converts between string coordinates and structured types (`toLocation` / `fromLocation`)  
-  Uses pure functions to maintain and update the game state across turns

---

##  How It Works

1. **Initial Guess**  
   The algorithm starts with a hardcoded initial guess designed to provide maximum information.

2. **Feedback Calculation**  
   Each guess is compared to the hidden ship locations, producing a triple `(correct, adjacent, near)`:
   - `0` → exact match (hit)  
   - `1` → adjacent (including diagonals)  
   - `2` → near (2 units away)

3. **Game State Filtering**  
   The algorithm maintains a list of **all possible 3-ship combinations** (4×8 grid → 4960 combinations).  
   After each guess, it filters out any candidates **inconsistent with the feedback**.

4. **Next Guess Selection**  
   Among the remaining candidates, the algorithm chooses the guess that **minimizes the expected number of remaining candidates**, using feedback distribution statistics.

5. **Repeat Until All Ships Are Found** 

---


## 🛠️ Requirements

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)  
- [Cabal](https://www.haskell.org/cabal/) or GHCi for interactive testing

Check installation:
```bash
ghc --version

Compile
ghc Proj2.hs

Run
ghci
Prelude> :load Proj2.hs

