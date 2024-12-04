import Batteries.Data.List.Basic
import Aoc2024Lean.Utils

open Utils List

namespace Day02

def safe (report : List Int) : Bool :=
  let diffs := zipWith (· - ·) report (tail! report)
  all diffs (fun x => 1 ≤ x && x ≤ 3)
  || all diffs (fun x => (-3 : Int) ≤ x && x ≤ (-1 : Int))

def test := "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"

def solve1 (input : String) :=
  let report := map parseInts (lines input)
  filter safe report
  |> length

#eval solve1 test

def dampener (diffs : List Int) : List (List Int) :=
  map (eraseIdx diffs) (range <| length diffs)

def solve2 (input : String) :=
  let report := map parseInts (lines input)
  filter (fun rep => any (dampener rep) safe) report
  |> length

#eval solve2 test

def run : IO Unit := do
  let input ← readInput 2
  IO.println <| solve1 input
  IO.println <| solve2 input

end Day02
