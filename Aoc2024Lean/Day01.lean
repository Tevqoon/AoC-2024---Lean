import Batteries.Data.List.Basic
import Aoc2024Lean.Utils

open Utils List

namespace Day01

def solve1 (input) :=
  let sorted_ids := map List.mergeSort <| transpose <| map parseInts (lines input)
  let left_ids  := get! sorted_ids 0
  let right_ids := get! sorted_ids 1
  zipWith (fun x y => Int.natAbs (x - y)) left_ids right_ids
  |> sum

def test := "3   4
4   3
2   5
1   3
3   9
3   3"

#eval solve1 test

def solve2 (input : String) :=
  let ids := transpose <| map parseInts (lines input)
  let left_ids  := get! ids 0
  let right_ids := get! ids 1
  map (fun x => x * count x right_ids) left_ids
  |> sum

#eval solve2 test

def run : IO Unit := do
  let input ← readInput 1
  let left_ids ←
  IO.println <| solve1 input
  IO.println <| solve2 input

end Day01
