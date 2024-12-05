import Batteries.Data.List.Basic
import Aoc2024Lean.Utils

open Utils List

namespace Day05

def test := "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"

def Cmp := Nat → Nat → Bool

def parseReqs (input : List String) : Cmp :=
  let table :=
    input
    |> map (String.splitOn · "|")
    |> map (map String.toNat!)
    |> filterMap toPair?

  λ a b => ((a, b) ∈ table)

def parseData (input : List String) : Grid Nat :=
  input
  |> map (String.splitOn · ",")
  |> map (map String.toNat!)

def solve1 (reqs : Cmp) (data : Grid Nat) :=
  data
  |> filter (λ line =>
    line = mergeSort line reqs)
  |> filterMap middle
  |> sum

def testreqs := (groups <| test)[0]! |> parseReqs
def testdata := (groups <| test)[1]! |> parseData

#eval solve1 testreqs testdata

def solve2 (reqs : Cmp) (data : Grid Nat) :=
  data
  |> filter (λ line =>
    line ≠ mergeSort line reqs)
  |> map (mergeSort · reqs)
  |> filterMap middle
  |> sum

#eval solve2 testreqs testdata

def run : IO Unit := do
  let input ← groups <$> readInput 5
  let reqs := parseReqs <| input[0]!
  let data := parseData <| input[1]!
  
  IO.println <| solve1 reqs data
  IO.println <| solve2 reqs data

end Day05
