import Batteries.Data.List.Basic
import Batteries.Data.String.Matcher
import Aoc2024Lean.Utils

open Utils List

namespace Day04

def count1 (grid : List <| List Char) : Nat :=
  let strlines := gridToStrings grid
  let m := String.Matcher.ofString "XMAS"
  map (fun s => m.findAll (String.toSubstring s) |> Array.size) strlines
  |> sum

def solve1 (input : String) :=
  let rows := charGrid input
  let swor := map reverse rows
  let cols := transpose rows
  let sloc := map reverse cols
  let diag := diags rows
  let gaid := map reverse diag

  -- for the diagonals from right up, we can use the reversed rows
  let ddag := diags swor
  let gadd := map reverse ddag
  [rows, swor, cols, sloc, diag, gaid, ddag, gadd]
  |> map count1
  |> sum

def test1 := "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"

#eval solve1 test1

def x_mas (pat : String) (block : Grid Char) :=
  let d1 := diag block
  let d2 := reverse d1
  let d3 := diag <| map reverse block
  let d4 := reverse d3

  let p1 := (String.mk <| d1) = pat
  let p2 := (String.mk <| d2) = pat
  let p3 := (String.mk <| d3) = pat
  let p4 := (String.mk <| d4) = pat

  (p1 || p2) && (p3 || p4)

def solve2 (input : String) :=
  charGrid input
  |> slices 3 3
  |> filter (x_mas "MAS")
  |> length

def test2 := ".M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
.........."


#eval solve2 test2

def run : IO Unit := do
  let input ← readInput 4
  IO.println <| solve1 input
  IO.println <| solve2 input

end Day04
