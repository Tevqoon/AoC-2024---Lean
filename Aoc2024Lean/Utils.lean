import Batteries.Data.List.Basic

namespace Utils

section Types

def Grid α := List <| List α

section IO
def readInput (i : Nat) : IO String := do
  let dayStr := if i < 10
                then s!"0{i}"
                else s!"{i}"
  IO.FS.readFile s!"./input/day{dayStr}.txt"

/-- Parse a string into a list of lines -/
def lines (s : String) : List String :=
  s.splitOn "\n" |>.filter (·.length > 0)

/-- Parse a string into a list of list of characters -/
def charGrid (s : String) : Grid Char :=
  lines s
  |> List.map String.toList

/-- Converts a list of list of characters into a list of strings -/
def gridToStrings (l : Grid Char) : List String :=
  List.map String.mk l

/-- Parse a list of words from a string -/
def words (s : String) : List String :=
  s.splitOn " " |>.filter (·.length > 0)

/-- Parse a list of integers from a line string.  -/
def parseInts (s : String) : List Int :=
  words s |>.map String.toInt!

/-- Split a string on empty lines to get groups of lines -/
def groups (s : String) : List (List String) :=
  s.splitOn "\n\n" |>.map lines

section List
open List

/-- A list getter which accepts integer indices, discarting the negatives. -/
def geti? (input : List α) (index : Int) : Option α :=
  if index < 0 then
    none
  else
    List.get? input (Int.toNat index)

/-- An integer range function akin to Python's range -/
def rangei : (start : Int) → (len : Nat) → (step : Int := 1) → List Int
  | _, 0, _ => []
  | s, n+1, step => s :: rangei (s+step) n step

/-- The list of all (sx × sy)-shaped slices of the input grid -/
def slices (sx : Nat) (sy : Nat) (input : Grid α) : List <| Grid α :=
  let nrows := length input
  let ncols := length (head! input)
  product (range <| nrows - sx + 1) (range <| ncols - sy + 1)
  |> map (fun (x, y) =>
    range sx
    |> map (fun i =>
      range sy
      |> filterMap (fun j =>
        do
          let r ← get? input (x + i)
          get? r (y + j))))

/-- Returns the main diagonal of a grid -/
def diag (input : Grid α) : List α :=
  let ncols := length (head! input)
  (range ncols)
  |> filterMap (λ i =>
    do let r ← get? input i
       get? r i)

/-- Computes all the subdiagonals of a grid -/
def diags (input : Grid α) : List <| List α :=
  let nrows := length input
  let ncols := length (head! input)
  (rangei (-ncols + 1) (nrows + ncols - 1))
  |> map (fun ci =>
      (rangei 0 ncols)
      |> filterMap (fun ri =>
        do let r ← geti? input ri
           geti? r (ci + ri)))

/-- Returns a list of the diagonal indices of a grid of given height -/
def diagIndices (ncols : Nat) : List <| Nat × Nat :=
  zip (range ncols) (range ncols)

/-- Returns a list of lists of the indices of all the subdiagonals of a given shape -/
def diagsIndices (nrows ncols : Nat) : List <| List <| Nat × Nat :=
  (rangei (-(ncols) + 1) (nrows + ncols - 1))
  |> map (fun (ci : Int) =>
      (rangei 0 ncols)
      |> filterMap (fun (ri : Int) =>
        do let x := ri
           let y := (ci + ri)
           guard (0 ≤ x && x < nrows)
           guard (0 ≤ y && y < ncols)
           some (Int.natAbs x, Int.natAbs y)
           ))           

end List
