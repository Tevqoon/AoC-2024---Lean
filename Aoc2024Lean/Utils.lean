namespace Utils

def readInput (i : Nat) : IO String := do
  let dayStr := if i < 10
                then s!"0{i}"
                else s!"{i}"
  IO.FS.readFile s!"./input/day{dayStr}.txt"

/-- Parse a string into a list of lines -/
def lines (s : String) : List String :=
  s.splitOn "\n" |>.filter (·.length > 0)

def Grid α := List <| List α

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
