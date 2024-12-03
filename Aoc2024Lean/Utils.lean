namespace Utils

def readInput (i : Nat) : IO String := do
  let dayStr := if i < 10 
                then s!"0{i}" 
                else s!"{i}"
  IO.FS.readFile s!"./input/day{dayStr}.txt"

def lines (s : String) : List String :=
  s.splitOn "\n" |>.filter (·.length > 0)

/-- Parse a list of words from a string -/
def words (s : String) : List String :=
  s.splitOn " " |>.filter (·.length > 0)

/-- Parse a list of integers from a line string.  -/
def parseInts (s : String) : List Int :=
  words s |>.map String.toInt!

/-- Split a string on empty lines to get groups of lines -/
def groups (s : String) : List (List String) :=
  s.splitOn "\n\n" |>.map lines

