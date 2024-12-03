import Aoc2024Lean.Day01
import Aoc2024Lean.Day02

def printUsage : IO UInt32 := do
  IO.println "Usage: aoc-2024-lean [day_number]"
  IO.println "Examples:"
  IO.println "  aoc-2024-lean 1    # Run only day 1"
  pure 0

def runDay (day : Nat) : IO UInt32 := do
  match day with
  | 1 => do Day01.run
            pure 0
  | 2 => do Day02.run
            pure 0
  | _ => do IO.println "Invalid day number!"
            pure 1

def main (args : List String) : IO UInt32 := do
  match args with
  | [n] =>
    match n.toNat? with
    | some day => runDay day
    | none =>
      IO.println "Invalid day number!"
      printUsage
  | _ =>
    IO.println "Too many arguments"
    printUsage
