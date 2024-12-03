import Batteries.Data.List.Basic
import Aoc2024Lean.Utils
import Aoc2024Lean.Parse

open Utils List Parse

namespace Day03

def test1 := "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
def test2 := "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

def parse_mul : Parser Nat := do
    let _ ← string "mul("
    let a ← natural
    let _ ← char ','
    let b ← natural
    let _ ← char ')'
    pure <| a * b

#eval parse_mul "mul(2,4)"
#eval parse_mul "dsmul(2,4)"

partial def solve1 (input : String) :=
  let rec go acc input :=
    if input.isEmpty then
      reverse acc
    else
    match parse_mul input with
    | none           => go acc (input.drop 1)
    | some (n, rest) => go (n :: acc) rest
  go [] input
  |> sum

#eval solve1 test1

partial def solve2 (input : String) :=
  let rec go acc input go? :=
    if input.isEmpty then
      reverse acc
    else
    if go? then
      match parse_mul input with
      | some (n, rest) => go (n :: acc) rest go?
      | none =>
      match string "don't()" input with
      | none => go acc (input.drop 1) go?
      | some (_, rest) => go acc rest false
    else
    match string "do()" input with
    | none => go acc (input.drop 1) go?
    | some (_, rest) => go acc rest true
  go [] input true
  |> sum

#eval solve2 test2

def run : IO Unit := do
  let input ← readInput 3
  let left_ids ←
  IO.println <| solve1 input
  IO.println <| solve2 input

end Day03
