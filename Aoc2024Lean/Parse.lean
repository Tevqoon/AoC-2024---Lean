namespace Parse

def Parser (α : Type) := String → Option (α × String)

def parse_pure {α : Type} (x : α) : Parser α :=
  fun input => some (x, input)

def parse_bind {α β : Type} (p : Parser α) (f : α → Parser β) : Parser β :=
  fun input =>
    match p input with
    | none => none
    | some (x, rest) => f x rest

instance : Monad Parser where
  pure := parse_pure
  bind := parse_bind

def fail {α : Type} : Parser α :=
  fun _ => none

def item : Parser Char :=
  fun input =>
    if input.isEmpty then
      none
    else
      some (input.front, input.drop 1)

def satisfy (p : Char → Bool) : Parser Char :=
  fun input =>
    match item input with
    | none => none
    | some (c, rest) =>
      if p c then
        some (c, rest)
      else
        none

-- Parses a single character
def char (c : Char) : Parser Char :=
  satisfy (· = c)

/- Parses some string -/
def string (str : String) : Parser String :=
  fun input =>
    if input.startsWith str then
      some (str, input.drop str.length)
    else
      none

-- Example parsers using the combinators
def digit : Parser Char :=
  satisfy Char.isDigit

def letter : Parser Char :=
  satisfy Char.isAlpha

-- Example: Parse a number
def natural : Parser Nat :=
  fun input =>
    let digits := input.takeWhile Char.isDigit
    if digits.isEmpty then
      none
    else
      some (digits.toNat!, input.drop digits.length)

-- Example usage in do notation
def exampleParser : Parser (Char × Char) := do
  let c1 ← letter
  let _ ← char ','
  let c2 ← digit
  pure (c1, c2)
