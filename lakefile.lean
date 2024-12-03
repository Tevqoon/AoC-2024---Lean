import Lake
open Lake DSL

require "leanprover-community" / "batteries" @ git "main"

package "aoc-2024-lean" where
  version := v!"0.1.0"
  leanOptions := #[
    ⟨`pp.unicode.fun, true⟩, -- pretty-prints `fun a ↦ b`
    ⟨`linter.unusedVariables, false⟩
]

lean_lib «Aoc2024Lean» where
  -- add library configuration options here

@[default_target]
lean_exe «aoc» where
  root := `Main
  moreLeanArgs := #["-R", "Aoc=."]


