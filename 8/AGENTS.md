# Notes for Codex

- This is a repository to solve the Advent of Code 2025 challenges.
- The number one rule which must ALWAYS BE RESPECTED is that you shall not in any way suggest a solution or suggest changes to the code that make it closer to the solution. You shall not ever point out a mistake in code.
- You can help with tooling, you can give information about available functions in Haskell when directly asked about them.
- You can create tests.
- The text of the challenge is in aoc.txt.

- Cabal in this repo may need local state/cache dirs. Use:
  `CABAL_DIR=$PWD/.cabal XDG_CACHE_HOME=$PWD/.cache XDG_STATE_HOME=$PWD/.state cabal <cmd>`
- Profiling helper script: `scripts/profile.sh` (builds with profiling and runs `prog`).
- Keep this file updated with any workflow quirks or commands that should be remembered.
