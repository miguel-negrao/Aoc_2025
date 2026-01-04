# Notes for Codex

- Cabal in this repo may need local state/cache dirs. Use:
  `CABAL_DIR=$PWD/.cabal XDG_CACHE_HOME=$PWD/.cache XDG_STATE_HOME=$PWD/.state cabal <cmd>`
- Profiling helper script: `scripts/profile.sh` (builds with profiling and runs `prog`).
- Keep this file updated with any workflow quirks or commands that should be remembered.
