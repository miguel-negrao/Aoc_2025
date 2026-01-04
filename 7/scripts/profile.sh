#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PROG_NAME="prog"

usage() {
  cat <<'USAGE'
Usage: scripts/profile.sh [--heap] [--top] [--] [prog args...]

Builds with profiling and runs the executable with RTS options.

Options:
  --heap     Generate heap profile (prog.hp) in addition to prog.prof.
  --top      Use -fprof-auto-top instead of -fprof-auto.
  -h, --help Show this help.

Any remaining args after -- are passed to the program.
USAGE
}

heap=0
prof_auto="-fprof-auto"
prog_args=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --heap) heap=1; shift ;;
    --top) prof_auto="-fprof-auto-top"; shift ;;
    -h|--help) usage; exit 0 ;;
    --) shift; prog_args+=("$@"); break ;;
    *) prog_args+=("$1"); shift ;;
  esac
done

cd "$ROOT_DIR"

export CABAL_DIR="$ROOT_DIR/.cabal"
export XDG_CACHE_HOME="$ROOT_DIR/.cache"
export XDG_STATE_HOME="$ROOT_DIR/.state"

cabal build "$PROG_NAME" --enable-profiling --enable-library-profiling \
  --ghc-options="$prof_auto -rtsopts"

rts_args=("+RTS" "-p")
if [[ $heap -eq 1 ]]; then
  rts_args+=("-hc")
fi
rts_args+=("-RTS")

cabal run "$PROG_NAME" -- "${prog_args[@]}" "${rts_args[@]}"

echo "Wrote ${PROG_NAME}.prof" >&2
if [[ $heap -eq 1 ]]; then
  echo "Wrote ${PROG_NAME}.hp" >&2
fi
