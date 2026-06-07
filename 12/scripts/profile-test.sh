#!/usr/bin/env bash
set -euo pipefail

# Builds and runs the Cabal test suite with GHC profiling enabled.
# Usage examples:
#   scripts/profile-test.sh
#   scripts/profile-test.sh --heap
#   scripts/profile-test.sh --heap --max-heap 2G

TEST_NAME="test"

usage() {
  cat <<'USAGE'
Usage: scripts/profile-test.sh [--heap] [--top] [--xc] [--max-heap SIZE] [--] [tasty args...]

Builds with profiling and runs the Cabal test suite with RTS profiling options.

Options:
  --heap          Generate heap profile (test.hp) in addition to test.prof.
  --top           Use -fprof-auto-top instead of -fprof-auto.
  --xc            Run with +RTS -xc for exception stack traces.
  --max-heap SIZE Stop via GHC RTS before OS OOM kill, e.g. 1G, 2048M, 500m.
  -h, --help      Show this help.

Any remaining args after -- are passed to the test executable.
USAGE
}

heap=0
xc=0
max_heap=""
prof_auto="-fprof-auto"
test_args=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --heap) heap=1; shift ;;
    --top) prof_auto="-fprof-auto-top"; shift ;;
    --xc) xc=1; shift ;;
    --max-heap)
      if [[ $# -lt 2 ]]; then
        echo "Missing value for --max-heap" >&2
        exit 2
      fi
      max_heap="$2"
      shift 2
      ;;
    -h|--help) usage; exit 0 ;;
    --) shift; test_args+=("$@"); break ;;
    *) test_args+=("$1"); shift ;;
  esac
done

CABAL_DIR=$PWD/.cabal XDG_CACHE_HOME=$PWD/.cache XDG_STATE_HOME=$PWD/.state \
  cabal build "test:$TEST_NAME" --enable-profiling --enable-library-profiling \
  --ghc-options="$prof_auto -rtsopts"

rts_args=("+RTS" "-p")
if [[ $heap -eq 1 ]]; then
  rts_args+=("-hc")
fi
if [[ -n "$max_heap" ]]; then
  rts_args+=("-M$max_heap")
fi
if [[ $xc -eq 1 ]]; then
  rts_args+=("-xc")
fi
rts_args+=("-RTS")

CABAL_DIR=$PWD/.cabal XDG_CACHE_HOME=$PWD/.cache XDG_STATE_HOME=$PWD/.state \
  cabal test "$TEST_NAME" --enable-profiling --enable-library-profiling \
  --ghc-options="$prof_auto -rtsopts" \
  --test-options="${test_args[*]} ${rts_args[*]}"

echo "Wrote ${TEST_NAME}.prof" >&2
if [[ $heap -eq 1 ]]; then
  echo "Wrote ${TEST_NAME}.hp" >&2
  echo "Render with: hp2ps -c ${TEST_NAME}.hp && ps2pdf ${TEST_NAME}.ps ${TEST_NAME}.pdf" >&2
fi
