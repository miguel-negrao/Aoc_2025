#!/bin/bash

hyperfine -w 3 -r 10 ./dist-newstyle/build/x86_64-linux/ghc-9.8.4/aoc-0.1.0.0/x/prog/build/prog/prog
