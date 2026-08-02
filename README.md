# tl-aoc-2024

Advent of Code 2024 solutions in [TypeLisp](https://github.com/JoNil-Botta/typelisp).

## Structure

- `src/dayNN.tl` — solution for each day; each exports `(solve-dayNN [path : String]) : unit`
  and has a `(main)` for standalone runs (`typelisp run src/dayNN.tl`)
- `inputs/dayNN.txt` — puzzle inputs (gitignored, fetch with `./fetch_inputs.sh`)
- `run_all.tl` — compiles and runs every day, printing answers and total runtime

## Running

```bash
# one day
typelisp run src/day01.tl

# everything
typelisp run run_all.tl
```

Target: all 25 days together execute in under 2 seconds.
