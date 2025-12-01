# Advent of Code 2025

## Setup

Install Clojure && cljfmt:

```bash
$ brew install clojure/tools/clojure
$ brew install weavejester/brew/cljfmt
```

## Usage

```bash
clojure -M:run --day N --stage N [--test]

Options:
  --day N        Day number for the puzzle
  --stage S      Stage number: 1 or 2 (first puzzle or second)
  --test         Use test input instead of real input
```

## Structure

Each day defines a `v1` and a `v2` function, which take the input string and return the corresponding solution.
