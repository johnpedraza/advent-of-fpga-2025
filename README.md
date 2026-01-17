# Advent of FPGA 2025
This is my attempt at implementing solutions to this year's
[Advent of Code](https://adventofcode.com/) puzzles on an FPGA.
If you'd like to try it out yourself, see Jane Street's
[Advent of FPGA challenge](https://blog.janestreet.com/advent-of-fpga-challenge-2025/).

# Implementation
I'm targeting the Nexys A7 board with the Artix-7 FPGA (Xilinx 7 Series),
part number XC7A100TCSG324-1.

For each puzzle, I stream the input as ASCII characters from my desktop 
computer to my FPGA via UART, terminating the transmission with a 0x4 byte 
(EOT).

# Solutions
Each day's subdirectory has its own README describing my solutions.

# Instructions
If you want to run the tests yourself, first install a few things:
- [opam](https://opam.ocaml.org/)
- [The OxCaml Compiler](https://oxcaml.org/get-oxcaml/), also follow the instructions
on that webpage to use the `5.2.0+ox` switch

and some packages:
`opam install hardcaml hardcaml_waveterm ppx_hardcaml core utop dune`

If you then run `dune test`, you can see the results of the expect tests in each
day's `test/test_solution.ml` file. If you'd like to test the full input,
copy it from the Advent of Code website into that day's corresponding
`input/` directory and edit the line in `test_solution.ml` to
target that file instead of the example input.
