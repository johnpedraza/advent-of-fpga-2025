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
Each day's subdirectory will have its own README describing my solutions.

