# Advent of FPGA 2025
This is my attempt at implementing solutions to this year's
[Advent of Code](https://adventofcode.com/) puzzles on an FPGA.
If you'd like to try it out yourself, see Jane Street's
[Advent of FPGA challenge](https://blog.janestreet.com/advent-of-fpga-challenge-2025/).

# Implementation
## Hardware
I'm targeting the Nexys A7 board with the Artix-7 FPGA (Xilinx 7 Series),
part number XC7A100TCSG324-1.

## Preprocessing
For each of the puzzles, I first preprocess the puzzle input into a memory file
to be read by `$readmemh` (SystemVerilog) and loaded into Block RAM. 
The preprocessing is minimal, converting the .txt file into a .mem
file containing space-separated hexadecimal bytes, one for each ASCII
character in the puzzle input (including newlines). I add a
special `0x00` byte to the end of the file to indicate that
the end of the puzzle input has been reached.

# Simulation
I'm using [Verilator](https://www.veripool.org/verilator/) as my simulator
and [Surfer](https://surfer-project.org/) as my waveform viewer.

```
cd dayXX

verilator --binary --trace-vcd Test.sv

./obj_dir/VTest
```

Tracefile available at `dayXX/trace_dayXX.vcd`

# Solutions
Each day's subdirectory has its own README describing my solutions.
