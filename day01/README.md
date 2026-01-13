# Day 1

## Solution
Read one input character at a time and keep track of the current direction of
each turn as the digits for that turn are parsed.

Store the two most recent digits encountered for each dial turn. The 
numbers are conveniently represented in decimal, which removes the need for 
explicit division or modulo operations.

Based on the current position, direction, and rotation amount, calculate the 
result position. For part 1, increment the result when this position
lands on zero. For part 2, there is additional logic to account for the dial
passing zero (potentially multiple times in one turn). Instead of incrementing
tick-by-tick, some casing can be done to determine how many times zero was
passed in a turn.

Processing time is linear in the number of puzzle input characters. Overall
performance is limited by UART throughput when running on a real FPGA. 
