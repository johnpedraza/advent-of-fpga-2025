# Day 1

## Solution
Read input one character at a time and produce some control signals:
- Current direction
- Is char a digit?
- Is char a newline?
- Done? (end of transmission (0x04))

The datapath reads each digit value and counts how often the dial reaches zero. 

The two most recent digits encountered for each dial turn are stored. The 
numbers are conveniently represented in decimal, which removes the need for 
explicit division or modulo operations to account for the 100-tick dial. 

Processing time is linear in the number of puzzle input characters. Overall
performance is limited by UART throughput when running on a real FPGA. 
