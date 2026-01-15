# Day 4

## Solution (Part 1)
Read the puzzle input one character at a time and pack it into arbitrary-width
(between 2- and 61-bit) words, where each bit is a '1' if the corresponding 
puzzle character was a '@' and '0' otherwise.  After a newline is reached, fill 
the rest of the current word being packed with 0s. Add an additional 
(word width) number of zeros to the end of each row. 

The rest of this README will assume 8-bit words.

So for the example input...
```
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
```

This is the zero padding (using 8-bit words):
```
..@@.@@@@.00000000000000
@@@.@.@.@@00000000000000
@@@@@.@.@@00000000000000
@.@@@@..@.00000000000000
@@.@@@@.@@00000000000000
.@@@@@@@.@00000000000000
.@.@.@.@@@00000000000000
@.@@@.@@@@00000000000000
.@@@@@@@@.00000000000000
@.@.@@@.@.00000000000000
```

And this is how BRAM will look (words separated by |):
```
00110111|10000000|00000000
11101010|11000000|00000000
11111010|11000000|00000000
10111100|10000000|00000000
11011110|11000000|00000000
01111111|01000000|00000000
01010101|11000000|00000000
10111011|11000000|00000000
01111111|10000000|00000000
10101110|10000000|00000000
```

This degree of padding may seem a bit overkill, but:
- For larger inputs, the proportion of BRAM occupied by the padding is much 
smaller
- Packing the chars into bits already saves a lot of BRAM
- This approach simplifies future computation by removing unneeded boundary-checks
- That extra word of all 0s per row isn't needed for the example's case, but if there
was a '1' at the end of the BRAM word immediately before it, it would cause
issues with how I add adjacent shelf positions later on. 

After setting up BRAM:

Read 3 words from 3 adjacent rows (top, middle, and bottom). This provides enough information
to determine the accessibility of rolls in the central 8 positions within those
3 rows. (Remember: this can be much wider, at the cost of more hardware). Keep track of the last 2
bits of each previous row to handle the cases where the addition of adjacent 
shelf locations spans multiple words in BRAM. (These extra 2 bits are
initialized to 0s when processing the start of a row). If the "top" or "bottom"
rows are out of bounds, interpret them as rows of 0s.

This illustrates how the puzzle will be interpreted during processing. Note
how each section is now 8+2 bits wide, since they include the last 2 bits of the
previous section. The first section to be processed is at the top left:
```
        ||||||||||
        vvvvvvvvvv
 top -> 0000000000|0000000000|00000000000
 mid -> 0000110111|1110000000|00000000000
 bot -> 0011101010|1011000000|00000000000
        0011111010|1011000000|00000000000
        0010111100|0010000000|00000000000
        0011011110|1011000000|00000000000
        0001111111|1101000000|00000000000
        0001010101|0111000000|00000000000
        0010111011|1111000000|00000000000
        0001111111|1110000000|00000000000
        0010101110|1010000000|00000000000
        0000000000|0000000000|00000000000
```

After counting the number of accessible rolls in the 8 central positions, 
either read the next 8 columns from BRAM or move down to the start of the next
row.

## Performance
Loading the puzzle input into BRAM is limited by UART throughput. 

Processing the puzzle input from BRAM takes time proportional to
number of characters / word width.
