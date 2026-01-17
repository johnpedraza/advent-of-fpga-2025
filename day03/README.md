# Day 3

## Solution (Part 1 & 2)
Decide how many batteries may be turned on at once.

For each line, maintain a working set of which batteries have been switched on
so far.

At the start of each line, turn on the first N batteries.

For each digit after the first N batteries, compute N possibilities in parallel, where
each possibility is what would happen if you replaced one of the currently ON batteries
with the new one. Of those possibilities, choose the greatest one and either
turn OFF an old battery and turn the new one ON, or leave the new battery OFF 
and keep the current ON set unchanged.

