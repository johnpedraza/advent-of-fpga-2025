# Given puzzle input file (txt), generate memory file containing
# space-separated hex bytes.
# The spacing of the numbers does affect how $readmemh puts the data into
# the array

PUZZLE_INPUT = "day01/input/input.txt"
MEM_FILE = "day01/input/input.mem"

with open(PUZZLE_INPUT, 'r') as ifile, open(MEM_FILE, 'w') as memfile:
    for line in ifile:
        for c in line:
            memfile.write(f"{ord(c):02X} ")
        memfile.write('\n')
    memfile.write(f"{0:02X}")
