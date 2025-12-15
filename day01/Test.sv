/*
Testbench for Day 1
*/
module Test();
    logic clk;
    logic reset_n;
    
    // Puzzle memory interface
    localparam BRAM_WIDTH = 8;
    localparam BRAM_DEPTH = 32768;
    logic [BRAM_WIDTH-1:0] puzzle_input [0:BRAM_DEPTH-1];

    logic [7:0] puzzle_char;
    logic [31:0] puzzle_addr;
    always_ff @(posedge clk) begin
        puzzle_char <= puzzle_input[puzzle_addr];
    end
    
    // Day 1 solution module
    logic [31:0] zero_count_part1;
    logic [31:0] zero_count_part2;
    logic done;
    Day01 dut(.clk(clk),
              .reset_n(reset_n),
              .puzzle_addr(puzzle_addr),
              .puzzle_char(puzzle_char),
              .zero_count_part1(zero_count_part1),
              .zero_count_part2(zero_count_part2),
              .done(done)
    );

    always #5 clk = ~clk;
    initial begin
        $dumpfile("trace_day01.vcd");
        $dumpvars;

        // Read puzzle memory file (see preprocessor.py)
        $readmemh("input/input.mem", puzzle_input);

        $display("Running test for Day 1...\n");

        reset_n = 1;
        #1;
        reset_n = 0;
        repeat (10) @(posedge clk);
        reset_n = 1;

        while (!done) begin
            @(posedge clk);
        end

        $display("Part 1 password: %d\n", zero_count_part1);
        $display("Part 2 password: %d\n", zero_count_part2);
        
        $finish;
    end
endmodule
