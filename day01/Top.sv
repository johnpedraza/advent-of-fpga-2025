/*
Synthesizable Day 1 Solution
*/

module Top(
    input  logic clk,         // 100MHz clock
    input  logic reset_n_raw, // "CPU Reset" button on dev board
    output logic [7:0] an,    // 7-segment display anode select
    output logic [6:0] seg,   // 7-segment display segments
    output logic dp           // 7-segment display decimal point
);
    localparam BRAM_WIDTH = 8;
    localparam BRAM_DEPTH = 32768;

    /*
    Load puzzle input into BRAM. 8-bit width, 32Kbit depth
    Memory file is preprocessed (see preprocessor.py) into space-separated
    hex bytes.
    */
    logic [BRAM_WIDTH-1:0] puzzle_input [0:BRAM_DEPTH-1];
    initial begin
        $readmemh("input/input.mem", puzzle_input);
    end

    // Memory interface
    // Synchronous read to infer BRAM
    logic [BRAM_WIDTH-1:0] puzzle_char;
    logic [31:0] puzzle_addr;
    always_ff @(posedge clk) begin
        puzzle_char <= puzzle_input[puzzle_addr];
    end

    // Synchronize and debounce reset button
    logic reset_n;
    ButtonSyncDebounce bsd(.clk(clk), 
                           .btn_raw(reset_n_raw), 
                           .btn_clean(reset_n));
    
    // Connect to 7-segment display
    logic [31:0] zero_count;
    SevenSegmentDisplay disp(
        .clk(clk),
        .reset_n(reset_n),
        .num(zero_count),
        .anode_n(an),
        .seg(seg),
        .dp(dp)
    );

    // Day 1 solution module
    logic done;
    Day01 solution(.clk(clk),
                   .reset_n(reset_n),
                   .puzzle_addr(puzzle_addr),
                   .puzzle_char(puzzle_char),
                   .zero_count(zero_count),
                   .done(done));
endmodule
