/*
* Day 1 solution
*/
module Day01(
    input  logic clk,
    input  logic reset_n,
    input  logic [7:0] puzzle_char,
    output logic [31:0] puzzle_addr,
    output logic [31:0] zero_count,
    output logic done);
    
    // Iterate over all of the characters in puzzle input
    always_ff @(posedge clk, negedge reset_n) begin
        if (!reset_n) begin
            puzzle_addr <= 0;
        end else if (!done) begin
            puzzle_addr <= puzzle_addr + 1;
        end
    end

    // Dial turning direction
    // Update when an 'L' or 'R' is seen
    enum logic {LEFT, RIGHT} direction;
    always_ff @(posedge clk, negedge reset_n) begin
        if (!reset_n) begin
            direction <= LEFT;
        end else begin
            case (puzzle_char)
                8'h4C: direction <= LEFT;
                8'h52: direction <= RIGHT;
                default: direction <= direction;
            endcase
        end
    end
   
    // As the turn amount is read one digit at a time, keep track of the 2 
    // least significant decimal digits since the dial turns are modulo 100
    logic [7:0] ones_place, tens_place;
    logic [7:0] binary_char;
    AsciiToBinary a2b(.ascii(puzzle_char), .binary(binary_char));

    logic is_digit;
    assign is_digit = 'h30 <= puzzle_char && puzzle_char <= 'h39;
    always_ff @(posedge clk, negedge reset_n) begin
        if (!reset_n) begin
            tens_place <= 0;
            ones_place <= 0;
        end else if (is_digit) begin
            tens_place <= ones_place;
            ones_place <= binary_char;
        end else if (puzzle_char == 8'h0A) begin // reset when newline
            tens_place <= 0;
            ones_place <= 0;
        end
    end

    // Intermediate sum (current turn value)
    logic [7:0] int_sum;
    assign int_sum = 10 * tens_place + ones_place;
    
    // Update the dial position and zero count
    logic [7:0] dial_pos;
    logic [7:0] result_pos;
    TurnDial td(.*); // calculate the result position of turning the dial
    always_ff @(posedge clk, negedge reset_n) begin
        if (!reset_n) begin
            dial_pos <= 50;
            zero_count <= 0;
        end else if (puzzle_char == 8'h0A) begin // update dialpos when newline
            dial_pos <= result_pos;
            zero_count <= zero_count + (result_pos == 0 ? 1 : 0);
        end
    end
    
    // Assert done at end of puzzle input
    always_ff @(posedge clk, negedge reset_n) begin
        if (!reset_n) begin
            done <= 0;
        end else begin
            done <= (puzzle_char == 0) ? 1 : 0;
        end
    end
endmodule
