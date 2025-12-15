/*
* Day 1 solution
*/
module Day01(
    input  logic clk,
    input  logic reset_n,
    input  logic [7:0] puzzle_char,
    output logic [31:0] puzzle_addr,
    output logic [31:0] zero_count_part1,
    output logic [31:0] zero_count_part2,
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
    logic is_newline;
    assign is_newline = puzzle_char == 8'h0A;
    logic is_digit;
    assign is_digit = 'h30 <= puzzle_char && puzzle_char <= 'h39;
    always_ff @(posedge clk, negedge reset_n) begin
        if (!reset_n) begin
            tens_place <= 0;
            ones_place <= 0;
        end else if (is_digit) begin
            tens_place <= ones_place;
            ones_place <= binary_char;
        end else if (is_newline) begin // reset when newline
            tens_place <= 0;
            ones_place <= 0;
        end
    end

    // Keep track of full rotations for part 2
    // This is equal to floor(rotation amount / 100)
    logic [31:0] full_rotations;
    always_ff @(posedge clk) begin
        if (!reset_n) begin
            full_rotations <= 0;
        end else if (is_digit) begin
            full_rotations <= 10 * full_rotations + {24'd0, tens_place};
        end else if (is_newline) begin
            full_rotations <= 0;
        end
    end

    // Intermediate sum (current turn value modulo 100)
    logic [7:0] int_sum;
    assign int_sum = 10 * tens_place + ones_place;
    
    // calculate the result position of turning the dial
    logic [7:0] dial_pos;
    logic [7:0] result_pos;
    TurnDial td(.direction(direction),
                .int_sum(int_sum),
                .dial_pos(dial_pos),
                .result_pos(result_pos)); 

    // number of new zeros for current turn (part 1)
    logic [31:0] new_zeros_part1;
    assign new_zeros_part1 = {31'd0, result_pos == 0};

    // number of new zeros for current turn (part 2)
    logic [31:0] new_zeros_part2;
    always_comb begin
        new_zeros_part2 = 0;

        // number of full rotations
        new_zeros_part2 += full_rotations;

        if (result_pos == 0) begin
            // landing on zero
            new_zeros_part2 += 1;
        end else if (dial_pos != 0) begin
            if ((direction == LEFT && result_pos > dial_pos)
             || (direction == RIGHT && result_pos < dial_pos)) begin
                // passing 0 position (but not part of a full 100 tick rotation) 
                new_zeros_part2 += 1;
            end
        end
    end

    // Update dial position when a newline is reached
    always_ff @(posedge clk, negedge reset_n) begin
        if (!reset_n) begin
            dial_pos <= 50;
        end else if (is_newline) begin
            dial_pos <= result_pos;
        end
    end

    // Update zero counts when a newline is reached
    always_ff @(posedge clk, negedge reset_n) begin
        if (!reset_n) begin
            zero_count_part1 <= 0;
            zero_count_part2 <= 0;
        end else if (is_newline) begin
            zero_count_part1 <= zero_count_part1 + new_zeros_part1;
            zero_count_part2 <= zero_count_part2 + new_zeros_part2;
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
