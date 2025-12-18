/*
Module for controlling 7-segment display on Nexys A7 dev board.
Reference (section 9.1):
https://digilent.com/reference/programmable-logic/nexys-a7/reference-manual
*/

module SevenSegmentDisplay(
    input logic clk,
    input logic reset_n,
    input logic [31:0] num,      // number to display
    output logic [7:0] anode_n,  // which anode (digit) to select, active low
    output logic [6:0] seg,      // individual segment signals
    output logic dp              // decimal point
);

    // Clock divider to 1kHz for period of 1ms, assuming input clk is 100MHz
    logic [31:0] counter;
    always_ff @(posedge clk) begin
        if (!reset_n) counter <= 32'd100_000;
        else if (counter == 0) counter <= 32'd100_000;
        else counter <= counter - 1;
    end

    // A slower signal to indicate when the next digit should be illuminated
    logic slow_clk;
    always_ff @(posedge clk) begin
        if (!reset_n) slow_clk <= 0;
        else slow_clk <= (counter == 1);
    end
    
    // Cycle through the anodes for each digit
    always_ff @(posedge clk) begin
        if (!reset_n) begin
            anode_n <= 8'b1111_1111; // turn off all digits
        end else if (slow_clk) begin
            case(anode_n)
                8'b1111_1110: anode_n <= 8'b1111_1101;
                8'b1111_1101: anode_n <= 8'b1111_1011;
                8'b1111_1011: anode_n <= 8'b1111_0111;
                8'b1111_0111: anode_n <= 8'b1110_1111;
                8'b1110_1111: anode_n <= 8'b1101_1111;
                8'b1101_1111: anode_n <= 8'b1011_1111;
                8'b1011_1111: anode_n <= 8'b0111_1111;
                8'b0111_1111: anode_n <= 8'b1111_1110;
                default:      anode_n <= 8'b1111_1110;
            endcase
        end
    end
    
    // Based on what digit is selected, choose a different slice of 
    // the overall number to display
    logic [3:0] digit_to_display;
    always_comb begin
        case(anode_n)
            8'b1111_1110: digit_to_display = num[3:0];
            8'b1111_1101: digit_to_display = num[7:4];
            8'b1111_1011: digit_to_display = num[11:8];
            8'b1111_0111: digit_to_display = num[15:12];
            8'b1110_1111: digit_to_display = num[19:16];
            8'b1101_1111: digit_to_display = num[23:20];
            8'b1011_1111: digit_to_display = num[27:24];
            8'b0111_1111: digit_to_display = num[31:28];
            default:      digit_to_display = 4'd0;
        endcase
    end

    // Generate hexadecimal 7-segment display signal for active digit
    BinaryTo7Segment bin2seg(.num(digit_to_display), .seg(seg));

    // Keep decimal point off for now
    assign dp = 1;
endmodule
