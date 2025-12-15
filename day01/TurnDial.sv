/*
Given a dial position, amount to turn, and direction to turn,
calculate the final position
*/
module TurnDial(
    input  logic direction,
    input  logic [7:0] int_sum,
    input  logic [7:0] dial_pos,
    output logic [7:0] result_pos
);
    always_comb begin
        result_pos = 0;
        if (direction == 0) begin // direction = LEFT
            if (dial_pos < int_sum) begin
                result_pos = 100 - (int_sum - dial_pos);
            end else begin
                result_pos = dial_pos - int_sum;
            end
        end else begin // direction = RIGHT
            if (dial_pos + int_sum > 99) begin
                result_pos = dial_pos + int_sum - 100;
            end else begin
                result_pos = dial_pos + int_sum;
            end
        end
    end
endmodule
