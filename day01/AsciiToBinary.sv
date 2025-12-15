/*
Convert ASCII digits 0-9 to their binary representation
*/
module AsciiToBinary(
    input  logic [7:0] ascii, 
    output logic [7:0] binary
);
    // '0' has hex value 0x30
    assign binary = ascii - 8'h30;
endmodule
