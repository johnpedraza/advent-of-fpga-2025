/*
Synchronize and debounce dev board buttons
*/
module ButtonSyncDebounce(
    input  logic clk,
    input  logic btn_raw,
    output logic btn_clean
);
    // First, synchronize to avoid metastability
    logic btn_sync_1, btn_sync_2;
    always_ff @(posedge clk) begin
        btn_sync_1 <= btn_raw;
        btn_sync_2 <= btn_sync_1;
    end
    
    // Then debounce
    /*
    If output is different from (synchronized) input, start a counter. If
    input is stable for 20ms, copy input to ouptut.
    */
    logic [31:0] counter;
    logic counter_en;
    always_ff @(posedge clk) begin
        if (btn_sync_2 != btn_clean) begin
            if (counter_en) begin
                if (counter == 2_000_000) begin
                    btn_clean <= btn_sync_2;
                    counter_en <= 0;
                end else begin
                    counter <= counter + 1;
                end
            end else begin
                counter <= 0;
                counter_en <= 1;
            end
        end else begin
            counter <= 0;
            counter_en <= 0;
        end
    end
endmodule
