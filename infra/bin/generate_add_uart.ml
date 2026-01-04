open! Core
open! Hardcaml
open! Aof_infra.Add_uart

(* Test RTL generation for UART running sum on 7-Segment Display *)

let generate_add_uart_rtl () =
  let module C = Circuit.With_interface (I) (O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"add_uart_top" (hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let%expect_test "print RTL" =
  generate_add_uart_rtl ();
  [%expect
    {|
    module seven_segment_display (
        num,
        reset_n,
        clock,
        anode_n,
        seg,
        dp
    );

        input [31:0] num;
        input reset_n;
        input clock;
        output [7:0] anode_n;
        output [6:0] seg;
        output dp;

        wire vdd;
        wire [6:0] _82;
        wire [6:0] _80;
        wire [6:0] _78;
        wire [6:0] _76;
        wire [6:0] _74;
        wire [6:0] _72;
        wire [6:0] _70;
        wire [6:0] _68;
        wire [6:0] _66;
        wire [6:0] _64;
        wire [6:0] _62;
        wire [6:0] _60;
        wire [6:0] _58;
        wire [6:0] _56;
        wire [6:0] _54;
        wire [6:0] _52;
        wire [6:0] _50;
        wire [3:0] _48;
        wire [7:0] _46;
        wire [3:0] _47;
        wire [11:0] _44;
        wire [3:0] _45;
        wire [15:0] _42;
        wire [3:0] _43;
        wire [19:0] _40;
        wire [3:0] _41;
        wire [23:0] _38;
        wire [3:0] _39;
        wire [27:0] _36;
        wire [3:0] _37;
        wire [31:0] _3;
        wire [3:0] _35;
        wire _30;
        wire _31;
        wire _32;
        wire _33;
        wire _26;
        wire _25;
        wire _27;
        wire _28;
        wire _29;
        wire _23;
        wire _21;
        wire _19;
        wire [7:0] _17;
        wire _18;
        wire _20;
        wire _22;
        wire _24;
        wire [2:0] _34;
        reg [3:0] _49;
        reg [6:0] _83;
        wire [7:0] _14;
        wire [7:0] _100;
        wire _98;
        wire [6:0] _97;
        wire [7:0] _99;
        wire _96;
        wire [7:0] _101;
        wire _93;
        wire [31:0] _91;
        wire _6;
        wire _13;
        wire _8;
        wire [31:0] _84;
        wire [31:0] _89;
        wire [31:0] _86;
        wire _87;
        wire [31:0] _90;
        wire [31:0] _9;
        reg [31:0] _85;
        wire _92;
        reg _94;
        wire [7:0] _102;
        wire [7:0] _10;
        reg [7:0] _15;
        wire [7:0] _16;
        assign vdd = 1'b1;
        assign _82 = 7'b1111111;
        assign _80 = 7'b0001110;
        assign _78 = 7'b0000110;
        assign _76 = 7'b0100001;
        assign _74 = 7'b1000110;
        assign _72 = 7'b0000011;
        assign _70 = 7'b0001000;
        assign _68 = 7'b0010000;
        assign _66 = 7'b0000000;
        assign _64 = 7'b1111000;
        assign _62 = 7'b0000010;
        assign _60 = 7'b0010010;
        assign _58 = 7'b0011001;
        assign _56 = 7'b0110000;
        assign _54 = 7'b0100100;
        assign _52 = 7'b1111001;
        assign _50 = 7'b1000000;
        assign _48 = _46[7:4];
        assign _46 = _44[11:4];
        assign _47 = _46[3:0];
        assign _44 = _42[15:4];
        assign _45 = _44[3:0];
        assign _42 = _40[19:4];
        assign _43 = _42[3:0];
        assign _40 = _38[23:4];
        assign _41 = _40[3:0];
        assign _38 = _36[27:4];
        assign _39 = _38[3:0];
        assign _36 = _3[31:4];
        assign _37 = _36[3:0];
        assign _3 = num;
        assign _35 = _3[3:0];
        assign _30 = _17[1:1];
        assign _31 = _30 | _26;
        assign _32 = _31 | _19;
        assign _33 = _32 | _23;
        assign _26 = _17[3:3];
        assign _25 = _17[2:2];
        assign _27 = _25 | _26;
        assign _28 = _27 | _21;
        assign _29 = _28 | _23;
        assign _23 = _17[7:7];
        assign _21 = _17[6:6];
        assign _19 = _17[5:5];
        assign _17 = ~ _16;
        assign _18 = _17[4:4];
        assign _20 = _18 | _19;
        assign _22 = _20 | _21;
        assign _24 = _22 | _23;
        assign _34 = { _24,
                       _29,
                       _33 };
        always @* begin
            case (_34)
            0:
                _49 <= _35;
            1:
                _49 <= _37;
            2:
                _49 <= _39;
            3:
                _49 <= _41;
            4:
                _49 <= _43;
            5:
                _49 <= _45;
            6:
                _49 <= _47;
            default:
                _49 <= _48;
            endcase
        end
        always @* begin
            case (_49)
            4'b0000:
                _83 <= _50;
            4'b0001:
                _83 <= _52;
            4'b0010:
                _83 <= _54;
            4'b0011:
                _83 <= _56;
            4'b0100:
                _83 <= _58;
            4'b0101:
                _83 <= _60;
            4'b0110:
                _83 <= _62;
            4'b0111:
                _83 <= _64;
            4'b1000:
                _83 <= _66;
            4'b1001:
                _83 <= _68;
            4'b1010:
                _83 <= _70;
            4'b1011:
                _83 <= _72;
            4'b1100:
                _83 <= _74;
            4'b1101:
                _83 <= _76;
            4'b1110:
                _83 <= _78;
            4'b1111:
                _83 <= _80;
            default:
                _83 <= _82;
            endcase
        end
        assign _14 = 8'b00000000;
        assign _100 = 8'b00000001;
        assign _98 = _15[7:7];
        assign _97 = _15[6:0];
        assign _99 = { _97,
                       _98 };
        assign _96 = _15 == _14;
        assign _101 = _96 ? _100 : _99;
        assign _93 = 1'b0;
        assign _91 = 32'b00000000000000000000000000000001;
        assign _6 = reset_n;
        assign _13 = ~ _6;
        assign _8 = clock;
        assign _84 = 32'b00000000000000011000011010100000;
        assign _89 = _85 - _91;
        assign _86 = 32'b00000000000000000000000000000000;
        assign _87 = _85 == _86;
        assign _90 = _87 ? _84 : _89;
        assign _9 = _90;
        always @(posedge _8) begin
            if (_13)
                _85 <= _84;
            else
                _85 <= _9;
        end
        assign _92 = _85 == _91;
        always @(posedge _8) begin
            if (_13)
                _94 <= _93;
            else
                _94 <= _92;
        end
        assign _102 = _94 ? _101 : _15;
        assign _10 = _102;
        always @(posedge _8) begin
            if (_13)
                _15 <= _14;
            else
                _15 <= _10;
        end
        assign _16 = ~ _15;
        assign anode_n = _16;
        assign seg = _83;
        assign dp = vdd;

    endmodule
    module uart_rx (
        reset_n,
        clock,
        rx,
        rx_byte$valid,
        rx_byte$value
    );

        input reset_n;
        input clock;
        input rx;
        output rx_byte$valid;
        output [7:0] rx_byte$value;

        wire [7:0] _21;
        wire [6:0] _36;
        wire _35;
        wire [7:0] _37;
        wire [6:0] _38;
        wire [7:0] _39;
        wire [7:0] _40;
        wire [6:0] _28;
        wire [7:0] _29;
        wire [6:0] _30;
        wire [7:0] _31;
        wire [7:0] _32;
        reg [7:0] _42;
        wire [7:0] _1;
        reg [7:0] _22;
        wire _85;
        wire gnd;
        wire _86;
        wire [1:0] _15;
        wire [1:0] _82;
        wire [1:0] _73;
        wire [3:0] _78;
        wire [3:0] _43;
        wire [3:0] _47;
        wire [3:0] _48;
        wire [3:0] _49;
        wire [3:0] _46;
        reg [3:0] _50;
        wire [3:0] _3;
        reg [3:0] bit_counter;
        wire _79;
        wire [1:0] _80;
        wire [1:0] _81;
        wire [1:0] _41;
        wire [1:0] _77;
        wire [1:0] _33;
        wire [31:0] _51;
        wire [31:0] _17;
        wire [31:0] _69;
        wire [31:0] _70;
        wire _68;
        wire [31:0] _72;
        wire [31:0] _65;
        wire _34;
        wire [31:0] _67;
        wire [31:0] _61;
        wire [31:0] _19;
        wire _20;
        wire [31:0] _63;
        wire [31:0] _54;
        wire [31:0] _56;
        wire [31:0] _58;
        reg [31:0] _74;
        wire [31:0] _4;
        reg [31:0] sample_counter;
        wire _52;
        wire [1:0] _75;
        wire _6;
        wire _14;
        wire _8;
        wire _10;
        reg _24;
        reg _26;
        wire [1:0] _76;
        reg [1:0] _83;
        wire [1:0] _11;
        (* fsm_encoding="one_hot" *)
        reg [1:0] _16;
        reg _87;
        wire _12;
        assign _21 = 8'b00000000;
        assign _36 = _22[7:1];
        assign _35 = 1'b0;
        assign _37 = { _35,
                       _36 };
        assign _38 = _37[6:0];
        assign _39 = { _26,
                       _38 };
        assign _40 = _34 ? _39 : _22;
        assign _28 = _22[7:1];
        assign _29 = { _35,
                       _28 };
        assign _30 = _29[6:0];
        assign _31 = { _26,
                       _30 };
        assign _32 = _20 ? _31 : _22;
        always @* begin
            case (_16)
            2'b01:
                _42 <= _32;
            2'b10:
                _42 <= _40;
            default:
                _42 <= _22;
            endcase
        end
        assign _1 = _42;
        always @(posedge _8) begin
            if (_14)
                _22 <= _21;
            else
                _22 <= _1;
        end
        assign _85 = 1'b1;
        assign gnd = 1'b0;
        assign _86 = _68 ? _85 : gnd;
        assign _15 = 2'b00;
        assign _82 = _68 ? _15 : _16;
        assign _73 = 2'b11;
        assign _78 = 4'b0110;
        assign _43 = 4'b0000;
        assign _47 = 4'b0001;
        assign _48 = bit_counter + _47;
        assign _49 = _34 ? _48 : bit_counter;
        assign _46 = _20 ? _43 : bit_counter;
        always @* begin
            case (_16)
            2'b01:
                _50 <= _46;
            2'b10:
                _50 <= _49;
            default:
                _50 <= bit_counter;
            endcase
        end
        assign _3 = _50;
        always @(posedge _8) begin
            if (_14)
                bit_counter <= _43;
            else
                bit_counter <= _3;
        end
        assign _79 = bit_counter == _78;
        assign _80 = _79 ? _73 : _16;
        assign _81 = _34 ? _80 : _16;
        assign _41 = 2'b10;
        assign _77 = _20 ? _41 : _16;
        assign _33 = 2'b01;
        assign _51 = 32'b00000000000000000000000110110010;
        assign _17 = 32'b00000000000000000000000000000000;
        assign _69 = 32'b00000000000000000000000000000001;
        assign _70 = sample_counter + _69;
        assign _68 = sample_counter == _19;
        assign _72 = _68 ? _17 : _70;
        assign _65 = sample_counter + _69;
        assign _34 = sample_counter == _19;
        assign _67 = _34 ? _17 : _65;
        assign _61 = sample_counter + _69;
        assign _19 = 32'b00000000000000000000001101100100;
        assign _20 = sample_counter == _19;
        assign _63 = _20 ? _17 : _61;
        assign _54 = sample_counter + _69;
        assign _56 = _52 ? _17 : _54;
        assign _58 = _26 ? _17 : _56;
        always @* begin
            case (_16)
            2'b00:
                _74 <= _58;
            2'b01:
                _74 <= _63;
            2'b10:
                _74 <= _67;
            2'b11:
                _74 <= _72;
            default:
                _74 <= sample_counter;
            endcase
        end
        assign _4 = _74;
        always @(posedge _8) begin
            if (_14)
                sample_counter <= _17;
            else
                sample_counter <= _4;
        end
        assign _52 = sample_counter == _51;
        assign _75 = _52 ? _33 : _16;
        assign _6 = reset_n;
        assign _14 = ~ _6;
        assign _8 = clock;
        assign _10 = rx;
        always @(posedge _8) begin
            if (_14)
                _24 <= _35;
            else
                _24 <= _10;
        end
        always @(posedge _8) begin
            if (_14)
                _26 <= _35;
            else
                _26 <= _24;
        end
        assign _76 = _26 ? _16 : _75;
        always @* begin
            case (_16)
            2'b00:
                _83 <= _76;
            2'b01:
                _83 <= _77;
            2'b10:
                _83 <= _81;
            2'b11:
                _83 <= _82;
            default:
                _83 <= _16;
            endcase
        end
        assign _11 = _83;
        always @(posedge _8) begin
            if (_14)
                _16 <= _15;
            else
                _16 <= _11;
        end
        always @* begin
            case (_16)
            2'b11:
                _87 <= _86;
            default:
                _87 <= gnd;
            endcase
        end
        assign _12 = _87;
        assign rx_byte$valid = _12;
        assign rx_byte$value = _22;

    endmodule
    module add_uart (
        uart_rx,
        reset_n,
        clock,
        anode_n,
        seg,
        dp
    );

        input uart_rx;
        input reset_n;
        input clock;
        output [7:0] anode_n;
        output [6:0] seg;
        output dp;

        wire _17;
        wire [6:0] _18;
        wire _14;
        wire [31:0] _12;
        wire _11;
        wire _4;
        wire [8:0] _13;
        wire [7:0] _20;
        wire [23:0] _19;
        wire [31:0] _21;
        wire [31:0] _22;
        wire [31:0] _5;
        reg [31:0] _15;
        wire _7;
        wire _9;
        wire [15:0] _16;
        wire [7:0] _23;
        assign _17 = _16[15:15];
        assign _18 = _16[14:8];
        assign _14 = _13[0:0];
        assign _12 = 32'b00000000000000000000000000000000;
        assign _11 = ~ _7;
        assign _4 = uart_rx;
        uart_rx
            uart_rx_0
            ( .clock(_9),
              .reset_n(_7),
              .rx(_4),
              .rx_byte$valid(_13[0:0]),
              .rx_byte$value(_13[8:1]) );
        assign _20 = _13[8:1];
        assign _19 = 24'b000000000000000000000000;
        assign _21 = { _19,
                       _20 };
        assign _22 = _15 + _21;
        assign _5 = _22;
        always @(posedge _9) begin
            if (_11)
                _15 <= _12;
            else
                if (_14)
                    _15 <= _5;
        end
        assign _7 = reset_n;
        assign _9 = clock;
        seven_segment_display
            seven_segment_display
            ( .clock(_9),
              .reset_n(_7),
              .num(_15),
              .anode_n(_16[7:0]),
              .seg(_16[14:8]),
              .dp(_16[15:15]) );
        assign _23 = _16[7:0];
        assign anode_n = _23;
        assign seg = _18;
        assign dp = _17;

    endmodule
    module add_uart_top (
        uart_rx,
        reset_n,
        clock,
        anode_n,
        seg,
        dp
    );

        input uart_rx;
        input reset_n;
        input clock;
        output [7:0] anode_n;
        output [6:0] seg;
        output dp;

        wire _11;
        wire [6:0] _12;
        wire _4;
        wire _6;
        wire _8;
        wire [15:0] _10;
        wire [7:0] _13;
        assign _11 = _10[15:15];
        assign _12 = _10[14:8];
        assign _4 = uart_rx;
        assign _6 = reset_n;
        assign _8 = clock;
        add_uart
            add_uart
            ( .clock(_8),
              .reset_n(_6),
              .uart_rx(_4),
              .anode_n(_10[7:0]),
              .seg(_10[14:8]),
              .dp(_10[15:15]) );
        assign _13 = _10[7:0];
        assign anode_n = _13;
        assign seg = _12;
        assign dp = _11;

    endmodule
    |}]
;;
