// Copyright 2020 ETH Zurich and University of Bologna.
// Solderpad Hardware License, Version 0.51, see LICENSE for details.
// SPDX-License-Identifier: SHL-0.51

// Fabian Schuiki <fschuiki@iis.ee.ethz.ch>
// Florian Zaruba <zarubaf@iis.ee.ethz.ch>

`include "common_cells/registers.svh"

// Translate register interface to refill requests.
// Used for bypassable accesses.
module l0_to_bypass #(
  parameter snitch_icache_pkg::config_t CFG = '0
) (
  input  logic                clk_i,
  input  logic                rst_ni,

  input  logic [CFG.NR_FETCH_PORTS-1:0]                   in_valid_i,
  output logic [CFG.NR_FETCH_PORTS-1:0]                   in_ready_o,
  input  logic [CFG.NR_FETCH_PORTS-1:0][CFG.FETCH_AW-1:0] in_addr_i,
  output logic [CFG.NR_FETCH_PORTS-1:0][CFG.FETCH_DW-1:0] in_data_o,
  output logic [CFG.NR_FETCH_PORTS-1:0]                   in_error_o,

  output logic [CFG.FETCH_AW-1:0] refill_req_addr_o,
  output logic                    refill_req_bypass_o,
  output logic                    refill_req_valid_o,
  input  logic                    refill_req_ready_i,

  input  logic [CFG.LINE_WIDTH-1:0] refill_rsp_data_i,
  input  logic                      refill_rsp_error_i,
  input  logic                      refill_rsp_valid_i,
  output logic                      refill_rsp_ready_o
);

  assign refill_req_bypass_o = 1'b1;

  logic [CFG.NR_FETCH_PORTS-1:0] in_valid;
  logic [CFG.NR_FETCH_PORTS-1:0] in_ready;

  typedef enum logic [1:0] {
    Idle, RequestData, WaitResponse, PresentResponse
  } state_e;
  state_e [CFG.NR_FETCH_PORTS-1:0] state_d , state_q;

  // Mask address so that it is aligned to the cache-line width.
  logic [CFG.NR_FETCH_PORTS-1:0][CFG.FETCH_AW-1:0] in_addr_masked;
  for (genvar i = 0; i < CFG.NR_FETCH_PORTS; i++) begin : gen_masked_addr
    assign in_addr_masked[i] = {in_addr_i[i][CFG.FETCH_AW-1:CFG.LINE_ALIGN],
                                {CFG.LINE_ALIGN{1'b0}}};
  end
  stream_arbiter #(
    .DATA_T ( logic [CFG.FETCH_AW-1:0] ),
    .N_INP  ( CFG.NR_FETCH_PORTS )
  ) i_stream_arbiter (
    .clk_i,
    .rst_ni,
    .inp_data_i  ( in_addr_masked     ),
    .inp_valid_i ( in_valid           ),
    .inp_ready_o ( in_ready           ),
    .oup_data_o  ( refill_req_addr_o  ),
    .oup_valid_o ( refill_req_valid_o ),
    .oup_ready_i ( refill_req_ready_i )
  );

  localparam int unsigned NrFetchPortsBin =
    CFG.NR_FETCH_PORTS == 1 ? 1 : $clog2(CFG.NR_FETCH_PORTS);

  logic [CFG.NR_FETCH_PORTS-1:0] rsp_fifo_mux;
  logic [NrFetchPortsBin-1:0] onehot_mux;
  logic [CFG.NR_FETCH_PORTS-1:0] rsp_fifo_pop;
  logic rsp_fifo_full;

  logic [CFG.NR_FETCH_PORTS-1:0] rsp_valid;
  logic [CFG.NR_FETCH_PORTS-1:0] rsp_ready;

  fifo_v3 #(
    .DATA_WIDTH ( CFG.NR_FETCH_PORTS ),
    .DEPTH      ( 4              )
  ) rsp_fifo (
    .clk_i,
    .rst_ni,
    .flush_i    ( 1'b0                  ),
    .testmode_i ( 1'b0                  ),
    .full_o     ( rsp_fifo_full         ),
    .empty_o    (                       ),
    .usage_o    (                       ),
    .data_i     ( {in_valid & in_ready} ),
    .push_i     ( |{in_valid & in_ready}),
    .data_o     ( rsp_fifo_mux          ),
    .pop_i      ( |rsp_fifo_pop         )
  );


  onehot_to_bin #(
    .ONEHOT_WIDTH (CFG.NR_FETCH_PORTS)
  ) i_onehot_to_bin (
    .onehot (rsp_fifo_mux),
    .bin (onehot_mux)
  );

  assign rsp_ready = '1;

  stream_demux #(
    .N_OUP  ( CFG.NR_FETCH_PORTS     )
  ) i_stream_mux_miss_refill (
    .inp_valid_i ( refill_rsp_valid_i ),
    .inp_ready_o ( refill_rsp_ready_o ),
    .oup_sel_i   ( onehot_mux ),
    .oup_valid_o ( rsp_valid ),
    .oup_ready_i ( rsp_ready )
  );

  for (genvar i = 0; i < CFG.NR_FETCH_PORTS; i++) begin : gen_bypass_request
    always_comb begin
      state_d[i] = state_q[i];
      in_ready_o[i] = 1'b0;
      rsp_fifo_pop[i] = 1'b0;
      in_valid[i] = 1'b0;
      unique case (state_q[i])
        // latch data when idle
        Idle: if (in_valid_i[i]) state_d[i] = RequestData;
        RequestData: begin
          // check that there is still space for the response to be accepted.
          if (!rsp_fifo_full) begin
            in_valid[i] = 1'b1;
            if (in_ready[i]) state_d[i] = WaitResponse;
          end
        end
        WaitResponse: begin
          if (rsp_valid[i]) begin
            rsp_fifo_pop[i] = 1'b1;
            state_d[i] = PresentResponse;
          end
        end
        // The response will be served from the register and is valid for one cycle.
        PresentResponse: begin
          state_d[i] = Idle;
          in_ready_o[i] = 1'b1;
        end
        default:;
      endcase
    end
    logic [CFG.FILL_DW-1:0] fill_rsp_data;
    assign fill_rsp_data =
      refill_rsp_data_i >> (in_addr_i[i][CFG.LINE_ALIGN-1:CFG.FETCH_ALIGN] * CFG.FETCH_DW);
    `FFLNR({in_data_o[i], in_error_o[i]}, {fill_rsp_data[CFG.FETCH_DW-1:0], refill_rsp_error_i},
            rsp_valid[i], clk_i)
  end

  `FF(state_q, state_d, '{default: Idle})

endmodule
