// Copyright 2021 ETH Zurich and University of Bologna.
// Solderpad Hardware License, Version 0.51, see LICENSE for details.
// SPDX-License-Identifier: SHL-0.51

// Author: Samuel Riedel <sriedel@iis.ee.ethz.ch>
//
// Adapted from the axi_burst_splitter authored by:
//   Andreas Kurth       <akurth@iis.ee.ethz.ch>
//   Florian Zaruba      <zarubaf@iis.ee.ethz.ch>
//   Wolfgang Roenninger <wroennin@iis.ee.ethz.ch>

`include "common_cells/registers.svh"
/// Stores the burst length and the corresponding address offset for the axi_to_cache module.
/// Adapted from axi_burst_splitter_counters
module axi_burst_splitter_table #(
  parameter int unsigned MaxTrans = 0,
  parameter int unsigned IdWidth  = 0,
  parameter type         offset_t = logic,
  parameter type         id_t     = logic [IdWidth-1:0]
) (
  input  logic          clk_i,
  input  logic          rst_ni,

  input  id_t           alloc_id_i,
  input  axi_pkg::len_t alloc_len_i,
  input  offset_t       alloc_offset_i,
  input  logic          alloc_req_i,
  output logic          alloc_gnt_o,

  input  id_t           cnt_id_i,
  output axi_pkg::len_t cnt_len_o,
  output offset_t       cnt_offset_o,
  input  logic          cnt_set_err_i,
  output logic          cnt_err_o,
  input  logic          cnt_len_dec_i,
  input  logic          cnt_offset_inc_i,
  input  logic          cnt_req_i,
  output logic          cnt_gnt_o
);
  localparam int unsigned CntIdxWidth = (MaxTrans > 1) ? $clog2(MaxTrans) : 32'd1;

  typedef logic [CntIdxWidth-1:0]         cnt_idx_t;
  typedef logic [$bits(axi_pkg::len_t):0] cnt_t;

  cnt_idx_t            cnt_free_idx, cnt_r_idx;
  logic [MaxTrans-1:0] cnt_len_dec, cnt_offset_inc, cnt_free, cnt_set, err_d, err_q;
  cnt_t                cnt_len_inp;
  cnt_t [MaxTrans-1:0] cnt_len_oup;
  offset_t                cnt_offset_inp;
  offset_t [MaxTrans-1:0] cnt_offset_oup;
  for (genvar i = 0; i < MaxTrans; i++) begin : gen_cnt
    counter #(
      .WIDTH ( $bits(cnt_t) )
    ) i_cnt_len (
      .clk_i,
      .rst_ni,
      .clear_i    ( 1'b0           ),
      .en_i       ( cnt_len_dec[i] ),
      .load_i     ( cnt_set[i]     ),
      .down_i     ( 1'b1           ),
      .d_i        ( cnt_len_inp    ),
      .q_o        ( cnt_len_oup[i] ),
      .overflow_o (                )  // not used
    );
    counter #(
      .WIDTH ( $bits(offset_t) )
    ) i_cnt_offset (
      .clk_i,
      .rst_ni,
      .clear_i    ( 1'b0              ),
      .en_i       ( cnt_offset_inc[i] ),
      .load_i     ( cnt_set[i]        ),
      .down_i     ( 1'b0              ),
      .d_i        ( cnt_offset_inp    ),
      .q_o        ( cnt_offset_oup[i] ),
      .overflow_o (                   )  // not used
    );
    assign cnt_free[i] = (cnt_len_oup[i] == '0);
  end
  assign cnt_len_inp    = {1'b0, alloc_len_i} + 1;
  assign cnt_offset_inp = alloc_offset_i;

  lzc #(
    .WIDTH ( MaxTrans ),
    .MODE  ( 1'b0     )
  ) i_lzc (
    .in_i    ( cnt_free     ),
    .cnt_o   ( cnt_free_idx ),
    .empty_o (              )
  );

  logic idq_inp_req, idq_inp_gnt;
  logic idq_oup_gnt, idq_oup_valid, idq_oup_pop;
  id_queue #(
    .ID_WIDTH ( $bits(id_t) ),
    .CAPACITY ( MaxTrans    ),
    .FULL_BW  ( 1'b1        ),
    .data_t   ( cnt_idx_t   )
  ) i_idq (
    .clk_i,
    .rst_ni,
    .inp_id_i         ( alloc_id_i    ),
    .inp_data_i       ( cnt_free_idx  ),
    .inp_req_i        ( idq_inp_req   ),
    .inp_gnt_o        ( idq_inp_gnt   ),
    .exists_data_i    ( '0            ),
    .exists_mask_i    ( '0            ),
    .exists_req_i     ( 1'b0          ),
    .exists_o         ( /* unused */  ),
    .exists_gnt_o     ( /* unused */  ),
    .oup_id_i         ( cnt_id_i      ),
    .oup_pop_i        ( idq_oup_pop   ),
    .oup_req_i        ( cnt_req_i     ),
    .oup_data_o       ( cnt_r_idx     ),
    .oup_data_valid_o ( idq_oup_valid ),
    .oup_gnt_o        ( idq_oup_gnt   )
  );
  logic [8:0] read_len;
  assign idq_inp_req  = alloc_req_i & alloc_gnt_o;
  assign alloc_gnt_o  = idq_inp_gnt & |(cnt_free);
  assign cnt_gnt_o    = idq_oup_gnt & idq_oup_valid;
  assign read_len     = cnt_len_oup[cnt_r_idx] - 1;
  assign cnt_len_o    = read_len[7:0];
  assign cnt_offset_o = cnt_offset_oup[cnt_r_idx];
  assign idq_oup_pop  = cnt_req_i & cnt_gnt_o & cnt_len_dec_i & (cnt_len_o == 8'd0);

  always_comb begin
    cnt_len_dec            = '0;
    cnt_len_dec[cnt_r_idx] = cnt_req_i & cnt_gnt_o & cnt_len_dec_i;
  end
  always_comb begin
    cnt_offset_inc            = '0;
    cnt_offset_inc[cnt_r_idx] = cnt_req_i & cnt_gnt_o & cnt_offset_inc_i;
  end
  always_comb begin
    cnt_set               = '0;
    cnt_set[cnt_free_idx] = alloc_req_i & alloc_gnt_o;
  end
  always_comb begin
    err_d     = err_q;
    cnt_err_o = err_q[cnt_r_idx];
    if (cnt_req_i && cnt_gnt_o && cnt_set_err_i) begin
      err_d[cnt_r_idx] = 1'b1;
      cnt_err_o        = 1'b1;
    end
    if (alloc_req_i && alloc_gnt_o) begin
      err_d[cnt_free_idx] = 1'b0;
    end
  end

  // registers
  `FF(err_q, err_d, '0, clk_i, rst_ni)

  `ifndef VERILATOR
  // pragma translate_off
  assume property (@(posedge clk_i) idq_oup_gnt |-> idq_oup_valid)
    else begin $warning("Invalid output at ID queue, read not granted!"); $finish(); end
  // pragma translate_on
  `endif

endmodule
