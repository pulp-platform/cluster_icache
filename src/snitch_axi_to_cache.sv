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

/// Convert from AXI to Cache requests and reconstruct response beats on the way back
/// Adapted from axi_burst_splitter
module snitch_axi_to_cache #(
  // Maximum number of AXI read bursts outstanding at the same time
  parameter int unsigned MaxTrans           = 32'd0,
  // AXI Bus Types
  parameter type         req_t              = logic,
  parameter type         resp_t             = logic,
  parameter snitch_icache_pkg::config_t CFG = '0
)(
  input  logic                      clk_i,
  input  logic                      rst_ni,
  // Cache request
  output logic [CFG.FETCH_AW-1:0]   req_addr_o,
  output logic [CFG.ID_WIDTH-1:0]   req_id_o,
  output logic                      req_valid_o,
  input  logic                      req_ready_i,
  // Cache response
  input  logic [CFG.LINE_WIDTH-1:0] rsp_data_i,
  input  logic                      rsp_error_i,
  input  logic [CFG.ID_WIDTH-1:0]   rsp_id_i,
  input  logic                      rsp_valid_i,
  output logic                      rsp_ready_o,
  // AXI
  input  req_t                      slv_req_i,
  output resp_t                     slv_rsp_o
);

  import cf_math_pkg::idx_width;

  // AXI-word offset within cache line
  localparam int unsigned WordOffset = idx_width(CFG.LINE_WIDTH/CFG.FETCH_DW);

  typedef logic [WordOffset-1:0]   offset_t;
  typedef logic [$clog2(CFG.ID_WIDTH)-1:0] id_t;
  typedef struct packed {
    logic [CFG.FETCH_AW-1:0] addr;
    logic [CFG.ID_WIDTH-1:0] id;
    axi_pkg::len_t           len;
    axi_pkg::burst_t         burst;
  } chan_t;

  // AR Channel
  chan_t         ax_d, ax_q, ax_o;
  offset_t       ar_offset;
  axi_pkg::len_t ar_len;
  logic          cnt_alloc_req, cnt_alloc_gnt;

  // R Channel
  id_t           rsp_id;
  logic          r_cnt_req, r_cnt_gnt;
  logic          r_cnt_len_dec, r_cnt_offset_inc;
  offset_t       r_cnt_offset, r_offset, r_offset_d, r_offset_q;
  axi_pkg::len_t r_cnt_len;

  // --------------------------------------------------
  // Write Channels
  // --------------------------------------------------
  // AW, W, B channel --> Never used tie off
  assign slv_rsp_o.aw_ready = 1'b0;
  assign slv_rsp_o.w_ready = 1'b0;
  assign slv_rsp_o.b_valid = 1'b0;
  assign slv_rsp_o.b = '0;

  // --------------------------------------------------
  // AR Channel
  // --------------------------------------------------
  // Store the offset if the cache is wider than AXI
  assign ar_offset = slv_req_i.ar.addr[$clog2(CFG.FETCH_DW/8)+:WordOffset];
  // Issue less requests if the cache is wider than AXI
  always_comb begin
    if (CFG.LINE_WIDTH > CFG.FETCH_DW) begin
      ar_len = (slv_req_i.ar.len + ar_offset) >> $clog2(CFG.LINE_WIDTH/CFG.FETCH_DW);
    end else begin
      ar_len = slv_req_i.ar.len;
    end
  end

  // Store counters
  axi_burst_splitter_table #(
    .MaxTrans ( MaxTrans             ),
    .IdWidth  ( $clog2(CFG.ID_WIDTH) ),
    .offset_t ( offset_t             )
  ) i_axi_burst_splitter_table (
    .clk_i,
    .rst_ni,
    .alloc_id_i       ( slv_req_i.ar.id  ),
    .alloc_len_i      ( slv_req_i.ar.len ),
    .alloc_offset_i   ( ar_offset        ),
    .alloc_req_i      ( cnt_alloc_req    ),
    .alloc_gnt_o      ( cnt_alloc_gnt    ),
    .cnt_id_i         ( rsp_id           ),
    .cnt_len_o        ( r_cnt_len        ),
    .cnt_offset_o     ( r_cnt_offset     ),
    .cnt_set_err_i    ( 1'b0             ),
    .cnt_err_o        ( /* unused */     ),
    .cnt_len_dec_i    ( r_cnt_len_dec    ),
    .cnt_offset_inc_i ( r_cnt_offset_inc ),
    .cnt_req_i        ( r_cnt_req        ),
    .cnt_gnt_o        ( r_cnt_gnt        )
  );

  assign req_addr_o = ax_o.addr;
  assign req_id_o = 'b1 << ax_o.id;

  typedef enum logic {Idle, Busy} ar_state_e;
  ar_state_e ar_state_d, ar_state_q;
  always_comb begin
    cnt_alloc_req      = 1'b0;
    ax_d               = ax_q;
    ar_state_d         = ar_state_q;
    ax_o               = '0;
    req_valid_o        = 1'b0;
    slv_rsp_o.ar_ready = 1'b0;
    unique case (ar_state_q)
      Idle: begin
        if (slv_req_i.ar_valid && cnt_alloc_gnt) begin
          if (ar_len == '0) begin // No splitting required -> feed through.
            ax_o.addr  = slv_req_i.ar.addr >> CFG.LINE_ALIGN << CFG.LINE_ALIGN;
            ax_o.id    = slv_req_i.ar.id;
            ax_o.len   = ar_len;
            req_valid_o = 1'b1;
            // As soon as downstream is ready, allocate a counter and acknowledge upstream.
            if (req_ready_i) begin
              cnt_alloc_req      = 1'b1;
              slv_rsp_o.ar_ready = 1'b1;
            end
          end else begin // Splitting required.
            // Store Ax, allocate a counter, and acknowledge upstream.
            ax_d.addr          = slv_req_i.ar.addr >> CFG.LINE_ALIGN << CFG.LINE_ALIGN;
            ax_d.id            = slv_req_i.ar.id;
            ax_d.len           = ar_len;
            ax_d.burst         = slv_req_i.ar.burst;
            cnt_alloc_req      = 1'b1;
            slv_rsp_o.ar_ready = 1'b1;
            // Try to feed first burst through.
            ax_o     = ax_d;
            ax_o.len = '0;
            req_valid_o = 1'b1;
            if (req_ready_i) begin
              // Reduce number of bursts still to be sent by one and increment address.
              ax_d.len--;
              if (ax_d.burst == axi_pkg::BURST_INCR) begin
                ax_d.addr += CFG.LINE_WIDTH/8;
              end
            end
            ar_state_d = Busy;
          end
        end
      end
      Busy: begin
        // Sent next burst from split.
        ax_o        = ax_q;
        ax_o.len    = '0;
        req_valid_o = 1'b1;
        if (req_ready_i) begin
          if (ax_q.len == '0) begin
            // If this was the last burst, go back to idle.
            ar_state_d = Idle;
          end else begin
            // Otherwise, continue with the next burst.
            ax_d.len--;
            if (ax_q.burst == axi_pkg::BURST_INCR) begin
              ax_d.addr += CFG.LINE_WIDTH/8;
            end
          end
        end
      end
      default: /*do nothing*/;
    endcase
  end

  // registers
  `FF(ax_q, ax_d, '0, clk_i, rst_ni)
  `FF(ar_state_q, ar_state_d, Idle, clk_i, rst_ni)

  // --------------------------------------------------
  // R Channel
  // --------------------------------------------------
  // Cut path
  typedef struct packed {
    logic [CFG.LINE_WIDTH-1:0] data;
    logic                      error;
    logic [CFG.ID_WIDTH-1:0]   id;
  } rsp_in_t;

  logic rsp_valid_q, rsp_ready_q;
  rsp_in_t rsp_in_d, rsp_in_q;

  assign rsp_in_d.data  = rsp_data_i;
  assign rsp_in_d.error = rsp_error_i;
  assign rsp_in_d.id    = rsp_id_i;

  spill_register #(
    .T      ( rsp_in_t),
    .Bypass ( 1'b0    )
  ) i_cut_rsp_in (
    .clk_i   ( clk_i       ),
    .rst_ni  ( rst_ni      ),
    .valid_i ( rsp_valid_i ),
    .ready_o ( rsp_ready_o ),
    .data_i  ( rsp_in_d    ),
    .valid_o ( rsp_valid_q ),
    .ready_i ( rsp_ready_q ),
    .data_o  ( rsp_in_q    )
  );

  // Reconstruct `id` by splitting cache's ID vector into multiple responses
  // rsp_valid_i --> [forward]                                             --> rsp_valid
  // rsp_ready_o <-- [ready if downstream is ready and input ID is onehot] <-- rsp_ready
  // rsp_id_i    --> [from vector to single ID with lzc]                   --> rsp_id
  // rsp_data_i  --> [forward]                                             --> rsp_in_q.data
  // rsp_error_i --> [forward]                                             --> rsp_in_q.error
  logic rsp_valid, rsp_ready;
  logic rsp_id_onehot, rsp_id_empty;
  logic [CFG.ID_WIDTH-1:0] rsp_id_mask, rsp_id_masked;

  assign rsp_ready_q   = (rsp_id_onehot | rsp_id_empty) & rsp_ready;
  assign rsp_valid     = rsp_valid_q; // And not empty?
  assign rsp_id_masked = rsp_in_q.id & ~rsp_id_mask;

  lzc #(
    .WIDTH ( CFG.ID_WIDTH ),
    .MODE  ( 0            )
  ) i_lzc (
    .in_i    ( rsp_id_masked ),
    .cnt_o   ( rsp_id        ),
    .empty_o ( rsp_id_empty  )
  );

  cc_onehot #(
    .Width ( CFG.ID_WIDTH )
  ) i_onehot (
    .d_i         ( rsp_id_masked ),
    .is_onehot_o ( rsp_id_onehot )
  );

  always_ff @(posedge clk_i or negedge rst_ni) begin
    if(~rst_ni) begin
      rsp_id_mask <= '0;
    end else begin
      if (rsp_valid && rsp_ready) begin
        // Downstream handshake --> Go to next ID
        rsp_id_mask <= rsp_id_mask | (1 << rsp_id);
      end
      if (rsp_valid_q && rsp_ready_q) begin // Or empty, should be redundant
        // Upstream handshake --> Clear mask
        rsp_id_mask <= '0;
      end
    end
  end

  // Reconstruct `last`, feed rest through.
  logic r_last_d, r_last_q;
  logic [CFG.FETCH_DW-1:0] r_data, r_data_d, r_data_q;
  typedef enum logic {RFeedthrough, RWait} r_state_e;
  r_state_e r_state_d, r_state_q;

  // Tie the offset down if cache and AXI width are the same
  assign r_offset  = (CFG.LINE_WIDTH > CFG.FETCH_DW) ? r_cnt_offset : '1;
  // Do not realign data if cache and AXI width are the same
  assign r_data = (CFG.LINE_WIDTH > CFG.FETCH_DW) ?
      rsp_in_q.data >> (r_offset * CFG.FETCH_DW) : rsp_in_q.data;

  always_comb begin
    r_cnt_len_dec     = 1'b0;
    r_cnt_offset_inc  = 1'b0;
    r_cnt_req         = 1'b0;
    r_last_d          = r_last_q;
    r_state_d         = r_state_q;
    r_data_d          = r_data_q;
    r_offset_d        = r_offset_q;
    rsp_ready         = 1'b0;
    slv_rsp_o.r.id    = rsp_id;
    slv_rsp_o.r.data  = r_data;
    slv_rsp_o.r.resp  = rsp_in_q.error; // This response is already an AXI response.
    slv_rsp_o.r.user  = '0;
    slv_rsp_o.r.last  = 1'b0;
    slv_rsp_o.r_valid = 1'b0;

    unique case (r_state_q)
      RFeedthrough: begin
        // If downstream has an R beat and the R counters can give us the remaining length of
        // that burst, ...
        if (rsp_valid) begin
          r_cnt_req = 1'b1;
          if (r_cnt_gnt) begin
            r_last_d = (r_cnt_len == 8'd0);
            slv_rsp_o.r.last   = r_last_d;
            // Decrement the counter
            r_cnt_len_dec      = 1'b1;
            // Increment the offset counter
            r_cnt_offset_inc   = 1'b1;
            // Try to forward the beat upstream
            slv_rsp_o.r_valid  = 1'b1;
            if (slv_req_i.r_ready) begin
              // Is this the last chunk of this cache line
              if (r_offset == '1 || r_last_d) begin
                // Acknowledge downstream
                rsp_ready      = 1'b1;
              end
            end else begin
              // Keep output constant
              r_data_d   = r_data;
              r_offset_d = r_offset;
              // Wait for upstream to become ready
              r_state_d = RWait;
            end
          end
        end
      end
      RWait: begin
        slv_rsp_o.r.last   = r_last_q;
        slv_rsp_o.r_valid  = rsp_valid;
        slv_rsp_o.r.data   = r_data_q;
        if (rsp_valid && slv_req_i.r_ready) begin
          // Is this the last chunk of this cache line
          if (r_offset_q == '1 || r_last_q) begin
            // Acknowledge downstream
            rsp_ready      = 1'b1;
          end
          r_state_d         = RFeedthrough;
        end
      end
      default: /*do nothing*/;
    endcase
  end

  // --------------------------------------------------
  // Flip-Flops
  // --------------------------------------------------
  `FF(r_last_q, r_last_d, 1'b0, clk_i, rst_ni)
  `FF(r_state_q, r_state_d, RFeedthrough, clk_i, rst_ni)
  `FF(r_data_q, r_data_d, '0, clk_i, rst_ni)
  `FF(r_offset_q, r_offset_d, '0, clk_i, rst_ni)

endmodule
