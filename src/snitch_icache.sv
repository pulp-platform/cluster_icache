// Copyright 2020 ETH Zurich and University of Bologna.
// Solderpad Hardware License, Version 0.51, see LICENSE for details.
// SPDX-License-Identifier: SHL-0.51

// Fabian Schuiki <fschuiki@iis.ee.ethz.ch>
// Florian Zaruba <zarubaf@iis.ee.ethz.ch>

`include "common_cells/registers.svh"

module snitch_icache
  import snitch_icache_pkg::*;
#(
  /// Number of request (fetch) ports
  parameter int unsigned NR_FETCH_PORTS      = -1,
  /// L0 Cache Line Count (L0 is fully associative)
  parameter int unsigned L0_LINE_COUNT       = -1,
  /// Cache Line Width
  parameter int unsigned LINE_WIDTH          = -1,
  /// The number of cache lines per way. Power of two; >= 2.
  parameter int unsigned LINE_COUNT          = -1,
  /// The set associativity of the cache. Power of two; >= 1.
  parameter int unsigned WAY_COUNT           = 1,
  /// Fetch interface address width. Same as FILL_AW; >= 1.
  parameter int unsigned FETCH_AW            = -1,
  /// Fetch interface data width. Power of two; >= 8.
  parameter int unsigned FETCH_DW            = -1,
  /// Fill interface address width. Same as FETCH_AW; >= 1.
  parameter int unsigned FILL_AW             = -1,
  /// Fill interface data width. Power of two; >= 8.
  parameter int unsigned FILL_DW             = -1,
  /// Allow fetches to have priority over prefetches for L0 to L1
  parameter bit          FETCH_PRIORITY      = 1'b0,
  /// Merge L0-L1 fetches if requesting the same address
  parameter bit          MERGE_FETCHES       = 1'b0,
  /// Serialize the L1 lookup (parallel tag/data lookup by default)
  parameter bit          SERIAL_LOOKUP       = 0,
  /// Replace the L1 tag banks with latch-based SCM.
  parameter bit          L1_TAG_SCM          = 0,
  /// Replace the L1 data banks with latch-based SCM.
  parameter bit          L1_DATA_SCM         = 0,
  /// Number of pending response beats for the L1 cache.
  parameter int unsigned NUM_AXI_OUTSTANDING = 2,
  /// This reduces area impact at the cost of
  /// increased hassle of having latches in
  /// the design.
  /// i_snitch_icache/gen_prefetcher*i_snitch_icache_l0/data*/Q
  parameter bit          EARLY_LATCH         = 0,
  /// Tag width of the data determining logic, this can reduce the
  /// the critical path into the L0 cache when small. The trade-off
  /// is a higher miss-rate in case the smaller tag matches more
  /// tags. The tag must be smaller than the necessary L0 tag.
  /// If configured to `-1` the entire tag is used, effectively
  /// disabling this feature.
  parameter int          L0_EARLY_TAG_WIDTH  = -1,
  /// Operate L0 cache in slower clock-domain
  parameter bit          ISO_CROSSING        = 1,
  /// Configuration input types for memory cuts used in implementation.
  parameter type         sram_cfg_data_t     = logic,
  parameter type         sram_cfg_tag_t      = logic,
  parameter type         sram_cfg_out_data_t = logic,
  parameter type         sram_cfg_out_tag_t  = logic,

  parameter type axi_req_t = logic,
  parameter type axi_rsp_t = logic
) (
  input logic clk_i,
  input logic clk_d2_i,
  input logic rst_ni,

  input  logic                                   enable_prefetching_i,
  input  logic                                   enable_branch_pred_i,
  output icache_l0_events_t [NR_FETCH_PORTS-1:0] icache_l0_events_o,
  output icache_l1_events_t                      icache_l1_events_o,

  input  logic [NR_FETCH_PORTS-1:0] flush_valid_i,
  output logic [NR_FETCH_PORTS-1:0] flush_ready_o,

  input  logic [NR_FETCH_PORTS-1:0][FETCH_AW-1:0] inst_addr_i,
  output logic [NR_FETCH_PORTS-1:0][FETCH_DW-1:0] inst_data_o,
  input  logic [NR_FETCH_PORTS-1:0]               inst_cacheable_i,
  input  logic [NR_FETCH_PORTS-1:0]               inst_valid_i,
  output logic [NR_FETCH_PORTS-1:0]               inst_ready_o,
  output logic [NR_FETCH_PORTS-1:0]               inst_error_o,

  input  sram_cfg_data_t     sram_cfg_data_i,
  input  sram_cfg_tag_t      sram_cfg_tag_i,
  output sram_cfg_out_data_t sram_cfg_out_data_o,
  output sram_cfg_out_tag_t  sram_cfg_out_tag_o,

  output axi_req_t axi_req_o,
  input  axi_rsp_t axi_rsp_i
);

  // Bundle the parameters up into a proper configuration struct that we can
  // pass to submodules.
  localparam snitch_icache_pkg::config_t CFG = '{
      NR_FETCH_PORTS: NR_FETCH_PORTS,
      LINE_WIDTH: LINE_WIDTH,
      LINE_COUNT: LINE_COUNT,
      L0_LINE_COUNT: L0_LINE_COUNT,
      WAY_COUNT: WAY_COUNT,
      PENDING_COUNT: NUM_AXI_OUTSTANDING,
      FETCH_AW: FETCH_AW,
      FETCH_DW: FETCH_DW,
      FILL_AW: FILL_AW,
      FILL_DW: FILL_DW,
      L1_TAG_SCM: L1_TAG_SCM,
      L1_DATA_SCM: L1_DATA_SCM,
      EARLY_LATCH: EARLY_LATCH,
      BUFFER_LOOKUP: 0,
      GUARANTEE_ORDERING: 0,

      FETCH_ALIGN: $clog2(FETCH_DW / 8),
      FILL_ALIGN: $clog2(FILL_DW / 8),
      LINE_ALIGN: $clog2(LINE_WIDTH / 8),
      COUNT_ALIGN: $clog2(LINE_COUNT),
      WAY_ALIGN: $clog2(WAY_COUNT),
      TAG_WIDTH: FETCH_AW - $clog2(LINE_WIDTH / 8) - $clog2(LINE_COUNT),
      L0_TAG_WIDTH: FETCH_AW - $clog2(LINE_WIDTH / 8),
      L0_EARLY_TAG_WIDTH:
      (
      L0_EARLY_TAG_WIDTH == -1
      ) ?
      FETCH_AW - $clog2(LINE_WIDTH / 8)
      :
      L0_EARLY_TAG_WIDTH,
      ID_WIDTH: 2 * NR_FETCH_PORTS,
      PENDING_IW: $clog2(NUM_AXI_OUTSTANDING)
  };

  // pragma translate_off
`ifndef VERILATOR
  // Check invariants.
  initial begin
    assert (L0_LINE_COUNT > 0);
    assert (LINE_WIDTH > 0);
    assert (LINE_COUNT > 1);
    assert (WAY_COUNT >= 2)
    else $warning("Only >= 2 ways are supported");
    assert (FETCH_AW > 0);
    assert (FETCH_DW > 0);
    assert (FILL_AW > 0);
    assert (FILL_DW > 0);
    assert (CFG.L0_EARLY_TAG_WIDTH <= CFG.L0_TAG_WIDTH);
    assert (FETCH_AW == FILL_AW);
    assert (2 ** $clog2(LINE_WIDTH) == LINE_WIDTH)
    else $fatal(1, "Cache LINE_WIDTH %0d is not a power of two", LINE_WIDTH);
    assert (2 ** $clog2(LINE_COUNT) == LINE_COUNT)
    else $fatal(1, "Cache LINE_COUNT %0d is not a power of two", LINE_COUNT);
    // NOTE(fschuiki): I think the following is not needed
    // assert(2**$clog2(WAY_COUNT) == WAY_COUNT) else $fatal(1, "Cache WAY_COUNT %0d is not a power of two", WAY_COUNT);
    assert (2 ** $clog2(FETCH_DW) == FETCH_DW)
    else $fatal(1, "Cache FETCH_DW %0d is not a power of two", FETCH_DW);
    assert (2 ** $clog2(FILL_DW) == FILL_DW)
    else $fatal(1, "Cache FILL_DW %0d is not a power of two", FILL_DW);
  end
`endif
  // pragma translate_on

  // Instantiate the optional early cache, or bypass it.
  logic [NR_FETCH_PORTS-1:0][  FETCH_AW-1:0] early_addr;
  logic [NR_FETCH_PORTS-1:0][LINE_WIDTH-1:0] early_data;
  logic [NR_FETCH_PORTS-1:0]                 early_valid;
  logic [NR_FETCH_PORTS-1:0]                 early_ready;
  logic [NR_FETCH_PORTS-1:0]                 early_error;

  // The prefetch module is responsible for taking the 1-channel valid/ready
  // transaction from the early cache and translate it into a 2-channel
  // transaction. Once the actual incoming request has been accepted on the
  // `req` channel, the prefetcher issues another low-priority request for the
  // next cache line.
  typedef struct packed {
    logic [CFG.FETCH_AW-1:0] addr;
    logic [CFG.ID_WIDTH-1:0] id;
  } prefetch_req_t;

  typedef struct packed {
    logic [CFG.LINE_WIDTH-1:0] data;
    logic                      error;
    logic [CFG.ID_WIDTH-1:0]   id;
  } prefetch_resp_t;

  prefetch_req_t  [NR_FETCH_PORTS-1:0] prefetch_req;
  logic           [NR_FETCH_PORTS-1:0] prefetch_req_valid;
  logic           [NR_FETCH_PORTS-1:0] prefetch_req_ready;

  prefetch_req_t                       prefetch_lookup_req;
  logic                                prefetch_lookup_req_valid;
  logic                                prefetch_lookup_req_ready;

  prefetch_resp_t [NR_FETCH_PORTS-1:0] prefetch_rsp;
  logic           [NR_FETCH_PORTS-1:0] prefetch_rsp_valid;
  logic           [NR_FETCH_PORTS-1:0] prefetch_rsp_ready;

  prefetch_resp_t                      prefetch_lookup_rsp;
  logic                                prefetch_lookup_rsp_valid;
  logic                                prefetch_lookup_rsp_ready;

  typedef struct packed {
    logic [CFG.FETCH_AW-1:0]   addr;
    logic [CFG.PENDING_IW-1:0] id;
    logic                      bypass;
  } miss_refill_req_t;
  miss_refill_req_t handler_req, bypass_req, bypass_req_q, refill_req;
  logic handler_req_valid, bypass_req_valid, bypass_req_valid_q, refill_req_valid;
  logic handler_req_ready, bypass_req_ready, bypass_req_ready_q, refill_req_ready;

  typedef struct packed {
    logic [CFG.LINE_WIDTH-1:0] data;
    logic                      error;
    logic [CFG.PENDING_IW-1:0] id;
    logic                      bypass;
  } miss_refill_rsp_t;
  miss_refill_rsp_t handler_rsp, bypass_rsp, bypass_rsp_q, refill_rsp;
  logic handler_rsp_valid, bypass_rsp_valid, bypass_rsp_valid_q, refill_rsp_valid;
  logic handler_rsp_ready, bypass_rsp_ready, bypass_rsp_ready_q, refill_rsp_ready;

  logic [NR_FETCH_PORTS-1:0][FETCH_DW-1:0] bypass_data;
  logic [NR_FETCH_PORTS-1:0]               bypass_error;
  logic [NR_FETCH_PORTS-1:0]               bypass_valid;
  logic [NR_FETCH_PORTS-1:0]               bypass_ready;
  logic [NR_FETCH_PORTS-1:0][FETCH_AW-1:0] bypass_addr;

  // logic [NR_FETCH_PORTS-1:0]
  logic [NR_FETCH_PORTS-1:0] in_cache_valid, in_bypass_valid;
  logic [NR_FETCH_PORTS-1:0] in_cache_ready, in_bypass_ready;
  logic [NR_FETCH_PORTS-1:0][FETCH_DW-1:0] in_cache_data, in_bypass_data;
  logic [NR_FETCH_PORTS-1:0] in_cache_error, in_bypass_error;

  for (genvar i = 0; i < NR_FETCH_PORTS; i++) begin : gen_prefetcher
    prefetch_req_t                     local_prefetch_req;
    logic                              local_prefetch_req_valid;
    logic                              local_prefetch_req_ready;
    prefetch_resp_t                    local_prefetch_rsp;
    logic                              local_prefetch_rsp_valid;
    logic                              local_prefetch_rsp_ready;
    logic           [CFG.ID_WIDTH-1:0] masked_local_rsp_id;

    assign in_cache_valid[i] = inst_cacheable_i[i] & inst_valid_i[i];
    assign in_bypass_valid[i] = ~inst_cacheable_i[i] & inst_valid_i[i];
    assign inst_ready_o[i] = (inst_cacheable_i[i] & in_cache_ready[i]) |
        (~inst_cacheable_i[i] & in_bypass_ready[i]);
    // multiplex results
    assign {inst_error_o[i], inst_data_o[i]} = ({($bits(
        in_cache_data[i]
    ) + 1) {inst_cacheable_i[i]}} & {in_cache_error[i], in_cache_data[i]}) | (~{($bits(
        in_cache_data[i]
    ) + 1) {inst_cacheable_i[i]}} & {in_bypass_error[i], in_bypass_data[i]});

    // ensure the IDs of responses only have those bits set that are set by the L0 cache that receives them, i.e., bits [2*i+1:2*i]
    assign masked_local_rsp_id = local_prefetch_rsp.id & ('b11 << (2 * i));

    snitch_icache_l0 #(
      .CFG  (CFG),
      .L0_ID(i)
    ) i_snitch_icache_l0 (
      .clk_i          (clk_d2_i),
      .rst_ni,
      .flush_valid_i  (flush_valid_i[i]),
      .enable_prefetching_i,
      .enable_branch_pred_i,
      .icache_events_o ( icache_l0_events_o [i]   ),
      .in_addr_i       ( inst_addr_i        [i]   ),
      .in_data_o       ( in_cache_data      [i]   ),
      .in_error_o      ( in_cache_error     [i]   ),
      .in_valid_i      ( in_cache_valid     [i]   ),
      .in_ready_o      ( in_cache_ready     [i]   ),

      .out_req_addr_o (local_prefetch_req.addr),
      .out_req_id_o   (local_prefetch_req.id),
      .out_req_valid_o(local_prefetch_req_valid),
      .out_req_ready_i(local_prefetch_req_ready),

      .out_rsp_data_i (local_prefetch_rsp.data),
      .out_rsp_error_i(local_prefetch_rsp.error),
      .out_rsp_id_i   (masked_local_rsp_id),
      .out_rsp_valid_i(local_prefetch_rsp_valid),
      .out_rsp_ready_o(local_prefetch_rsp_ready)
    );

    isochronous_spill_register #(
      .T     (prefetch_req_t),
      .Bypass(!ISO_CROSSING)
    ) i_spill_register_prefetch_req (
      .src_clk_i  (clk_d2_i),
      .src_rst_ni (rst_ni),
      .src_valid_i(local_prefetch_req_valid),
      .src_ready_o(local_prefetch_req_ready),
      .src_data_i (local_prefetch_req),
      .dst_clk_i  (clk_i),
      .dst_rst_ni (rst_ni),
      .dst_valid_o(prefetch_req_valid[i]),
      .dst_ready_i(prefetch_req_ready[i]),
      .dst_data_o (prefetch_req[i])
    );

    isochronous_spill_register #(
      .T     (prefetch_resp_t),
      .Bypass(!ISO_CROSSING)
    ) i_spill_register_prefetch_resp (
      .src_clk_i  (clk_i),
      .src_rst_ni (rst_ni),
      .src_valid_i(prefetch_rsp_valid[i]),
      .src_ready_o(prefetch_rsp_ready[i]),
      .src_data_i (prefetch_rsp[i]),
      .dst_clk_i  (clk_d2_i),
      .dst_rst_ni (rst_ni),
      .dst_valid_o(local_prefetch_rsp_valid),
      .dst_ready_i(local_prefetch_rsp_ready),
      .dst_data_o (local_prefetch_rsp)
    );

  end

  l0_to_bypass #(
    .CFG(CFG)
  ) i_l0_to_bypass (
    .clk_i(clk_d2_i),
    .rst_ni,

    .in_valid_i(in_bypass_valid),
    .in_ready_o(in_bypass_ready),
    .in_addr_i (inst_addr_i),
    .in_data_o (in_bypass_data),
    .in_error_o(in_bypass_error),

    .refill_req_addr_o  (bypass_req.addr),
    .refill_req_bypass_o(bypass_req.bypass),
    .refill_req_valid_o (bypass_req_valid),
    .refill_req_ready_i (bypass_req_ready),

    .refill_rsp_data_i (bypass_rsp_q.data),
    .refill_rsp_error_i(bypass_rsp_q.error),
    .refill_rsp_valid_i(bypass_rsp_valid_q),
    .refill_rsp_ready_o(bypass_rsp_ready_q)
  );

  assign bypass_req.id = '0;

  isochronous_spill_register #(
    .T     (miss_refill_req_t),
    .Bypass(!ISO_CROSSING)
  ) i_spill_register_bypass_req (
    .src_clk_i  (clk_d2_i),
    .src_rst_ni (rst_ni),
    .src_valid_i(bypass_req_valid),
    .src_ready_o(bypass_req_ready),
    .src_data_i (bypass_req),
    .dst_clk_i  (clk_i),
    .dst_rst_ni (rst_ni),
    .dst_valid_o(bypass_req_valid_q),
    .dst_ready_i(bypass_req_ready_q),
    .dst_data_o (bypass_req_q)
  );

  isochronous_spill_register #(
    .T     (miss_refill_rsp_t),
    .Bypass(!ISO_CROSSING)
  ) i_spill_register_bypass_resp (
    .src_clk_i  (clk_i),
    .src_rst_ni (rst_ni),
    .src_valid_i(bypass_rsp_valid),
    .src_ready_o(bypass_rsp_ready),
    .src_data_i (bypass_rsp),
    .dst_clk_i  (clk_d2_i),
    .dst_rst_ni (rst_ni),
    .dst_valid_o(bypass_rsp_valid_q),
    .dst_ready_i(bypass_rsp_ready_q),
    .dst_data_o (bypass_rsp_q)
  );

  logic          [NR_FETCH_PORTS-1:0] prefetch_req_ready_tmp;
  prefetch_req_t                      prefetch_lookup_req_tmp;

  /// Arbitrate cache port
  // 1. Request Side
  if (FETCH_PRIORITY) begin : gen_fetch_priority

    logic [NR_FETCH_PORTS-1:0] prefetch_req_priority;
    logic [NR_FETCH_PORTS-1:0] prefetch_req_ready_pre, prefetch_req_ready_fetch;
    prefetch_req_t prefetch_lookup_req_pre, prefetch_lookup_req_fetch;
    logic prefetch_lookup_req_valid_pre, prefetch_lookup_req_valid_fetch;
    logic lock_pre_d, lock_pre_q;
    logic prefetch_filtered_ready, fetch_filtered_ready;


    for (genvar i = 0; i < NR_FETCH_PORTS; i++) begin : gen_prio
      // prioritize fetches over prefetches
      assign prefetch_req_priority[i] = prefetch_req[i].id[2*i];

      assign prefetch_req_ready_tmp[i] = prefetch_req_priority[i] ? prefetch_req_ready_fetch[i] :
          prefetch_req_ready_pre[i];
    end

    assign
        prefetch_lookup_req_valid = prefetch_lookup_req_valid_pre | prefetch_lookup_req_valid_fetch;
    assign prefetch_lookup_req_tmp = prefetch_lookup_req_valid_fetch && !lock_pre_q ?
        prefetch_lookup_req_fetch : prefetch_lookup_req_pre;

    assign lock_pre_d = (lock_pre_q | (~prefetch_lookup_req_valid_fetch &
                                       prefetch_lookup_req_valid_pre)) & ~prefetch_lookup_req_ready;

    // Suppress ready if fetch is valid and if the prefetcher is not locked
    // If merge fetches, still give ready if addresses match
    assign prefetch_filtered_ready = (prefetch_lookup_req_valid_fetch && !lock_pre_q) &&
        !(MERGE_FETCHES && prefetch_lookup_req_pre.addr == prefetch_lookup_req_fetch.addr) ? 1'b0 :
        prefetch_lookup_req_ready;

    // Suppress ready if locked to prefetcher
    // If merge fetches, still give ready if addresses match
    assign fetch_filtered_ready = lock_pre_q &&
        !(MERGE_FETCHES && prefetch_lookup_req_pre.addr == prefetch_lookup_req_fetch.addr) ? 1'b0 :
        prefetch_lookup_req_ready;

    // prefetch arbiter - low priority
    multi_accept_rr_arb #(
      .data_t(prefetch_req_t),
      .NumInp(NR_FETCH_PORTS)
    ) i_stream_arbiter_pre (
      .clk_i,
      .rst_ni,
      .flush_i    ('0),
      .inp_data_i (prefetch_req),
      .inp_valid_i(prefetch_req_valid & ~prefetch_req_priority),
      .inp_ready_o(prefetch_req_ready_pre),
      .oup_data_o (prefetch_lookup_req_pre),
      .oup_valid_o(prefetch_lookup_req_valid_pre),
      .oup_ready_i(prefetch_filtered_ready)
    );

    // fetch arbiter - high priority
    multi_accept_rr_arb #(
      .data_t(prefetch_req_t),
      .NumInp(NR_FETCH_PORTS)
    ) i_stream_arbiter_fetch (
      .clk_i,
      .rst_ni,
      .flush_i    ('0),
      .inp_data_i (prefetch_req),
      .inp_valid_i(prefetch_req_valid & prefetch_req_priority),
      .inp_ready_o(prefetch_req_ready_fetch),
      .oup_data_o (prefetch_lookup_req_fetch),
      .oup_valid_o(prefetch_lookup_req_valid_fetch),
      .oup_ready_i(fetch_filtered_ready)
    );

    `FF(lock_pre_q, lock_pre_d, '0)

  end else begin : gen_standard_fetch

    multi_accept_rr_arb #(
      .data_t(prefetch_req_t),
      .NumInp(NR_FETCH_PORTS)
    ) i_stream_arbiter (
      .clk_i,
      .rst_ni,
      .flush_i    ('0),
      .inp_data_i (prefetch_req),
      .inp_valid_i(prefetch_req_valid),
      .inp_ready_o(prefetch_req_ready_tmp),
      .oup_data_o (prefetch_lookup_req_tmp),
      .oup_valid_o(prefetch_lookup_req_valid),
      .oup_ready_i(prefetch_lookup_req_ready)
    );

  end

  if (MERGE_FETCHES) begin : gen_merge_fetches
    for (genvar i = 0; i < NR_FETCH_PORTS; i++) begin : gen_prefetch_req_ready
      assign prefetch_req_ready[i] = prefetch_req_ready_tmp[i] |
          (prefetch_lookup_req_ready & prefetch_req[i].addr == prefetch_lookup_req.addr);
    end

    always_comb begin
      prefetch_lookup_req = prefetch_lookup_req_tmp;
      for (int i = 0; i < NR_FETCH_PORTS; i++) begin
        prefetch_lookup_req.id |= prefetch_req_ready[i] && prefetch_req_valid[i] ?
            prefetch_req[i].id : '0;
      end
    end
  end else begin : gen_no_merge_fetches
    assign prefetch_req_ready  = prefetch_req_ready_tmp;
    assign prefetch_lookup_req = prefetch_lookup_req_tmp;
  end

  // 2. Response Side
  // This breaks if the pre-fetcher would not alway be ready
  // which is the case for the moment
  for (genvar i = 0; i < NR_FETCH_PORTS; i++) begin : gen_resp
    assign prefetch_rsp[i] = prefetch_lookup_rsp;
    // check if one of the ID bits is set
    assign prefetch_rsp_valid[i] = ((|((prefetch_rsp[i].id >> 2 * i) & 2'b11)) &
                                    prefetch_lookup_rsp_valid);
  end
  assign prefetch_lookup_rsp_ready = |prefetch_rsp_ready;

  /// Tag lookup

  // The lookup module contains the actual cache RAMs and performs lookups.
  logic [   CFG.FETCH_AW-1:0] lookup_addr;
  logic [   CFG.ID_WIDTH-1:0] lookup_id;
  logic [  CFG.WAY_ALIGN-1:0] lookup_way;
  logic                       lookup_hit;
  logic [ CFG.LINE_WIDTH-1:0] lookup_data;
  logic                       lookup_error;
  logic                       lookup_valid;
  logic                       lookup_ready;

  logic [CFG.COUNT_ALIGN-1:0] write_addr;
  logic [  CFG.WAY_ALIGN-1:0] write_way;
  logic [ CFG.LINE_WIDTH-1:0] write_data;
  logic [  CFG.TAG_WIDTH-1:0] write_tag;
  logic                       write_error;
  logic                       write_valid;
  logic                       write_ready;

  logic flush_valid, flush_ready;
  logic flush_valid_lookup, flush_ready_lookup;

  assign flush_ready_o = {CFG.NR_FETCH_PORTS{flush_ready}};
  assign flush_valid   = |flush_valid_i;

  // We need to propagate the handshake into the other
  // clock domain in case we operate w/ different clocks.
  if (ISO_CROSSING) begin : gen_flush_crossing
    isochronous_spill_register i_isochronous_4phase_handshake (
      .src_clk_i  (clk_d2_i),
      .src_rst_ni (rst_ni),
      .src_valid_i(flush_valid),
      .src_ready_o(flush_ready),
      .src_data_i ('0),
      .dst_clk_i  (clk_i),
      .dst_rst_ni (rst_ni),
      .dst_valid_o(flush_valid_lookup),
      .dst_ready_i(flush_ready_lookup),
      .dst_data_o (  /* Unused */)
    );
  end else begin : gen_no_flush_crossing
    assign flush_valid_lookup = flush_valid;
    assign flush_ready        = flush_ready_lookup;
  end

  if (SERIAL_LOOKUP) begin : gen_serial_lookup
    snitch_icache_lookup_serial #(
      .CFG                (CFG),
      .sram_cfg_tag_t     (sram_cfg_tag_t),
      .sram_cfg_data_t    (sram_cfg_data_t),
      .sram_cfg_out_tag_t (sram_cfg_out_tag_t),
      .sram_cfg_out_data_t(sram_cfg_out_data_t)
    ) i_lookup (
      .clk_i,
      .rst_ni,

      .flush_valid_i  (flush_valid_lookup),
      .flush_ready_o  (flush_ready_lookup),
      .icache_events_o(icache_l1_events_o),

      .in_addr_i (prefetch_lookup_req.addr),
      .in_id_i   (prefetch_lookup_req.id),
      .in_valid_i(prefetch_lookup_req_valid),
      .in_ready_o(prefetch_lookup_req_ready),

      .out_addr_o (lookup_addr),
      .out_id_o   (lookup_id),
      .out_way_o  (lookup_way),
      .out_hit_o  (lookup_hit),
      .out_data_o (lookup_data),
      .out_error_o(lookup_error),
      .out_valid_o(lookup_valid),
      .out_ready_i(lookup_ready),

      .write_addr_i (write_addr),
      .write_way_i  (write_way),
      .write_data_i (write_data),
      .write_tag_i  (write_tag),
      .write_error_i(write_error),
      .write_valid_i(write_valid),
      .write_ready_o(write_ready),

      .sram_cfg_tag_i,
      .sram_cfg_data_i,
      .sram_cfg_out_tag_o,
      .sram_cfg_out_data_o
    );

  end else begin : gen_parallel_lookup
    snitch_icache_lookup_parallel #(
      .CFG                (CFG),
      .sram_cfg_tag_t     (sram_cfg_tag_t),
      .sram_cfg_data_t    (sram_cfg_data_t),
      .sram_cfg_out_tag_t (sram_cfg_out_tag_t),
      .sram_cfg_out_data_t(sram_cfg_out_data_t)
    ) i_lookup (
      .clk_i,
      .rst_ni,

      .flush_valid_i  (flush_valid_lookup),
      .flush_ready_o  (flush_ready_lookup),
      .icache_events_o(icache_l1_events_o),

      .in_addr_i (prefetch_lookup_req.addr),
      .in_id_i   (prefetch_lookup_req.id),
      .in_valid_i(prefetch_lookup_req_valid),
      .in_ready_o(prefetch_lookup_req_ready),

      .out_addr_o (lookup_addr),
      .out_id_o   (lookup_id),
      .out_way_o  (lookup_way),
      .out_hit_o  (lookup_hit),
      .out_data_o (lookup_data),
      .out_error_o(lookup_error),
      .out_valid_o(lookup_valid),
      .out_ready_i(lookup_ready),

      .write_addr_i (write_addr),
      .write_way_i  (write_way),
      .write_data_i (write_data),
      .write_tag_i  (write_tag),
      .write_error_i(write_error),
      .write_valid_i(write_valid),
      .write_ready_o(write_ready),

      .sram_cfg_tag_i,
      .sram_cfg_data_i,
      .sram_cfg_out_tag_o,
      .sram_cfg_out_data_o
    );
  end

  // The miss handler module deals with the result of the lookup. It also
  // keeps track of the pending refills and ensures that no redundant memory
  // requests are made. Upon refill completion, it sends a new tag/data item
  // to the lookup module and the received data to the prefetch module.
  snitch_icache_handler #(CFG) i_handler (
    .clk_i,
    .rst_ni,

    .in_req_addr_i (lookup_addr),
    .in_req_id_i   (lookup_id),
    .in_req_way_i  (lookup_way),
    .in_req_hit_i  (lookup_hit),
    .in_req_data_i (lookup_data),
    .in_req_error_i(lookup_error),
    .in_req_valid_i(lookup_valid),
    .in_req_ready_o(lookup_ready),

    .in_rsp_data_o (prefetch_lookup_rsp.data),
    .in_rsp_error_o(prefetch_lookup_rsp.error),
    .in_rsp_id_o   (prefetch_lookup_rsp.id),
    .in_rsp_valid_o(prefetch_lookup_rsp_valid),
    .in_rsp_ready_i(prefetch_lookup_rsp_ready),

    .write_addr_o (write_addr),
    .write_way_o  (write_way),
    .write_data_o (write_data),
    .write_tag_o  (write_tag),
    .write_error_o(write_error),
    .write_valid_o(write_valid),
    .write_ready_i(write_ready),

    .out_req_addr_o (handler_req.addr),
    .out_req_id_o   (handler_req.id),
    .out_req_valid_o(handler_req_valid),
    .out_req_ready_i(handler_req_ready),

    .out_rsp_data_i (handler_rsp.data),
    .out_rsp_error_i(handler_rsp.error),
    .out_rsp_id_i   (handler_rsp.id),
    .out_rsp_valid_i(handler_rsp_valid),
    .out_rsp_ready_o(handler_rsp_ready)
  );
  assign handler_req.bypass = 1'b0;
  // Arbitrate between bypass and cache-refills
  stream_arbiter #(
    .DATA_T(miss_refill_req_t),
    .N_INP (2)
  ) i_stream_arbiter_miss_refill (
    .clk_i,
    .rst_ni,
    .inp_data_i ({bypass_req_q, handler_req}),
    .inp_valid_i({bypass_req_valid_q, handler_req_valid}),
    .inp_ready_o({bypass_req_ready_q, handler_req_ready}),
    .oup_data_o (refill_req),
    .oup_valid_o(refill_req_valid),
    .oup_ready_i(refill_req_ready)
  );
  // Response path muxing
  stream_demux #(
    .N_OUP(2)
  ) i_stream_demux_miss_refill (
    .inp_valid_i(refill_rsp_valid),
    .inp_ready_o(refill_rsp_ready),

    .oup_sel_i(refill_rsp.bypass),

    .oup_valid_o({{bypass_rsp_valid, handler_rsp_valid}}),
    .oup_ready_i({{bypass_rsp_ready, handler_rsp_ready}})
  );

  assign handler_rsp = refill_rsp;
  assign bypass_rsp  = refill_rsp;

  // Instantiate the cache refill module which emits AXI transactions.
  snitch_icache_refill #(
    .CFG      (CFG),
    .axi_req_t(axi_req_t),
    .axi_rsp_t(axi_rsp_t)
  ) i_refill (
    .clk_i,
    .rst_ni,

    .in_req_addr_i  (refill_req.addr),
    .in_req_id_i    (refill_req.id),
    .in_req_bypass_i(refill_req.bypass),
    .in_req_valid_i (refill_req_valid),
    .in_req_ready_o (refill_req_ready),

    .in_rsp_data_o  (refill_rsp.data),
    .in_rsp_error_o (refill_rsp.error),
    .in_rsp_id_o    (refill_rsp.id),
    .in_rsp_bypass_o(refill_rsp.bypass),
    .in_rsp_valid_o (refill_rsp_valid),
    .in_rsp_ready_i (refill_rsp_ready),
    .axi_req_o      (axi_req_o),
    .axi_rsp_i      (axi_rsp_i)
  );

endmodule

// Translate register interface to refill requests.
// Used for bypassable accesses.
module l0_to_bypass #(
  parameter snitch_icache_pkg::config_t CFG = '0
) (
  input logic clk_i,
  input logic rst_ni,

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
    Idle,
    RequestData,
    WaitResponse,
    PresentResponse
  } state_e;
  state_e [CFG.NR_FETCH_PORTS-1:0] state_d, state_q;

  // Mask address so that it is aligned to the cache-line width.
  logic [CFG.NR_FETCH_PORTS-1:0][CFG.FETCH_AW-1:0] in_addr_masked;
  for (genvar i = 0; i < CFG.NR_FETCH_PORTS; i++) begin : gen_masked_addr
    assign in_addr_masked[i] = {
      in_addr_i[i][CFG.FETCH_AW-1:CFG.LINE_ALIGN], {CFG.LINE_ALIGN{1'b0}}
    };
  end
  stream_arbiter #(
    .DATA_T(logic [CFG.FETCH_AW-1:0]),
    .N_INP (CFG.NR_FETCH_PORTS)
  ) i_stream_arbiter (
    .clk_i,
    .rst_ni,
    .inp_data_i (in_addr_masked),
    .inp_valid_i(in_valid),
    .inp_ready_o(in_ready),
    .oup_data_o (refill_req_addr_o),
    .oup_valid_o(refill_req_valid_o),
    .oup_ready_i(refill_req_ready_i)
  );

  localparam int unsigned NrFetchPortsBin = CFG.NR_FETCH_PORTS == 1 ? 1 : $clog2(
      CFG.NR_FETCH_PORTS
  );

  logic [CFG.NR_FETCH_PORTS-1:0] rsp_fifo_mux;
  logic [   NrFetchPortsBin-1:0] onehot_mux;
  logic [CFG.NR_FETCH_PORTS-1:0] rsp_fifo_pop;
  logic                          rsp_fifo_full;

  logic [CFG.NR_FETCH_PORTS-1:0] rsp_valid;
  logic [CFG.NR_FETCH_PORTS-1:0] rsp_ready;

  fifo_v3 #(
    .DATA_WIDTH(CFG.NR_FETCH_PORTS),
    .DEPTH     (4)
  ) rsp_fifo (
    .clk_i,
    .rst_ni,
    .flush_i   (1'b0),
    .testmode_i(1'b0),
    .full_o    (rsp_fifo_full),
    .empty_o   (),
    .usage_o   (),
    .data_i    ({in_valid & in_ready}),
    .push_i    (|{in_valid & in_ready}),
    .data_o    (rsp_fifo_mux),
    .pop_i     (|rsp_fifo_pop)
  );


  onehot_to_bin #(
    .ONEHOT_WIDTH(CFG.NR_FETCH_PORTS)
  ) i_onehot_to_bin (
    .onehot(rsp_fifo_mux),
    .bin   (onehot_mux)
  );

  assign rsp_ready = '1;

  stream_demux #(
    .N_OUP(CFG.NR_FETCH_PORTS)
  ) i_stream_mux_miss_refill (
    .inp_valid_i(refill_rsp_valid_i),
    .inp_ready_o(refill_rsp_ready_o),
    .oup_sel_i  (onehot_mux),
    .oup_valid_o(rsp_valid),
    .oup_ready_i(rsp_ready)
  );

  for (genvar i = 0; i < CFG.NR_FETCH_PORTS; i++) begin : gen_bypass_request
    always_comb begin
      state_d[i]      = state_q[i];
      in_ready_o[i]   = 1'b0;
      rsp_fifo_pop[i] = 1'b0;
      in_valid[i]     = 1'b0;
      unique case (state_q[i])
        // latch data when idle
        Idle:    if (in_valid_i[i]) state_d[i] = RequestData;
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
            state_d[i]      = PresentResponse;
          end
        end
        // The response will be served from the register and is valid for one cycle.
        PresentResponse: begin
          state_d[i]    = Idle;
          in_ready_o[i] = 1'b1;
        end
        default: ;
      endcase
    end
    logic [CFG.FILL_DW-1:0] fill_rsp_data;
    assign fill_rsp_data = refill_rsp_data_i >>
        (in_addr_i[i][CFG.LINE_ALIGN-1:CFG.FETCH_ALIGN] * CFG.FETCH_DW);
    `FFL({in_data_o[i], in_error_o[i]}, {fill_rsp_data[CFG.FETCH_DW-1:0], refill_rsp_error_i},
         rsp_valid[i], '0, clk_i, rst_ni)
  end

  `FF(state_q, state_d, '{default: Idle})

endmodule
