// Copyright 2024 ETH Zurich and University of Bologna.
// Solderpad Hardware License, Version 0.51, see LICENSE for details.
// SPDX-License-Identifier: SHL-0.51

// Michael Rogenmoser <michaero@iis.ee.ethz.ch>

`include "common_cells/registers.svh"

/// Porting from hier-icache:
///   Unsupported: different line width, banks in L1, L0 not fully associative
///   [SH_FETCH_DATA_WIDTH == Cache line width]
///   [SH_NB_BANKS == 1]
///   [PRI_NB_WAYS == L0LineCount] -> here fully associative
///   [SH_CACHE_LINE == PRI_CACHE_LINE]
///   NumFetchPorts = NB_CORES
///   L0LineCount = PRI_CACHE_SIZE/(bytes per line)
///   LineWidth = X_CACHE_LINE * DATA_WIDTH -> Use >= 32*NB_CORES for optimal performance
///   LineCount = SH_CACHE_SIZE/(bytes per line)
///   WayCount = SH_NB_WAYS
///   FetchAddrWidth = FETCH_ADDR_WIDTH
///   FetchDataWidth = PRI_FETCH_DATA_WIDTH
///   AxiAddrWidth = AXI_ADDR
///   AxiDataWidth = AXI_DATA
module obi_icache_wrap import snitch_icache_pkg::*; #(
  /// Number of request (fetch) ports
  parameter int NumFetchPorts = -1,
  /// L0 Cache Line Count
  parameter int L0LineCount = -1,
  /// Cache Line Width
  /// For optimal performance, use >= 32*NumFetchPorts to allow execution of 32-bit instructions
  /// for each core before requiring another L0-L1 fetch.
  parameter int LineWidth = -1,
  /// The number of cache lines per way. Power of two; >= 2.
  parameter int LineCount = -1,
  /// The set associativity of the cache. Power of two; >= 1.
  parameter int WayCount = 1,
  /// Fetch interface address width. Same as FILL_AW; >= 1.
  parameter int FetchAddrWidth = -1,
  /// Fetch interface data width. Power of two; >= 8.
  parameter int FetchDataWidth = -1,
  /// Fill interface address width. Same as FetchAddrWidth; >= 1.
  parameter int AxiAddrWidth = -1,
  /// Fill interface data width. Power of two; >= 8.
  parameter int AxiDataWidth = -1,
  /// Allow fetches to have priority over prefetches for L0 to L1
  parameter bit FetchPriority = 1'b1,
  /// Merge L0-L1 fetches if requesting the same address
  parameter bit MergeFetches = 1'b1,
  /// Serialize the L1 lookup (parallel tag/data lookup by default)
  parameter bit SerialLookup = 1'b1,
  /// Replace the L1 tag banks with latch-based SCM.
  parameter bit L1TagScm = 1'b1,
  /// Number of pending response beats for the L1 cache.
  parameter int unsigned NumAxiOutstanding = 4,
  /// This reduces area impact at the cost of
  /// increased hassle of having latches in
  /// the design.
  /// i_snitch_icache/gen_prefetcher*i_snitch_icache_l0/data*/Q
  parameter bit EarlyLatch = 1'b0,
  /// Tag width of the data determining logic, this can reduce the
  /// the critical path into the L0 cache when small. The trade-off
  /// is a higher miss-rate in case the smaller tag matches more
  /// tags. The tag must be smaller than the necessary L0 tag.
  /// If configured to `-1` the entire tag is used, effectively
  /// disabling this feature.
  parameter int L0EarlyTagWidth = -1,
  /// Operate L0 cache in slower clock-domain
  parameter bit IsoCrossing      = 1,
  /// Configuration input types for memory cuts used in implementation.
  parameter type sram_cfg_data_t  = logic,
  parameter type sram_cfg_tag_t   = logic,

  parameter type axi_req_t = logic,
  parameter type axi_rsp_t = logic
) (
  input  logic                                         clk_i,
  input  logic                                         rst_ni,

  // Processor interface
  input  logic [NumFetchPorts-1:0]                     fetch_req_i,
  input  logic [NumFetchPorts-1:0][FetchAddrWidth-1:0] fetch_addr_i,
  output logic [NumFetchPorts-1:0]                     fetch_gnt_o,
  output logic [NumFetchPorts-1:0]                     fetch_rvalid_o,
  output logic [NumFetchPorts-1:0][FetchDataWidth-1:0] fetch_rdata_o,
  output logic [NumFetchPorts-1:0]                     fetch_rerror_o,

  input  logic                                         enable_prefetching_i,
  output icache_l0_events_t [NumFetchPorts-1:0]        icache_l0_events_o,
  output icache_l1_events_t                            icache_l1_events_o,
  input  logic [NumFetchPorts-1:0]                     flush_valid_i,
  output logic [NumFetchPorts-1:0]                     flush_ready_o,

  // SRAM configs
  input  sram_cfg_data_t                               sram_cfg_data_i,
  input  sram_cfg_tag_t                                sram_cfg_tag_i,

  // AXI interface
  output axi_req_t                                     axi_req_o,
  input  axi_rsp_t                                     axi_rsp_i
);
  // AdapterType 1 is the only tested variant
  localparam int unsigned AdapterType = 1;

  logic [NumFetchPorts-1:0] fetch_valid, fetch_ready, fetch_rerror;
  logic [NumFetchPorts-1:0][FetchAddrWidth-1:0] fetch_addr;
  logic [NumFetchPorts-1:0][FetchDataWidth-1:0] fetch_rdata;

  for (genvar i = 0; i < NumFetchPorts; i++) begin : gen_adapter
    if (AdapterType == 0) begin : gen_response_cut

      // Reuquires the core to keep data applied steady while req is high, may not be guaranteed...
      spill_register #(
        .T     (logic [FetchDataWidth-1+1:0]),
        .Bypass(1'b0)
      ) i_spill_reg (
        .clk_i,
        .rst_ni,
        .valid_i ( fetch_ready   [i]                     ),
        .ready_o ( /* Unconnected as always ready */     ),
        .data_i  ( {fetch_rdata  [i], fetch_rerror  [i]} ),
        .valid_o ( fetch_rvalid_o[i]                     ),
        .ready_i ( '1                                    ),
        .data_o  ( {fetch_rdata_o[i], fetch_rerror_o[i]} )
      );

      assign fetch_addr[i] = fetch_addr_i[i];
      assign fetch_valid[i] = fetch_req_i[i];
      assign fetch_gnt_o[i] = fetch_ready[i];

    end else if (AdapterType == 1) begin : gen_request_cut

      logic gnt;

      assign fetch_gnt_o[i] = gnt & fetch_req_i[i];

      spill_register #(
        .T     (logic [FetchAddrWidth-1:0]),
        .Bypass(1'b0)
      ) i_spill_reg (
        .clk_i,
        .rst_ni,
        .valid_i ( fetch_req_i [i] ),
        .ready_o ( gnt             ),
        .data_i  ( fetch_addr_i[i] ),
        .valid_o ( fetch_valid [i] ),
        .ready_i ( fetch_ready [i] ),
        .data_o  ( fetch_addr  [i] )
      );

      assign fetch_rdata_o [i] = fetch_rdata [i];
      assign fetch_rerror_o[i] = fetch_rerror[i];
      assign fetch_rvalid_o[i] = fetch_ready [i] & fetch_valid[i];

    end else begin : gen_flexible_cut
      // This can still be improved, there is still an extra stall cycle sometimes AFAIK...

      logic stalled_d, stalled_q;

      logic spill_valid, spill_ready;
      logic [FetchAddrWidth-1:0] spill_addr;

      spill_register #(
        .T     (logic [FetchAddrWidth-1:0]),
        .Bypass(1'b0)
      ) i_req_spill_reg (
        .clk_i,
        .rst_ni,
        .valid_i ( fetch_req_i [i] ),
        .ready_o ( fetch_gnt_o [i] ),
        .data_i  ( fetch_addr_i[i] ),
        .valid_o ( spill_valid     ),
        .ready_i ( spill_ready     ),
        .data_o  ( spill_addr      )
      );

      always_comb begin
        // Keep steady state
        stalled_d = stalled_q;

        // If already stalled
        if (stalled_q) begin
          // only revert back to unstalled state with sufficient gap
          if (!spill_valid && !fetch_req_i[i])
            stalled_d = 1'b0;
        end else begin
          if (fetch_req_i[i] && !fetch_ready[i])
            stalled_d = 1'b1;
        end
      end
      `FF(stalled_q, stalled_d, '0)

      assign fetch_valid[i] = stalled_q ? spill_valid : fetch_req_i[i];
      assign fetch_addr [i] = stalled_q ? spill_addr : fetch_addr_i[i];

      logic spill_rvalid;
      logic spill_rerror;
      logic [FetchDataWidth-1:0] spill_rdata;

      spill_register #(
        .T     (logic [FetchDataWidth-1+1:0]),
        .Bypass(1'b0)
      ) i_rsp_spill_reg (
        .clk_i,
        .rst_ni,
        .valid_i ( fetch_ready [i]                   ),
        .ready_o ( /* Unconnected as always ready */ ),
        .data_i  ( {fetch_rdata[i], fetch_rerror[i]} ),
        .valid_o ( spill_rvalid                      ),
        .ready_i ( '1                                ),
        .data_o  ( {spill_rdata   , spill_rerror   } )
      );

      assign fetch_rvalid_o[i] = stalled_q ? fetch_ready[i] : spill_rvalid;
      assign fetch_rdata_o [i] = stalled_q ? fetch_rdata [i] : spill_rdata;
      assign fetch_rerror_o[i] = stalled_q ? fetch_rerror[i] : spill_rerror;

    end
  end

  snitch_icache #(
    .NR_FETCH_PORTS     ( NumFetchPorts     ),
    .L0_LINE_COUNT      ( L0LineCount       ),
    .LINE_WIDTH         ( LineWidth         ),
    .LINE_COUNT         ( LineCount         ),
    .WAY_COUNT          ( WayCount          ),
    .FETCH_AW           ( FetchAddrWidth    ),
    .FETCH_DW           ( FetchDataWidth    ),
    .FILL_AW            ( AxiAddrWidth      ),
    .FILL_DW            ( AxiDataWidth      ),
    .FETCH_PRIORITY     ( FetchPriority     ),
    .MERGE_FETCHES      ( MergeFetches      ),
    .SERIAL_LOOKUP      ( SerialLookup      ),
    .L1_TAG_SCM         ( L1TagScm          ),
    .NUM_AXI_OUTSTANDING( NumAxiOutstanding ),
    .EARLY_LATCH        ( EarlyLatch        ),
    .L0_EARLY_TAG_WIDTH ( L0EarlyTagWidth   ),
    .ISO_CROSSING       ( IsoCrossing       ),
    .sram_cfg_data_t    ( sram_cfg_data_t   ),
    .sram_cfg_tag_t     ( sram_cfg_tag_t    ),
    .axi_req_t          ( axi_req_t         ),
    .axi_rsp_t          ( axi_rsp_t         )
  ) i_snitch_icache (
    .clk_i,
    .clk_d2_i         ( clk_i                 ),
    .rst_ni,

    .enable_prefetching_i,
    .icache_l0_events_o,
    .icache_l1_events_o,
    .flush_valid_i,
    .flush_ready_o,

    .inst_addr_i      ( fetch_addr            ),
    .inst_data_o      ( fetch_rdata           ),
    .inst_cacheable_i ( {NumFetchPorts{1'b1}} ),
    .inst_valid_i     ( fetch_valid           ),
    .inst_ready_o     ( fetch_ready           ),
    .inst_error_o     ( fetch_rerror          ),

    .sram_cfg_data_i,
    .sram_cfg_tag_i,

    .axi_req_o,
    .axi_rsp_i
  );

endmodule
