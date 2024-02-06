# Copyright 2020 ETH Zurich and University of Bologna.
# Solderpad Hardware License, Version 0.51, see LICENSE for details.
# SPDX-License-Identifier: SHL-0.51

BENDER ?= bender
VSIM ?= vsim

VLOG_FLAGS += -svinputport=compat
VLOG_FLAGS += -suppress 2583
VLOG_FLAGS += +cover=sbecft

Bender.lock:
	$(BENDER) update

.bender:
	$(BENDER) checkout

compile.tcl: .bender
	$(BENDER) script vsim -t test \
    --vlog-arg="$(VLOG_FLAGS)" \
    > compile.tcl

.PHONY: build test_l0 test_ro

build: compile.tcl
	$(VSIM) -c -do 'source compile.tcl; quit'

test_l0: build
	$(VSIM) snitch_icache_l0_tb -coverage -voptargs='+acc +cover=sbecft' -voptargs=+acc -do "log -r /*; run -all"

test_ro: build
	$(VSIM) snitch_read_only_cache_tb -coverage -voptargs='+acc +cover=sbecft' -voptargs=+acc -do "log -r /*; run -all"

.PHONY: clean

clean:
	rm -f compile.tcl
