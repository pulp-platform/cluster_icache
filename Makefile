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
	echo "return 0" >> compile.tcl

.PHONY: build test_l0 test_ro test_l0_nogui test_ro_nogui

build: compile.tcl
	$(VSIM) -c -do 'quit -code [source compile.tcl]'

test_l0: build
	$(VSIM) snitch_icache_l0_tb -coverage -voptargs='+acc +cover=sbecft' -do "log -r /*; run -all"

test_ro: build
	$(VSIM) snitch_read_only_cache_tb -coverage -voptargs='+acc +cover=sbecft' -do "log -r /*; run -all"

test_l0_nogui: build
	$(VSIM) -c snitch_icache_l0_tb -coverage -voptargs='+cover=sbecft' -do "run -all"

test_ro_nogui: build
	$(VSIM) -c snitch_read_only_cache_tb -coverage -voptargs='+cover=sbecft' -do "run -all"

.PHONY: clean

clean:
	rm -f compile.tcl
	rm -f modelsim.ini
	rm -f transcript
	rm -f vsim.wlf
	rm -rf work
