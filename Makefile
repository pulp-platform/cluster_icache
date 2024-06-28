# Copyright 2020 ETH Zurich and University of Bologna.
# Solderpad Hardware License, Version 0.51, see LICENSE for details.
# SPDX-License-Identifier: SHL-0.51

BENDER ?= bender
VSIM ?= vsim

VLOG_FLAGS += -svinputport=compat
VLOG_FLAGS += -suppress 2583
VLOG_FLAGS += +cover=sbecft

CTRL_UNIT_DIR = src/ctrl_unit
CTRL_UNIT = $(CTRL_UNIT_DIR)/cluster_icache_ctrl
CTRL_UNIT_PERF = $(CTRL_UNIT_DIR)/cluster_icache_ctrl_perfctr

Bender.lock:
	$(BENDER) update

.bender:
	$(BENDER) checkout

.PHONY: gen_hw
gen_hw: .bender $(CTRL_UNIT)_reg_top.sv $(CTRL_UNIT_PERF)_reg_top.sv

$(CTRL_UNIT)_reg_top.sv: .bender $(CTRL_UNIT).hjson
	python $(shell $(BENDER) path register_interface)/vendor/lowrisc_opentitan/util/regtool.py $(CTRL_UNIT).hjson -t $(CTRL_UNIT_DIR) -r

$(CTRL_UNIT_PERF)_reg_top.sv: .bender $(CTRL_UNIT_PERF).hjson
	python $(shell $(BENDER) path register_interface)/vendor/lowrisc_opentitan/util/regtool.py $(CTRL_UNIT_PERF).hjson -t $(CTRL_UNIT_DIR) -r

compile.tcl: .bender Bender.yml Bender.lock
	$(BENDER) script vsim -t test \
    --vlog-arg="$(VLOG_FLAGS)" \
    > compile.tcl

.PHONY: build test_l0 test_ro test_l0_nogui test_ro_nogui

build: compile.tcl
	$(VSIM) -c -do 'source compile.tcl; quit'

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
