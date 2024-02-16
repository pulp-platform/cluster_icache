# Cluster Instruction Cache

This folder contains components for the cluster instruction cache, originally
developed for the
[snitch cluster](https://github.com/pulp-platform/snitch_cluster). It is
developed as part of the PULP project, a joint effort between ETH Zurich and
the University of Bologna.

The instruction cache consists of a private L0 cache, usually made out of
latches or flip-flops, and an L1 cache, generally made of SRAMs.

The L0 cache is small and is used to serve requests in the same cycle as it has
been requested (the L0 sits in the snitch core's only pipeline stage).

The L1 cache is larger and is used to serve requests that miss in the L0 cache.
It accepts requests from the L0 cache and emits AXI requests downstream. It has
also been repurposed into a configurable AXI read-only cache.

## License

Unless specified otherwise in the respective file headers, all code checked
into this repository is made available under a permissive license. All hardware
sources and tool scripts are licensed under the Solderpad Hardware License 0.51
(see `LICENSE`).
