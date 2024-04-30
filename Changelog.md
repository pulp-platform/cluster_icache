# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).


## Unreleased
### Fixed
- lookup_serial: Make `write_ready_o` independent of `write_valid_i`.
- Fix L0 testbench.

### Changed
- Rename `SET_COUNT` to `WAY_COUNT` to correct terminology, as it reflects the number of ways in a set.

## 0.1.1 - 28.06.2024
### Added
- Allow fetches to bypass prefetches in L1.
- Requests to identical memory locations from different L0 caches are served at the same time, even when cached in L1

### Fixed
- Fix multi-set hit in serial L1.
- Fix verilator `lint_off` for latch.
- Allow `early_tag_width` == `tag_width` in L0.
- Fix SCM for FPGA target.

## 0.1.0 - 27.02.2024
Initial release, incorporating snitch_cluster and mempool modifications.
