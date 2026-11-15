# Changelog

All notable changes to this project will be documented in this file.

## [0.18.3] - 2026-11-08

### Documentation

- Sharpen the Hex description (#420)

## [0.18.2] - 2026-11-06

### Documentation

- Add context7.json to claim the Context7 library and scope indexing to guides (#421)

## [0.18.0] - 2026-09-11

### Features

- *(multipart)* Add nova_multipart_plugin for multipart/form-data uploads (#412)

## [0.17.0] - 2026-09-08

### Features

- Replace routing_tree with an in-tree routing trie and support multiple listeners (#405)

## [0.16.0] - 2026-08-08

### Features

- *(pubsub)* Allow binary channels, not just atoms (#390)

## [0.15.3] - 2026-08-04

### Bug Fixes

- Splice a list-valued websocket reply into the command list (#400)

## [0.15.2] - 2026-07-30

### Dependencies

- Bump cowboy 2.15.0 -> 2.18.0 (CVE-2026-43966) (#398)

## [0.15.1] - 2026-07-02

### Bug Fixes

- Use SPDX 'Apache-2.0' license identifier for Hex (#392)

## [0.15.0] - 2026-05-26

### Features

- Support nested nova_apps (#380)

## [0.14.3] - 2026-05-16

### Miscellaneous

- Upgrade cowboy 2.15.0 and refresh deps (#384)

## [0.14.1] - 2026-03-25

### Documentation

- Add graceful shutdown guide (#379)

## [0.14.0] - 2026-03-19

### Features

- Add graceful shutdown for k8s deployments (#365)

## [0.13.16] - 2026-03-19

### Refactor

- Use binary format strings in logger calls (#377)

## [0.13.15] - 2026-03-19

### Bug Fixes

- Guard port_close in nova_watcher terminate (#376)

## [0.13.14] - 2026-03-18

### Bug Fixes

- *(test)* Update stale add_routes test to match PR #373 fix (#375)

## [0.13.12] - 2026-03-18

### Testing

- Improve test coverage via mutation testing (#374)

## [0.13.11] - 2026-03-18

### Bug Fixes

- Resolve crashes in add_routes/2 and get_view_name/1 (#373)

## [0.13.6] - 2026-02-13

### Bug Fixes

- Pass fork repo URL to nova_request_app CI (#363)

## [0.13.5] - 2026-02-11

### Bug Fixes

- Remove @doc tags from callback declarations (#355)

## [0.13.3] - 2026-02-11

### Testing

- Add EUnit and PropEr tests for core modules (#357)

## [0.13.2] - 2026-02-11

### Miscellaneous

- Update renovate config with hex datasource and ~> support
- Remove duplicate renovate config from .github/
- Add auto-release GitHub Action (#356)

## [0.13.1] - 2026-02-09

### Bug Fixes

- Handle old plugin info (#341)
- Delete labeler (#353)

### Miscellaneous

- Update GitHub Actions to latest versions and add Renovate (#352)

## [0.13.0] - 2025-09-18

### Bug Fixes

- Added the third print so we have correct arguments (#334)
- Updated dependencies (#338)

## [0.11.0] - 2025-03-21

### Bug Fixes

- Solve that we end up in a loop in error controller (#318)
- This will solve an issue whit cowboy and rebar (#317)
- Correct lock without pmod (#320)

### Miscellaneous

- Added jsonlogger module (#316)

## [0.9.17] - 2024-01-31

### Features

- Dynamically enabled stacktraces (#256)
- Improve documentation (#257)

## [0.9.16] - 2024-01-30

### Bug Fixes

- Fix a case where the controller crashed when stacktrace were missing (#252)

## [0.9.13] - 2023-12-27

### Features

- Configuration for correlation plugin (#245)

## [0.9.12] - 2023-10-28

### Bug Fixes

- Restructured the plugins (#224)

## [0.9.11] - 2023-10-24

### Bug Fixes

- Removed hex publish (#239)

## [0.9.10] - 2023-10-24

### Bug Fixes

- Bumper rebar3 in github actions (#238)

## [0.9.9] - 2023-10-24

### Bug Fixes

- Bump checkout version (#237)

## [0.9.8] - 2023-10-24

### Bug Fixes

- Changed from semver to git (#236)

## [0.9.7] - 2023-10-24

### Bug Fixes

- Now take semver version (#235)

## [0.9.6] - 2023-10-24

### Bug Fixes

- Added sPR title (#225)
- Added needs to job (#229)
- Changed github token (#230)
- Changed steps and workflow (#232)
- Added commit and tag (#234)

## [0.8.5] - 2021-11-25

### Bug Fixes

- Found yer problem
- Include nova.hrl, parse_options.
- Incorrect filename, undid include_lib changes
- Flush options list.
- Separation of concerns, better naming.
- Removed garbage.
- Removed superfluous string conversion.
- Spelling error, dialyzer.

### Features

- Cors plugin, utilities, separate folder
- Ignore crashdump.

