# camtrapEvents (development version)

* Validates `metadata` column names under every rule, including
  `rule = "time_only"`. A name not present in `data` previously raised an
  opaque `undefined columns selected` error when `count` was absent, and went
  undetected when `count` was supplied. Filtering results are unchanged: a call
  that used to succeed with an unrecognised `metadata` name now errors, because
  that name was being silently ignored.
* Reports each distinct warning once across an `independence_sensitivity()`
  grid, instead of repeating it for every configuration. The default grid runs
  15 configurations and R does not print warnings inline past ten, so a warning
  about the input was previously collapsed to `There were 15 warnings` with its
  message text hidden.
* Warns when `station` or `species` contains missing, blank or whitespace-only
  values, naming the column and the number of rows affected. Independence is
  assessed within these columns, so such records share a group and are filtered
  against each other, which can merge distinct animals into one event. Filtering
  behaviour is unchanged; the condition was previously silent.

# camtrapEvents 0.3.2

* Updates the canonical GitHub repository and citation links after the account
  rename to `samuelawini`.
* Keeps installation examples, badges, issue links, and archival metadata
  aligned with the renamed repository. Filtering behaviour is unchanged.

# camtrapEvents 0.3.1

* Fixes `count_increment` when counts are missing in an early event but become
  available later in the same burst.
* Validates `metadata_refractory` against the smallest threshold in
  `independence_sensitivity()` and documents how to compare settling windows.
* Clarifies that event and burst identifiers are local to each station-species
  group and that count increments should be summed only over retained rows.
* Makes the README examples runnable from a fresh R session and keeps package
  citation metadata synchronized with the package version.
* Adds automated cross-platform package checks and contribution guidance.

# camtrapEvents 0.3.0

* Adds `metadata_refractory`, an optional inner settling window during which
  metadata update the running state but cannot open another event. This exposes
  a two-time-scale alternative while retaining `0` as the backward-compatible
  default.
* Renames the derived quantity `n_new` to `count_increment`. The old name remains
  as a compatibility alias, but the documentation now states its correct
  interpretation: an increment in the maximum observed count, not proof of the
  number or identity of distinct animals.
* Adds optional `record_id` validation to catch multiple annotation rows for the
  same photograph and species before they are treated as separate detections.
* Makes `time_only` the safe default for `independent_events()`.
* Fixes running-maximum state after missing metadata so that one `NA` value does
  not disable later comparisons.
* Validates that count fields are numeric and non-negative, and passes
  `min_increase` through `independence_sensitivity()`.
* Expands tests and documentation around input units, uncertainty, sensitivity,
  and the distinction between encounters and observed group-size increments.

# camtrapEvents 0.2.0

* Added `n_new`, now retained as a deprecated alias of `count_increment`, and
  `min_increase` for raising the evidence threshold under `running_max`.
* Event flagging at `min_increase = 1` was unchanged from version 0.1.0.

# camtrapEvents 0.1.0

* First release with `time_only`, `running_max`, and `any_change` rules.
* Added `compare_to`, event and burst identifiers, and threshold-by-rule
  sensitivity summaries.
* Base R implementation with no hard dependencies.
