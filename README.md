# camtrapEvents

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21628401.svg)](https://doi.org/10.5281/zenodo.21628401)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/samuelawini/camtrapEvents/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/samuelawini/camtrapEvents/actions/workflows/R-CMD-check.yaml)

**Metadata-aware independence filtering for camera-trap data**

Camera traps often produce many photographs of the same animal or group.
Analysts usually collapse these records with a fixed time threshold: another
record of the same species at the same station starts a new event only after
*k* minutes without a detection.

Time alone cannot resolve every case. A second group can arrive inside the
threshold, while one group can remain visible beyond it. `camtrapEvents` makes
the decision explicit and reproducible by allowing observed group size, age and
sex composition, behaviour, or identity labels to supplement time. These data
provide evidence, not automatic ground truth, so the package also reports
sensitivity to the rule and threshold chosen.

## Installation

```r
# install.packages("remotes")
# Install the current development version.
remotes::install_github("samuelawini/camtrapEvents")
library(camtrapEvents)
```

For exact reproduction of an archived release, add its tag. For example,
`remotes::install_github("samuelawini/camtrapEvents@v0.3.2")` installs version 0.3.2.

## Input contract

The input must contain one row per unique photograph-species detection.
Multiple annotation rows for the same photograph and species must first be
combined into one record. If a photo identifier is available, pass it through
`record_id`; the function will stop if duplicate photograph-species rows remain.

```r
events <- independent_events(
  records,
  datetime  = "Photo.Date.Time",
  station   = "Sampling.Unit.Name",
  species   = "species",
  record_id = "Photo.ID",
  threshold = 30,
  rule      = "running_max",
  metadata  = c("Adult.Male", "Adult.Female", "Juvenile", "Unknown.Adult"),
  count     = "Number.of.Animals"
)
```

Date-times are sorted within station and species, and the original row order is
restored in the output.

## The three rules

| Rule | A record inside the time-defined burst starts a new event when... | Metadata type |
|---|---|---|
| `time_only` | never | none |
| `running_max` | a numeric field exceeds its maximum already observed in the burst | numeric |
| `any_change` | any field differs from the preceding record | numeric or categorical |

`time_only` is the safe default when metadata are absent or unreliable.
For backward compatibility, supplying `metadata` while omitting `rule` selects
`running_max`. Name the rule explicitly in reproducible analyses.

`running_max` is the more conservative metadata rule. A fall from five visible
animals to three, or a later return to five, does not open another event. A rise
above five can. This limits repeated splitting as animals move in and out of the
frame, but it can still over-split one encounter when visibility improves.

`any_change` is deliberately permissive. It can be useful with reliable
individual IDs or other categorical evidence, but ordinary changes in visible
composition can make it over-count encounters. It should not be interpreted as
truth merely because it retains more records.

### Optional two-time-scale filter

`metadata_refractory` adds a short settling window inside the main threshold.
During that window, records still update the running maxima, but metadata cannot
open another event. For example:

```r
events <- independent_events(
  records,
  datetime = "datetime", station = "station", species = "species",
  threshold = 30,
  rule = "running_max",
  metadata = c("males", "females", "juveniles"),
  count = "group_size",
  metadata_refractory = 2
)
```

Here, the outer 30-minute threshold defines time bursts and the inner 2-minute
window prevents rapid count fluctuations from repeatedly opening events. A
positive value is an additional ecological assumption and should be justified
and included in sensitivity analysis. The default is `0`, which preserves the
single-threshold behaviour.

## Events are not individuals

The output distinguishes event classification from observed group size:

- `independent` identifies retained event records.
- `event_id` assigns every photograph to an event within station and species.
- `burst_id` identifies the outer time-defined burst within station and species.
- `count_increment` allocates increases in the maximum observed group size
  across events in a burst.
- `n_new` is a compatibility alias for `count_increment` and is deprecated.

The identifiers restart inside every station-species group. Use `station`,
`species` and `event_id` or `burst_id` together when grouping the full dataset.

`count_increment` prevents a split burst from duplicating the same observed
maximum, but it is not an identity estimate. Animals can leave, re-enter, remain
hidden, or be replaced by similar-looking individuals. Claims about distinct
individuals require independent identity evidence such as unique markings,
tags, or genetic identification.

When all photograph rows are returned, `count_increment` is repeated on every
row assigned to its event. Sum it only after retaining `independent == TRUE`, or
call the function with `filter = TRUE`.

## Report sensitivity

`independence_sensitivity()` evaluates the threshold-by-rule grid:

```r
s <- independence_sensitivity(
  records,
  datetime = "Photo.Date.Time",
  station  = "Sampling.Unit.Name",
  species  = "species",
  record_id = "Photo.ID",
  thresholds = c(15, 30, 60, 120),
  metadata = c("Adult.Male", "Adult.Female", "Juvenile", "Unknown.Adult"),
  count = "Number.of.Animals"
)

s$overall
s$by_species
s$inflation
```

A positive `metadata_refractory` must not exceed the smallest value in
`thresholds`. To compare several settling windows, rerun the function for each
value.

The `inflation` table is a relative-change diagnostic, not proof that the
additional events are correct. Large, species-specific differences show where
the analytical result depends strongly on the metadata rule and therefore where
manual validation or cautious interpretation is most important.

## Example data

The package includes `waterhole`, a simulated dataset with a constructed
`true_group` label for demonstrating the functions:

```r
data(waterhole)
truth <- length(unique(waterhole$true_group))

vapply(c("time_only", "running_max", "any_change"), function(r) {
  sum(independent_events(
    waterhole, "datetime", "station", "species",
    threshold = 30,
    rule = r,
    metadata = if (r == "time_only") NULL else
      c("males", "females", "juveniles"),
    count = "group_size"
  )$independent)
}, numeric(1))
```

This is an educational simulation, not empirical validation. Its constructed
labels make code behaviour checkable, but conclusions about accuracy should
come from simulations spanning plausible observation processes and from manual
image review conducted independently of the metadata being tested.

## Relationship to camtrapR

`camtrapEvents` complements rather than replaces
[camtrapR](https://jniedballa.github.io/camtrapR/). It accepts a plain data frame,
including output from `camtrapR::recordTable(minDeltaTime = 0)`, Camelot exports,
or Camera Trap Data Package observation tables.

| | camtrapR | camtrapEvents |
|---|---|---|
| Time threshold | `minDeltaTime` | `threshold` |
| Reference point | `deltaTimeComparedTo` | `compare_to` |
| Metadata-conditional classification | not available | `rule` + `metadata` |
| Short settling window | not available | `metadata_refractory` |
| Threshold/rule sensitivity | not available | `independence_sensitivity()` |

## Reporting template

> Records were grouped by species and camera station. A new event was retained
> after more than 30 minutes without a record, or when an age- or sex-class count
> exceeded the maximum already observed in the current time burst
> (`camtrapEvents` v0.3.2, `rule = "running_max"`,
> `compare_to = "last_record"`). Metadata-triggered events were not allowed
> within two minutes of the previous retained event
> (`metadata_refractory = 2`). Event totals under alternative thresholds and
> rules are reported in Table S1.

Adapt the wording to the configuration actually used; do not copy the settling
window if it was not applied.

## Citation

The concept DOI always resolves to the latest archived release:
[`10.5281/zenodo.21628401`](https://doi.org/10.5281/zenodo.21628401).
For exact reproducibility, cite the version DOI corresponding to the release
used. Version 0.3.2 is archived at
[`10.5281/zenodo.22062900`](https://doi.org/10.5281/zenodo.22062900).
Version 0.3.1 is archived at
[`10.5281/zenodo.22056886`](https://doi.org/10.5281/zenodo.22056886).
Version 0.3.0 is archived at
[`10.5281/zenodo.22010874`](https://doi.org/10.5281/zenodo.22010874).
The earlier version 0.2.0 is archived at
[`10.5281/zenodo.21639726`](https://doi.org/10.5281/zenodo.21639726).

The metadata-aware approach was first applied in:

> Awini, S., Cabeza, M., Goded, S., Mahama, A. & Annorbah, N.N.D. (2026).
> Tourism alters mammal behaviour and juvenile distribution in a West African
> protected area. *Oryx*. doi:10.1017/S0030605325102500

## Licence

MIT
