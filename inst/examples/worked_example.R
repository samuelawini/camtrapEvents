################################################################################
##  camtrapEvents: worked example
##
##  Runs on the bundled `waterhole` dataset, so it reproduces without any
##  external files. Its constructed `true_group` label makes software behaviour
##  checkable; it is an educational example, not empirical validation.
################################################################################

library(camtrapEvents)
data(waterhole)

dem   <- c("males", "females", "juveniles")
truth <- length(unique(waterhole$true_group))

cat("records:", nrow(waterhole),
    "| stations:", length(unique(waterhole$station)),
    "| species:", length(unique(waterhole$species)),
    "| TRUE encounters:", truth, "\n\n")

## ---------------------------------------------------------------------------
## 1. How close does each rule get to the truth?
## ---------------------------------------------------------------------------
cat("=== 30-minute threshold ===\n")
for (r in c("time_only", "running_max", "any_change")) {
  ev <- independent_events(
    waterhole, "datetime", "station", "species",
    threshold = 30, rule = r,
    metadata  = if (r == "time_only") NULL else dem,
    count     = "group_size", filter = TRUE
  )
  cat(sprintf("  %-12s %4d events (difference %+5.1f%%)   %4.0f count maximum\n",
              r, nrow(ev), 100 * (nrow(ev) - truth) / truth,
              sum(ev$count_increment)))
}

cat("\nThe sum of count_increment is identical under every rule because it is the\n",
    "sum of burst-wise observed maxima. It is not an identity estimate. Event\n",
    "counts remain rule-dependent.\n\n", sep = "")

## ---------------------------------------------------------------------------
## 2. Why summing group size duplicates an observed maximum after event splits
## ---------------------------------------------------------------------------
ev <- independent_events(waterhole, "datetime", "station", "species",
                         threshold = 30, rule = "any_change", metadata = dem,
                         count = "group_size", filter = TRUE)
cat("=== observed count summaries, any_change at 30 min ===\n")
cat(sprintf("  sum(count_increment) %5.0f   <- burst-wise observed maxima\n",
            sum(ev$count_increment)))
cat(sprintf("  sum(group_size)      %5.0f   <- repeats visible counts after splits\n",
            sum(ev$group_size)))
cat(sprintf("  events with zero count increment: %d of %d (%.1f%%)\n\n",
            sum(ev$count_increment == 0), nrow(ev),
            100 * mean(ev$count_increment == 0)))

## ---------------------------------------------------------------------------
## 3. Sensitivity: report the choice rather than asserting it
## ---------------------------------------------------------------------------
s <- independence_sensitivity(
  waterhole, "datetime", "station", "species",
  thresholds = c(0, 15, 30, 60, 120), metadata = dem, count = "group_size"
)
cat("=== event totals by configuration ===\n")
print(s$overall, row.names = FALSE)

cat("\n=== inflation over the time-only rule, by species, 30 min ===\n")
print(s$inflation[s$inflation$threshold == 30, ], row.names = FALSE)

cat("\nLarge species-specific differences show where conclusions are most sensitive\n",
    "to the rule. They do not, by themselves, identify which count is true.\n",
    sep = "")

## ---------------------------------------------------------------------------
## 4. Categorical metadata
## ---------------------------------------------------------------------------
## `any_change` also accepts non-numeric metadata, such as behaviour or an
## individual ID. `running_max` cannot, since it needs a count to compare.
beh <- independent_events(waterhole, "datetime", "station", "species",
                          threshold = 30, rule = "any_change",
                          metadata = "behaviour", filter = TRUE)
cat(sprintf("\nusing behaviour as the metadata: %d events\n", nrow(beh)))
