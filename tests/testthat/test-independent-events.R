## Tests for independent_events(). Base fixtures build one station x species
## group at controlled spacings and metadata values.

mk <- function(mins, ...) {
  data.frame(
    station  = "CAM01",
    species  = "sp1",
    datetime = as.POSIXct("2021-01-01 00:00:00", tz = "UTC") + mins * 60,
    ...,
    stringsAsFactors = FALSE
  )
}

flag <- function(d, ...) {
  independent_events(d, "datetime", "station", "species", ...)$independent
}

test_that("a single record is always independent", {
  d <- mk(0, adults = 1)
  expect_true(flag(d, threshold = 30, rule = "time_only"))
})

test_that("time_only is the safe default", {
  d <- mk(c(0, 5), adults = c(1, 2))
  expect_equal(flag(d, threshold = 30), c(TRUE, FALSE))
  ## Preserve the <=0.2.0 shorthand in which supplying metadata selected the
  ## running-maximum rule even when `rule` was not named.
  expect_equal(flag(d, threshold = 30, metadata = "adults"), c(TRUE, TRUE))
})

test_that("sensitivity defaults to time_only when metadata are absent", {
  d <- mk(c(0, 5), adults = c(1, 2))
  s <- independence_sensitivity(d, "datetime", "station", "species",
                                thresholds = c(15, 30))
  expect_equal(s$overall$rule, c("time_only", "time_only"))
})

test_that("a gap beyond the threshold always starts a new event", {
  d <- mk(c(0, 45), adults = c(1, 1))
  expect_equal(flag(d, threshold = 30, rule = "time_only"), c(TRUE, TRUE))
  expect_equal(flag(d, threshold = 30, rule = "running_max",
                    metadata = "adults"), c(TRUE, TRUE))
})

test_that("a gap exactly equal to the threshold is not a new event", {
  d <- mk(c(0, 30), adults = c(1, 1))
  expect_equal(flag(d, threshold = 30, rule = "time_only"), c(TRUE, FALSE))
})

test_that("identical records inside the window are dependent under every rule", {
  d <- mk(c(0, 5), adults = c(2, 2))
  for (r in c("time_only", "any_change", "running_max")) {
    expect_equal(flag(d, threshold = 30, rule = r, metadata = "adults"),
                 c(TRUE, FALSE), info = r)
  }
})

test_that("running_max ignores a fall and a return to a value already seen", {
  ## The counting-noise case: 5 -> 3 -> 5 is one group, not three.
  d <- mk(c(0, 5, 10), adults = c(5, 3, 5))
  expect_equal(flag(d, threshold = 30, rule = "running_max",
                    metadata = "adults"), c(TRUE, FALSE, FALSE))
  expect_equal(flag(d, threshold = 30, rule = "any_change",
                    metadata = "adults"), c(TRUE, TRUE, TRUE))
})

test_that("running_max flags a rise above the burst maximum", {
  d <- mk(c(0, 5, 10), adults = c(3, 3, 7))
  expect_equal(flag(d, threshold = 30, rule = "running_max",
                    metadata = "adults"), c(TRUE, FALSE, TRUE))
})

test_that("running_max responds to any one metadata column", {
  d <- mk(c(0, 5), adults = c(4, 4), juveniles = c(0, 1))
  expect_equal(flag(d, threshold = 30, rule = "running_max",
                    metadata = c("adults", "juveniles")), c(TRUE, TRUE))
})

test_that("any_change works with categorical metadata but running_max does not", {
  d <- mk(c(0, 5), behaviour = c("passing", "drinking"))
  expect_equal(flag(d, threshold = 30, rule = "any_change",
                    metadata = "behaviour"), c(TRUE, TRUE))
  expect_error(flag(d, threshold = 30, rule = "running_max",
                    metadata = "behaviour"), "numeric")
})

test_that("count is honoured by running_max alongside metadata", {
  d <- mk(c(0, 5), adults = c(2, 2), n_animals = c(2, 9))
  expect_equal(flag(d, threshold = 30, rule = "running_max",
                    metadata = "adults", count = "n_animals"),
               c(TRUE, TRUE))
})

test_that("compare_to changes how long bursts are subdivided", {
  ## Records every 5 min for an hour, identical metadata.
  d <- mk(seq(0, 60, by = 5), adults = 1)
  expect_equal(sum(flag(d, threshold = 30, rule = "time_only",
                        compare_to = "last_record")), 1L)
  expect_equal(sum(flag(d, threshold = 30, rule = "time_only",
                        compare_to = "last_independent")), 2L)
})

test_that("independence is assessed within station and within species", {
  d <- rbind(
    data.frame(station = "A", species = "x",
               datetime = as.POSIXct("2021-01-01 00:00:00", tz = "UTC"),
               adults = 1, stringsAsFactors = FALSE),
    data.frame(station = "B", species = "x",
               datetime = as.POSIXct("2021-01-01 00:05:00", tz = "UTC"),
               adults = 1, stringsAsFactors = FALSE),
    data.frame(station = "A", species = "y",
               datetime = as.POSIXct("2021-01-01 00:05:00", tz = "UTC"),
               adults = 1, stringsAsFactors = FALSE)
  )
  ## All three are in different groups, so all are independent.
  expect_equal(flag(d, threshold = 30, rule = "time_only"),
               c(TRUE, TRUE, TRUE))
})

test_that("row order is preserved and unsorted input is handled", {
  d <- mk(c(45, 0), adults = c(1, 1))
  expect_equal(flag(d, threshold = 30, rule = "time_only"), c(TRUE, TRUE))

  d2 <- mk(c(5, 0), adults = c(2, 2))
  ## The record at t=0 is the first chronologically, so it is the event.
  expect_equal(flag(d2, threshold = 30, rule = "time_only"), c(FALSE, TRUE))
})

test_that("event_id groups dependent records onto their event", {
  d <- mk(c(0, 5, 45), adults = 1)
  out <- independent_events(d, "datetime", "station", "species",
                            threshold = 30, rule = "time_only")
  expect_equal(out$event_id, c(1L, 1L, 2L))
  expect_equal(out$burst_id, c(1L, 1L, 2L))
})

test_that("filter = TRUE returns only independent records", {
  d <- mk(c(0, 5, 45), adults = 1)
  out <- independent_events(d, "datetime", "station", "species",
                            threshold = 30, rule = "time_only", filter = TRUE)
  expect_equal(nrow(out), 2L)
  expect_true(all(out$independent))
})

test_that("threshold = 0 retains everything except exact-duplicate times", {
  d <- mk(c(0, 1, 2), adults = 1)
  expect_equal(flag(d, threshold = 0, rule = "time_only"),
               c(TRUE, TRUE, TRUE))
  d2 <- mk(c(0, 0), adults = 1)
  expect_equal(flag(d2, threshold = 0, rule = "time_only"), c(TRUE, FALSE))
})

test_that("character date-times are parsed with format", {
  d <- data.frame(station = "A", species = "x",
                  datetime = c("21-06-02 08:00:00", "21-06-02 08:05:00"),
                  adults = 1, stringsAsFactors = FALSE)
  expect_equal(
    independent_events(d, "datetime", "station", "species", threshold = 30,
                       rule = "time_only", format = "%y-%m-%d %H:%M:%S")$independent,
    c(TRUE, FALSE)
  )
})

test_that("NA metadata does not propagate into the flag", {
  d <- mk(c(0, 5), adults = c(2, NA))
  expect_false(anyNA(flag(d, threshold = 30, rule = "running_max",
                          metadata = "adults")))
  expect_false(anyNA(flag(d, threshold = 30, rule = "any_change",
                          metadata = "adults")))
})

test_that("running maxima recover after missing metadata", {
  d <- mk(c(0, 5, 10), adults = c(2, NA, 3))
  expect_equal(flag(d, threshold = 30, rule = "running_max",
                    metadata = "adults"), c(TRUE, FALSE, TRUE))

  d2 <- mk(c(0, 5, 10), adults = c(NA, 2, 3))
  expect_equal(flag(d2, threshold = 30, rule = "running_max",
                    metadata = "adults"), c(TRUE, FALSE, TRUE))
})

test_that("informative errors are raised for bad input", {
  d <- mk(c(0, 5), adults = 1)
  expect_error(flag(d, threshold = -1, rule = "time_only"), "non-negative")
  expect_error(independent_events(d, "nope", "station", "species"),
               "not found")
  expect_error(flag(d, threshold = 30, rule = "any_change"), "requires")
  expect_error(independent_events(d[0, ], "datetime", "station", "species"),
               "no rows")

  factor_count <- transform(d, n_animals = factor(c(1, 2)))
  expect_error(independent_events(factor_count, "datetime", "station", "species",
                                  count = "n_animals"), "numeric")

  negative <- mk(c(0, 5), adults = c(1, -1))
  expect_error(flag(negative, threshold = 30, rule = "running_max",
                    metadata = "adults"), "non-negative")
})

test_that("record_id catches duplicate photograph-species annotations", {
  d <- mk(c(0, 0, 5), adults = c(1, 1, 2))
  d$photo_id <- c("IMG001", "IMG001", "IMG002")
  expect_error(
    independent_events(d, "datetime", "station", "species",
                       threshold = 30, rule = "time_only",
                       record_id = "photo_id"),
    "Consolidate multiple annotation rows"
  )

  d$species[2] <- "sp2"
  expect_silent(independent_events(
    d, "datetime", "station", "species", threshold = 30,
    rule = "time_only", record_id = "photo_id"
  ))
})

test_that("species = NULL pools all species", {
  d <- data.frame(station = "A", species = c("x", "y"),
                  datetime = as.POSIXct("2021-01-01", tz = "UTC") + c(0, 300),
                  adults = 1, stringsAsFactors = FALSE)
  expect_equal(
    independent_events(d, "datetime", "station", species = NULL,
                       threshold = 30, rule = "time_only")$independent,
    c(TRUE, FALSE)
  )
})

test_that("independence_sensitivity returns a coherent grid", {
  d <- mk(c(0, 5, 10, 45, 50), adults = c(2, 3, 2, 1, 1))
  s <- independence_sensitivity(
    d, "datetime", "station", "species",
    thresholds = c(15, 30), metadata = "adults"
  )
  expect_equal(nrow(s$overall), 6L)
  expect_true(all(s$overall$events <= nrow(d)))
  ## time_only can never retain more events than a metadata rule
  for (th in c(15, 30)) {
    base <- s$overall$events[s$overall$rule == "time_only" &
                             s$overall$threshold == th]
    others <- s$overall$events[s$overall$rule != "time_only" &
                               s$overall$threshold == th]
    expect_true(all(others >= base))
  }
  expect_true(!is.null(s$inflation))
})

test_that("min_increase raises the evidence bar for a new event", {
  ## a rise of 1 looks like a miscount; a rise of 4 does not
  d <- mk(c(0, 5), adults = c(3, 4))
  expect_equal(flag(d, threshold = 30, rule = "running_max",
                    metadata = "adults", min_increase = 1), c(TRUE, TRUE))
  expect_equal(flag(d, threshold = 30, rule = "running_max",
                    metadata = "adults", min_increase = 2), c(TRUE, FALSE))

  d2 <- mk(c(0, 5), adults = c(3, 7))
  expect_equal(flag(d2, threshold = 30, rule = "running_max",
                    metadata = "adults", min_increase = 2), c(TRUE, TRUE))

  expect_error(flag(d, threshold = 30, rule = "running_max",
                    metadata = "adults", min_increase = 0), "min_increase")
})

test_that("metadata_refractory provides a two-time-scale filter", {
  ## The 3 -> 5 -> 2 fluctuation is absorbed into the running maximum during
  ## the settling window. A later rise above that maximum can open an event.
  d <- mk(c(0, 0.5, 1, 5), adults = c(3, 5, 2, 6))
  single <- flag(d, threshold = 30, rule = "running_max",
                 metadata = "adults", metadata_refractory = 0)
  two_scale <- flag(d, threshold = 30, rule = "running_max",
                    metadata = "adults", metadata_refractory = 2)
  expect_equal(single, c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(two_scale, c(TRUE, FALSE, FALSE, TRUE))
})

test_that("the refractory boundary is inclusive and time gaps still win", {
  d <- mk(c(0, 2, 2.1, 45), adults = c(1, 2, 3, 3))
  expect_equal(flag(d, threshold = 30, rule = "running_max",
                    metadata = "adults", metadata_refractory = 2),
               c(TRUE, FALSE, TRUE, TRUE))
  expect_error(flag(d, threshold = 30, rule = "running_max",
                    metadata = "adults", metadata_refractory = 31),
               "between 0 and")
  expect_error(flag(d, threshold = 30, rule = "running_max",
                    metadata = "adults", metadata_refractory = -1),
               "between 0 and")
})

test_that("count_increment allocates a burst-wise observed maximum", {
  ## 3 -> 5 -> 5 contributes increments 3 and 2, not duplicated counts 3 and 5
  d <- mk(c(0, 5, 10), adults = c(3, 5, 5))
  out <- independent_events(d, "datetime", "station", "species",
                            threshold = 30, rule = "running_max",
                            metadata = "adults")
  expect_equal(out$independent, c(TRUE, TRUE, FALSE))
  expect_equal(sum(out$count_increment[out$independent]), 5)
  expect_equal(out$count_increment[out$independent], c(3, 2))
  expect_equal(out$n_new, out$count_increment)
})

test_that("count_increment equals the observed burst maximum when unsplit", {
  ## a group of 3 seen repeatedly is one encounter of three animals
  d <- mk(c(0, 5, 10), adults = c(3, 2, 3))
  out <- independent_events(d, "datetime", "station", "species",
                            threshold = 30, rule = "time_only", count = "adults")
  expect_equal(sum(out$independent), 1L)
  expect_equal(sum(out$count_increment[out$independent]), 3)
})

test_that("count_increment is NA unless a group size can be obtained", {
  d <- mk(c(0, 5), adults = c(3, 3))
  bare <- independent_events(d, "datetime", "station", "species",
                             threshold = 30, rule = "time_only")
  expect_true(all(is.na(bare$count_increment)))
})

test_that("an event opened by a decrease has zero count increment", {
  ## any_change splits on 5 -> 3, but the observed maximum did not rise
  d <- mk(c(0, 5), adults = c(5, 3))
  out <- independent_events(d, "datetime", "station", "species",
                            threshold = 30, rule = "any_change",
                            metadata = "adults")
  expect_equal(out$independent, c(TRUE, TRUE))
  expect_equal(out$count_increment[out$independent], c(5, 0))
  expect_equal(sum(out$count_increment[out$independent]), 5)
})

test_that("count_increment resets across bursts", {
  d <- mk(c(0, 200), adults = c(3, 4))
  out <- independent_events(d, "datetime", "station", "species",
                            threshold = 30, rule = "running_max",
                            metadata = "adults")
  expect_equal(out$count_increment[out$independent], c(3, 4))
})

test_that("count_increment uses count when supplied and metadata sum otherwise", {
  d <- mk(c(0, 5), males = c(1, 2), females = c(2, 2), n_animals = c(3, 4))
  a <- independent_events(d, "datetime", "station", "species", threshold = 30,
                          rule = "running_max", metadata = c("males", "females"),
                          count = "n_animals")
  expect_equal(sum(a$count_increment[a$independent]), 4)

  b <- independent_events(d, "datetime", "station", "species", threshold = 30,
                          rule = "running_max", metadata = c("males", "females"))
  expect_equal(sum(b$count_increment[b$independent]), 4)
})

test_that("count_increment is NA for non-numeric metadata without count", {
  d <- mk(c(0, 5), behaviour = c("passing", "drinking"))
  out <- independent_events(d, "datetime", "station", "species", threshold = 30,
                            rule = "any_change", metadata = "behaviour")
  expect_true(all(is.na(out$count_increment)))
})

test_that("event totals decrease monotonically as the threshold increases", {
  set.seed(42)
  n <- 300
  d <- data.frame(
    station  = sample(c("A", "B"), n, TRUE),
    species  = sample(c("x", "y"), n, TRUE),
    datetime = as.POSIXct("2021-01-01", tz = "UTC") +
                 cumsum(sample(c(30, 120, 4000), n, TRUE)),
    adults   = sample(1:5, n, TRUE),
    stringsAsFactors = FALSE
  )
  s <- independence_sensitivity(d, "datetime", "station", "species",
                                thresholds = c(0, 15, 30, 60, 120),
                                rules = "time_only")
  expect_false(is.unsorted(rev(s$overall$events)))
})
