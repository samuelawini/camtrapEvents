audit_fixture <- function(t = c(0, 5, 10), counts = c(2, 3, 2)) {
  data.frame(station = "A", species = "x",
             datetime = as.POSIXct("2021-01-01", tz = "UTC") + 60 * t,
             adults = counts)
}

audit_events <- function(data, ...) {
  independent_events(data, "datetime", "station", "species", ...)
}

test_that("group and record keys preserve distinct tuples", {
  d <- audit_fixture(c(0, 5), c(1, 1))
  d$station <- c("A\rB", "A")
  d$species <- c("C", "B\rC")
  d$photo <- "same-photo"
  expect_equal(audit_events(d, record_id = "photo")$independent, c(TRUE, TRUE))
  d$station <- c(NA_character_, "NA")
  d$species <- "x"
  expect_equal(suppressWarnings(audit_events(d, record_id = "photo"))$independent,
               c(TRUE, TRUE))
  d$station <- "A"
  d$photo <- c("photo\rx", "photo")
  d$species <- c("y", "x\ry")
  expect_equal(audit_events(d, record_id = "photo")$independent, c(TRUE, TRUE))
  d$species <- "x"
  d$photo <- "same-photo"
  expect_error(audit_events(d, record_id = "photo"), "share.*photo")
})

test_that("missing species reconcile overall, species and inflation totals", {
  d <- audit_fixture()
  d$species <- c("NA", NA_character_, "x")
  s <- suppressWarnings(independence_sensitivity(
    d, "datetime", "station", "species", thresholds = 30, metadata = "adults"
  ))
  expect_equal(nrow(s$inflation), 3L)
  expect_equal(sum(is.na(s$inflation$species)), 1L)
  for (rule in s$overall$rule) {
    expect_equal(sum(s$by_species$events[s$by_species$rule == rule]),
                 s$overall$events[s$overall$rule == rule])
  }
  d$species <- NA_character_
  s <- suppressWarnings(independence_sensitivity(
    d, "datetime", "station", "species", thresholds = 30, metadata = "adults"
  ))
  expect_equal(nrow(s$inflation), 1L)
  expect_true(is.na(s$inflation$species))
})

test_that("observed group sizes must be valid counts", {
  for (rule in c("time_only", "running_max", "any_change")) {
    expect_error(audit_events(audit_fixture(counts = c(-1, 2, 3)),
                              rule = rule, metadata = "adults"), "non-negative")
    expect_error(audit_events(audit_fixture(counts = c(1, Inf, Inf)),
                              rule = rule, metadata = "adults"), "finite")
    expect_error(audit_events(audit_fixture(counts = c(1, Inf, Inf)),
                              rule = rule, metadata = "adults", count = "adults"),
                 "finite")
    z <- audit_events(audit_fixture(counts = c(NA, 2, 3)),
                      rule = rule, metadata = "adults")
    expect_equal(sum(z$count_increment[z$independent], na.rm = TRUE), 3)
  }
  d <- audit_fixture(counts = rep(1e308, 3))
  d$other <- d$adults
  expect_error(audit_events(d, metadata = c("adults", "other")), "sum.*finite")
  d <- audit_fixture(counts = c(-2, -1, -2))
  d$behaviour <- c("rest", "walk", "rest")
  z <- audit_events(d, rule = "any_change", metadata = c("adults", "behaviour"))
  expect_equal(z$independent, rep(TRUE, 3))
  expect_true(all(is.na(z$count_increment)))
})

test_that("repeated sensitivity requests do not multiply configurations", {
  s <- independence_sensitivity(audit_fixture(), "datetime", "station", "species",
                                thresholds = c(30, 15, 30), metadata = "adults")
  expect_equal(s$overall$threshold, rep(c(30, 15), 3))
  expect_equal(nrow(s$inflation), 2L)
  expect_false(anyDuplicated(s$overall[c("rule", "threshold")]) > 0)
  repeated <- independence_sensitivity(
    audit_fixture(), "datetime", "station", "species",
    thresholds = 30, rules = c("time_only", "running_max", "time_only"),
    metadata = "adults"
  )
  expect_equal(repeated$overall$rule, c("time_only", "running_max"))
  expect_equal(nrow(repeated$inflation), 1L)
})
