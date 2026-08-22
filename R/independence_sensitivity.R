#' Sensitivity of event totals to threshold and rule
#'
#' Runs \code{\link{independent_events}} across a grid of thresholds and rules
#' and returns event counts, overall and optionally per species. The choice of
#' independence filter is rarely justified from data, so reporting this grid
#' turns an arbitrary decision into a stated sensitivity analysis.
#'
#' The per-species output is the more diagnostic of the two. If the extra events
#' produced by a metadata rule are concentrated in gregarious species, a
#' comparative index such as RAI is especially sensitive to the rule chosen.
#' The direction of the difference alone does not establish which rule is more
#' accurate.
#'
#' @inheritParams independent_events
#' @param thresholds Numeric vector of thresholds in minutes.
#' @param rules Character vector of rules to compare.
#' @param by_species If \code{TRUE}, also return counts per species.
#' @param metadata_refractory A single settling-window value in minutes, applied
#'   to every configuration. It must not exceed the smallest value in
#'   \code{thresholds}. To compare several settling windows, rerun the function
#'   for each value.
#'
#' @return A list with:
#'   \describe{
#'     \item{\code{overall}}{data frame: rule, threshold,
#'       metadata_refractory, records, events and pct_retained}
#'     \item{\code{by_species}}{data frame of per-species event counts with rule,
#'       threshold and metadata_refractory, or \code{NULL}}
#'     \item{\code{inflation}}{per-species percentage increase of each rule over
#'       \code{"time_only"} at the same threshold, or \code{NULL} if
#'       \code{"time_only"} was not among \code{rules}}
#'   }
#'
#' @examples
#' set.seed(1)
#' n <- 200
#' recs <- data.frame(
#'   station  = sample(c("CAM01", "CAM02"), n, TRUE),
#'   species  = sample(c("solitary_sp", "herd_sp"), n, TRUE),
#'   datetime = as.POSIXct("2021-01-01", tz = "UTC") +
#'                cumsum(sample(c(60, 300, 5000), n, TRUE)),
#'   adults   = sample(1:6, n, TRUE)
#' )
#' s <- independence_sensitivity(
#'   recs, "datetime", "station", "species",
#'   thresholds = c(15, 30, 60), metadata = "adults"
#' )
#' s$overall
#'
#' @seealso \code{\link{independent_events}}
#' @export
independence_sensitivity <- function(data,
                                     datetime,
                                     station,
                                     species    = NULL,
                                     thresholds = c(0, 15, 30, 60, 120),
                                     rules      = c("time_only", "running_max", "any_change"),
                                     metadata   = NULL,
                                     count      = NULL,
                                     compare_to = c("last_record", "last_independent"),
                                     format     = "%Y-%m-%d %H:%M:%S",
                                     tz         = "UTC",
                                     by_species = TRUE,
                                     record_id  = NULL,
                                     min_increase = 1,
                                     metadata_refractory = 0) {

  compare_to <- match.arg(compare_to)
  if (missing(rules) && !length(metadata)) rules <- "time_only"
  rules <- match.arg(rules, c("time_only", "running_max", "any_change"),
                     several.ok = TRUE)
  if (by_species && is.null(species)) by_species <- FALSE

  if (!is.numeric(thresholds) || !length(thresholds) || anyNA(thresholds) ||
      any(thresholds < 0)) {
    stop("`thresholds` must be a non-empty numeric vector of non-negative minutes.",
         call. = FALSE)
  }
  if (!is.numeric(metadata_refractory) || length(metadata_refractory) != 1L ||
      is.na(metadata_refractory) || metadata_refractory < 0) {
    stop("`metadata_refractory` must be a single non-negative number of minutes.",
         call. = FALSE)
  }
  if (metadata_refractory > min(thresholds)) {
    stop("`metadata_refractory` must not exceed the smallest value in `thresholds`.",
         call. = FALSE)
  }

  overall <- list()
  persp   <- list()

  for (rl in rules) {
    for (th in thresholds) {

      flagged <- independent_events(
        data, datetime, station, species,
        threshold = th, rule = rl, metadata = metadata, count = count,
        record_id = record_id,
        min_increase = min_increase,
        metadata_refractory = metadata_refractory,
        compare_to = compare_to, format = format, tz = tz, filter = FALSE
      )
      kept <- flagged[flagged$independent, , drop = FALSE]

      overall[[length(overall) + 1L]] <- data.frame(
        rule         = rl,
        threshold    = th,
        metadata_refractory = metadata_refractory,
        records      = nrow(data),
        events       = nrow(kept),
        pct_retained = round(100 * nrow(kept) / nrow(data), 2),
        stringsAsFactors = FALSE
      )

      if (by_species) {
        tab <- as.data.frame(table(species = kept[[species]]),
                             stringsAsFactors = FALSE)
        names(tab) <- c("species", "events")
        tab$rule      <- rl
        tab$threshold <- th
        tab$metadata_refractory <- metadata_refractory
        persp[[length(persp) + 1L]] <- tab
      }
    }
  }

  overall <- do.call(rbind, overall)
  by_sp   <- if (by_species) do.call(rbind, persp) else NULL

  ## per-species inflation relative to the pure time rule
  inflation <- NULL
  if (!is.null(by_sp) && "time_only" %in% rules && length(rules) > 1L) {
    base <- by_sp[by_sp$rule == "time_only",
                  c("species", "threshold", "metadata_refractory", "events")]
    names(base)[names(base) == "events"] <- "time_only"
    inflation <- base
    for (rl in setdiff(rules, "time_only")) {
      add <- by_sp[by_sp$rule == rl,
                   c("species", "threshold", "metadata_refractory", "events")]
      names(add)[names(add) == "events"] <- rl
      inflation <- merge(inflation, add,
                         by = c("species", "threshold", "metadata_refractory"),
                         all.x = TRUE)
      inflation[[rl]][is.na(inflation[[rl]])] <- 0
      inflation[[paste0(rl, "_pct")]] <-
        round(100 * (inflation[[rl]] - inflation$time_only) /
                pmax(inflation$time_only, 1), 1)
    }
    inflation <- inflation[order(inflation$threshold, -inflation$time_only), ]
    rownames(inflation) <- NULL
  }

  list(overall = overall, by_species = by_sp, inflation = inflation)
}
