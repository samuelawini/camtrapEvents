#' Flag independent detection events in camera-trap data
#'
#' Camera traps fire repeatedly on the same animal or group, so raw records are
#' not statistically independent. The near-universal remedy is a fixed time
#' threshold: a record starts a new event only if it falls more than \code{k}
#' minutes after the previous one at the same station for the same species.
#'
#' A time threshold alone cannot distinguish an animal that lingers in front of
#' the camera from the arrival of new individuals. Species that loiter, such as
#' baboons or warthogs at a waterhole, are collapsed into a single event no
#' matter how long they stay or how the group changes, while the same threshold
#' applied to a transient species behaves quite differently. \code{independent_events()}
#' therefore lets the independence decision use record-level metadata (group
#' size, sex and age counts, behaviour, individual ID) in addition to time.
#'
#' @param data A data frame of camera-trap records, one row per unique
#'   photograph-species detection. Multiple annotation rows for the same
#'   photograph and species must be consolidated before filtering.
#' @param datetime Name of the date-time column. Either \code{POSIXct}, or
#'   character parsed with \code{format}. Records whose date-time cannot be
#'   parsed are warned about, and each becomes its own burst and event carrying
#'   its own \code{count_increment}. Drop or repair them beforehand if that is
#'   not intended.
#' @param station Name of the column identifying the camera or station.
#'   Independence is assessed within station. Missing, blank or whitespace-only
#'   values raise a warning: such records share a group and are filtered against
#'   each other.
#' @param species Name of the species column. Independence is assessed within
#'   species. Pass \code{NULL} to pool all species. Missing, blank or
#'   whitespace-only values raise a warning: such records share a group, so
#'   distinct unidentified animals at one station may be merged into a single
#'   event. Label them explicitly if that is not intended.
#' @param record_id Optional name of a column uniquely identifying a photograph
#'   or trigger. When supplied, duplicate identifiers within a station-species
#'   group cause an error. This catches split annotation rows that would
#'   otherwise be mistaken for separate detections.
#' @param threshold Time threshold in minutes. A gap strictly greater than
#'   \code{threshold} always starts a new event.
#' @param rule How metadata is used inside the time window:
#'   \describe{
#'     \item{\code{"time_only"}}{Metadata ignored. The conventional fixed-threshold
#'       filter, corresponding to \pkg{camtrapR}'s \code{minDeltaTime}. The
#'       boundary differs: a gap exactly equal to \code{threshold} does not open
#'       an event here, whereas \pkg{camtrapR} treats a gap of exactly
#'       \code{minDeltaTime} as independent.}
#'     \item{\code{"any_change"}}{A record starts a new event if ANY column in
#'       \code{metadata} differs from the preceding record. Works with numeric or
#'       categorical metadata. This is the rule used in Awini et al. (2026).
#'       Note that it is triggered by decreases as well as increases, so with
#'       noisy counts it can markedly increase event totals for gregarious species; see
#'       \code{vignette("choosing-a-rule")} and \code{independence_sensitivity()}.}
#'     \item{\code{"running_max"}}{A record starts a new event only if a numeric
#'       metadata column, or \code{count}, EXCEEDS the running maximum already
#'       observed within the current burst. Interprets only an increase above
#'       everything seen so far as evidence consistent with an arrival not
#'       represented in earlier records.
#'       It is less sensitive than \code{"any_change"} to oscillating counts of a
#'       moving group. Requires numeric \code{metadata}.}
#'   }
#'   If omitted, the function uses \code{"time_only"} when no metadata are
#'   supplied and \code{"running_max"} when metadata are supplied, preserving
#'   the shorthand used in package versions up to 0.2.0.
#' @param metadata Character vector of column names carrying the metadata used
#'   by \code{rule}. Ignored for event classification when
#'   \code{rule = "time_only"}. If \code{count} is absent and every metadata
#'   column is numeric, their row sum is still used as the fallback group size
#'   for \code{count_increment}.
#' @param count Optional name of a total group-size column, used by
#'   \code{"running_max"} in addition to \code{metadata}, and used to compute
#'   \code{count_increment}. If absent, group size falls back to the sum of numeric
#'   \code{metadata}.
#' @param min_increase How far a count must exceed the running maximum before it
#'   is treated as evidence for a new event under \code{"running_max"}. The
#'   default of 1 accepts any increase. Raise it where tagging is noisy: a rise
#'   from 3 to 4 animals is exactly what a miscount looks like, whereas a rise
#'   from 3 to 7 is not. Ignored by the other rules.
#' @param metadata_refractory Optional short settling window in minutes. Within
#'   this period after the last retained event, metadata may update the running
#'   maxima but cannot itself open another event. A gap beyond \code{threshold}
#'   still opens an event. The default, 0, preserves the single-threshold
#'   behaviour. Positive values provide the two-time-scale filter discussed in
#'   the package vignette and must not exceed \code{threshold}.
#' @param compare_to Reference point for the time gap:
#'   \describe{
#'     \item{\code{"last_record"}}{Gap measured from the previous record, retained
#'       or not. A burst ends only after \code{threshold} elapses with no records
#'       at all. Default, and the behaviour of most published filters.}
#'     \item{\code{"last_independent"}}{Gap measured from the last retained event,
#'       subdividing long bursts. The subdivisions fall at fixed intervals only
#'       under \code{rule = "time_only"}; under a metadata rule they do not,
#'       because a metadata-triggered event also resets the reference point.}
#'   }
#'   Corresponds to \pkg{camtrapR}'s \code{deltaTimeComparedTo}
#'   (\code{"lastRecord"} and \code{"lastIndependentRecord"}).
#' @param format Format string used to parse \code{datetime} when it is character.
#' @param tz Time zone used for parsing. Defaults to \code{"UTC"}.
#' @param filter If \code{TRUE}, return only independent records. If \code{FALSE}
#'   (default), return all records with the flag columns added.
#'
#' @return \code{data} with five columns added:
#'   \describe{
#'     \item{\code{independent}}{logical, \code{TRUE} for an independent event}
#'     \item{\code{event_id}}{integer, consecutive event number within station
#'       and species}
#'     \item{\code{burst_id}}{integer, consecutive time-defined burst number
#'       within station and species}
#'     \item{\code{count_increment}}{numeric, the increase in the maximum
#'       observed group size allocated to this event; \code{NA} when no group
#'       size is available}
#'     \item{\code{n_new}}{deprecated compatibility alias of
#'       \code{count_increment}}
#'   }
#'   Row order is preserved. Neither identifier is globally unique: use
#'   station, species and \code{event_id} or \code{burst_id} together when
#'   grouping records across the full dataset.
#'
#' @section Events versus individuals:
#' These are different units and the package reports both. A group of three
#' animals passing together is one encounter containing three individuals, not
#' three encounters; treating it as three is the pseudo-replication that
#' independence filtering exists to prevent. But three animals arriving
#' separately within the window are three encounters, and a time-only rule
#' wrongly merges them.
#'
#' Use \code{event_id} for anything that counts encounters, such as diel activity
#' patterns or encounter-rate indices. \code{count_increment} is a separate
#' descriptive quantity: summing it over retained records gives the sum of the
#' maximum observed group size in each time-defined burst, without double-
#' counting increases when a metadata rule splits that burst:
#'
#' \preformatted{
#' ev <- independent_events(recs, ..., filter = TRUE)
#' sum(ev$count_increment)  # sum of burst-wise observed maxima
#' nrow(ev)                 # encounters
#' }
#'
#' This is not an identity estimate. Animals can leave, re-enter, remain hidden,
#' or be replaced by similar-looking individuals, so neither
#' \code{count_increment} nor its compatibility alias \code{n_new} proves how
#' many distinct individuals were present. Individual-level inference requires
#' independent identity evidence.
#'
#' @section Choosing a rule:
#' \code{"time_only"} is the safe default when metadata is unreliable or absent.
#' \code{"running_max"} is the more conservative metadata rule when age and sex
#' counts are available, because decreases and returns to values already seen do
#' not open events. It can still over-split one encounter when visibility first
#' improves, so it should be checked against \code{"time_only"} rather than
#' treated as ground truth.
#' \code{"any_change"} is the most permissive and should be reported alongside
#' \code{independence_sensitivity()} output so readers can see how much of the
#' event total depends on it.
#'
#' Whichever is chosen, state the threshold, the rule and \code{compare_to}
#' explicitly in the methods: these three choices are not interchangeable and
#' materially change event totals, especially for gregarious species.
#'
#' @references
#' Awini, S., Cabeza, M., Goded, S., Mahama, A. & Annorbah, N.N.D. (2026)
#' Tourism alters mammal behaviour and juvenile distribution in a West African
#' protected area. \emph{Oryx}. \doi{10.1017/S0030605325102500}
#'
#' @examples
#' recs <- data.frame(
#'   station  = "CAM01",
#'   species  = "Kobus kob",
#'   datetime = as.POSIXct("2021-06-02 08:00:00", tz = "UTC") +
#'                60 * c(0, 5, 10, 15, 90),
#'   females  = c(3, 5, 2, 3, 1),
#'   juveniles = c(0, 0, 1, 1, 0)
#' )
#'
#' # Conventional time-only filter: two events.
#' independent_events(recs, "datetime", "station", "species",
#'                    threshold = 30, rule = "time_only")$independent
#'
#' # running_max: also flags the rise to 5 females and the first juvenile.
#' independent_events(recs, "datetime", "station", "species",
#'                    threshold = 30, rule = "running_max",
#'                    metadata = c("females", "juveniles"))$independent
#'
#' @seealso \code{\link{independence_sensitivity}}
#' @export
independent_events <- function(data,
                               datetime,
                               station,
                               species      = NULL,
                               threshold    = 30,
                               rule         = c("time_only", "running_max", "any_change"),
                               metadata     = NULL,
                               count        = NULL,
                               min_increase = 1,
                               compare_to   = c("last_record", "last_independent"),
                               format       = "%Y-%m-%d %H:%M:%S",
                               tz           = "UTC",
                               filter       = FALSE,
                               record_id    = NULL,
                               metadata_refractory = 0) {

  if (missing(rule)) {
    ## Calls written for <=0.2.0 often supplied metadata without naming the
    ## rule, when running_max was first in the formal choices. Preserve that
    ## behaviour while allowing a metadata-free call to use the safe time rule.
    rule <- if (length(metadata)) "running_max" else "time_only"
  } else {
    rule <- match.arg(rule)
  }
  compare_to <- match.arg(compare_to)

  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (nrow(data) == 0L)     stop("`data` has no rows.", call. = FALSE)
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) ||
      threshold < 0) {
    stop("`threshold` must be a single non-negative number of minutes.", call. = FALSE)
  }
  if (!is.numeric(min_increase) || length(min_increase) != 1L ||
      is.na(min_increase) || min_increase < 1) {
    stop("`min_increase` must be a single number >= 1.", call. = FALSE)
  }
  if (!is.numeric(metadata_refractory) || length(metadata_refractory) != 1L ||
      is.na(metadata_refractory) || metadata_refractory < 0 ||
      metadata_refractory > threshold) {
    stop("`metadata_refractory` must be between 0 and `threshold` minutes.",
         call. = FALSE)
  }

  need <- c(datetime, station, species, record_id, count, metadata)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols)) {
    stop("Column(s) not found in `data`: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  if (rule != "time_only" && !length(metadata)) {
    stop("`rule = \"", rule, "\"` requires `metadata` column names.", call. = FALSE)
  }

  ## Independence is assessed within station and within species, so records with
  ## a missing value in either column are grouped together and filtered against
  ## each other. Report it and leave the data alone: distinct animals -- or
  ## distinct cameras -- would otherwise be merged into one event silently.
  for (col in c(station, species)) {
    values <- as.character(data[[col]])
    missing_value <- is.na(values) | !nzchar(trimws(values))
    if (any(missing_value)) {
      warning(sum(missing_value), " row(s) have a missing `", col,
              "`. Independence is assessed within station and species, so ",
              "these records are grouped together and filtered against each ",
              "other. Label them explicitly (for example \"unidentified\") if ",
              "that is not intended.", call. = FALSE)
    }
  }

  if (!is.null(record_id)) {
    id_keys <- list(as.character(data[[station]]))
    if (!is.null(species)) {
      id_keys <- c(id_keys, list(as.character(data[[species]])))
    }
    id_keys <- c(id_keys, list(as.character(data[[record_id]])))
    id_group <- do.call(paste, c(id_keys, sep = "\r"))
    duplicate_id <- !is.na(data[[record_id]]) &
      (duplicated(id_group) | duplicated(id_group, fromLast = TRUE))
    if (any(duplicate_id)) {
      stop(sum(duplicate_id), " row(s) share `", record_id,
           "` within a station-species group. Consolidate multiple annotation ",
           "rows for each photograph and species before filtering.",
           call. = FALSE)
    }
  }

  ## --- date-times -----------------------------------------------------------
  tt <- data[[datetime]]
  if (is.character(tt) || is.factor(tt)) {
    tt <- as.POSIXct(trimws(as.character(tt)), format = format, tz = tz)
  }
  if (!inherits(tt, "POSIXct")) {
    stop("`", datetime, "` must be POSIXct or character parseable with `format`.",
         call. = FALSE)
  }
  if (all(is.na(tt))) {
    stop("No values in `", datetime, "` could be parsed. Check `format`.",
         call. = FALSE)
  }
  n_bad <- sum(is.na(tt))
  if (n_bad) {
    warning(n_bad, " record(s) have an unparseable date-time and are each ",
            "treated as a separate event.", call. = FALSE)
  }

  ## --- metadata matrix ------------------------------------------------------
  meta <- NULL
  if (rule != "time_only") {
    meta <- data[, metadata, drop = FALSE]
    if (rule == "running_max") {
      not_num <- metadata[!vapply(meta, is.numeric, logical(1))]
      if (length(not_num)) {
        stop("`rule = \"running_max\"` needs numeric `metadata`; these are not: ",
             paste(not_num, collapse = ", "),
             ". Use `rule = \"any_change\"` for categorical metadata.",
             call. = FALSE)
      }
      if (any(as.matrix(meta) < 0, na.rm = TRUE)) {
        stop("Numeric `metadata` counts must be non-negative.", call. = FALSE)
      }
      meta <- as.matrix(meta)
      storage.mode(meta) <- "numeric"
    } else {
      ## any_change compares as character so factors, logicals and numbers
      ## all behave predictably.
      meta <- as.matrix(as.data.frame(lapply(meta, as.character),
                                      stringsAsFactors = FALSE))
    }
  }

  tot <- NULL
  if (!is.null(count)) {
    if (!is.numeric(data[[count]])) {
      stop("`count` must name a numeric column.", call. = FALSE)
    }
    tot <- as.numeric(data[[count]])
    if (any(tot < 0, na.rm = TRUE)) {
      stop("`count` values must be non-negative.", call. = FALSE)
    }
  }

  ## Group size used to calculate the observed count increment. Prefer an
  ## explicit count column; otherwise fall back to the sum of numeric metadata.
  size <- tot
  if (is.null(size) && length(metadata) &&
      all(vapply(data[, metadata, drop = FALSE], is.numeric, logical(1)))) {
    size_data <- data[, metadata, drop = FALSE]
    size <- rowSums(size_data, na.rm = TRUE)
    size[rowSums(!is.na(size_data)) == 0L] <- NA_real_
  }

  ## --- grouping -------------------------------------------------------------
  keys <- list(as.character(data[[station]]))
  if (!is.null(species)) keys <- c(keys, list(as.character(data[[species]])))
  grp <- do.call(paste, c(keys, sep = "\r"))

  independent <- logical(nrow(data))
  burst_id    <- integer(nrow(data))
  event_id    <- integer(nrow(data))
  count_increment <- rep(NA_real_, nrow(data))

  for (rows in split(seq_len(nrow(data)), grp)) {
    res <- .flag_one_group(
      times        = tt[rows],
      meta         = if (is.null(meta)) NULL else meta[rows, , drop = FALSE],
      total        = if (is.null(tot))  NULL else tot[rows],
      size         = if (is.null(size)) NULL else size[rows],
      threshold    = threshold,
      rule         = rule,
      compare_to   = compare_to,
      min_increase = min_increase,
      metadata_refractory = metadata_refractory
    )
    independent[rows] <- res$independent
    burst_id[rows]    <- res$burst
    event_id[rows]    <- res$event
    count_increment[rows] <- res$count_increment
  }

  data$independent <- independent
  data$event_id    <- event_id
  data$burst_id    <- burst_id
  data$count_increment <- count_increment
  ## Soft-deprecated alias retained so analyses written for versions <=0.2.0
  ## continue to run. New work should use the more accurate name above.
  data$n_new <- count_increment

  if (filter) data[data$independent, , drop = FALSE] else data
}


#' Internal worker: flag one station x species group
#'
#' Sequential by construction, because whether a record opens a new event
#' depends on the running state of the burst it belongs to. Written as an
#' explicit loop so the rule is auditable line by line.
#'
#' @noRd
.flag_one_group <- function(times, meta, total, size, threshold, rule,
                            compare_to, min_increase = 1,
                            metadata_refractory = 0) {

  n <- length(times)
  ord <- order(times, method = "radix")   # ties keep input order
  t_s <- times[ord]
  m_s <- if (is.null(meta))  NULL else meta[ord, , drop = FALSE]
  c_s <- if (is.null(total)) NULL else total[ord]
  s_s <- if (is.null(size))  NULL else size[ord]

  indep <- logical(n)
  burst <- integer(n)
  ## running maximum group size within the current burst, recorded after each
  ## record; used below to allocate observed count increments
  bmax  <- rep(NA_real_, n)
  indep[1] <- TRUE
  burst[1] <- 1L
  if (!is.null(s_s)) bmax[1] <- s_s[1]

  if (n > 1L) {
    max_meta  <- if (rule == "running_max") m_s[1, ] else NULL
    max_total <- if (rule == "running_max" && !is.null(c_s)) c_s[1] else NULL
    last_indep_time <- t_s[1]
    b <- 1L
    run_size <- if (is.null(s_s)) NA_real_ else s_s[1]

    for (i in 2:n) {

      ref <- if (compare_to == "last_record") t_s[i - 1] else last_indep_time
      gap <- as.numeric(difftime(t_s[i], ref, units = "mins"))

      new_burst <- is.na(gap) || gap > threshold

      if (new_burst) {
        indep[i] <- TRUE
        b <- b + 1L
        if (rule == "running_max") {
          max_meta  <- m_s[i, ]
          if (!is.null(c_s)) max_total <- c_s[i]
        }
        ## new burst: the running group-size maximum restarts
        if (!is.null(s_s)) run_size <- s_s[i]
      } else {
        ## isTRUE() so that NA metadata is treated as "no evidence of a new
        ## individual" rather than propagating NA into the flag.
        elapsed_since_event <- as.numeric(difftime(
          t_s[i], last_indep_time, units = "mins"
        ))
        metadata_allowed <- metadata_refractory == 0 ||
          (!is.na(elapsed_since_event) &&
             elapsed_since_event > metadata_refractory)
        metadata_evidence <- switch(
          rule,
          time_only   = FALSE,
          any_change  = isTRUE(any(m_s[i, ] != m_s[i - 1, ])),
          running_max = isTRUE(any(m_s[i, ] >= max_meta + min_increase)) ||
                        isTRUE(!is.null(c_s) && c_s[i] >= max_total + min_increase)
        )
        indep[i] <- metadata_allowed && metadata_evidence
        if (!is.null(s_s) && !is.na(s_s[i])) {
          run_size <- if (is.na(run_size)) s_s[i] else max(run_size, s_s[i])
        }
        if (rule == "running_max") {
          ## The running maximum absorbs every record in the burst, retained or
          ## not. This is what stops a value already seen from re-triggering.
          observed <- !is.na(m_s[i, ])
          if (any(observed)) {
            previously_missing <- observed & is.na(max_meta)
            max_meta[previously_missing] <- m_s[i, previously_missing]
            comparable <- observed & !is.na(max_meta)
            max_meta[comparable] <- pmax(max_meta[comparable],
                                         m_s[i, comparable])
          }
          if (!is.null(c_s) && !is.na(c_s[i])) {
            max_total <- if (is.na(max_total)) c_s[i] else max(max_total, c_s[i])
          }
        }
      }

      burst[i] <- b
      bmax[i]  <- run_size
      if (indep[i]) last_indep_time <- t_s[i]
    }
  }

  ## Every record carries the id of the event it belongs to, so dependent
  ## records can be aggregated back onto their event.
  event <- cumsum(indep)

  count_increment_s <- .allocate_count_increment(indep, burst, bmax,
                                                 has_size = !is.null(s_s))

  ## back to caller's row order
  out <- list(independent = logical(n), burst = integer(n),
              event = integer(n), count_increment = numeric(n))
  out$independent[ord] <- indep
  out$burst[ord]       <- burst
  out$event[ord]       <- event
  out$count_increment[ord] <- count_increment_s
  out
}


#' Internal: allocate the observed count increment to each event
#'
#' A group of three animals passing together is ONE encounter containing three
#' individuals, not three encounters. But if a rule splits a burst, naively
#' taking the group size of each resulting event double-counts the animals that
#' were already recorded. The quantity returned here is the increment: how far
#' the running maximum group size rose during this event relative to where it
#' stood when the event opened. Summing this over events recovers the maximum
#' observed count in the burst without duplicating a previously observed
#' maximum. It is not proof of individual identity.
#'
#' @noRd
.allocate_count_increment <- function(indep, burst, bmax, has_size) {

  n <- length(indep)
  if (!has_size) return(rep(NA_real_, n))

  starts <- which(indep)                    # events are consecutive runs, so
  ends   <- c(starts[-1L] - 1L, n)          # each run ends before the next
  ev_max     <- bmax[ends]
  ev_burst   <- burst[starts]
  prev_max   <- c(0, ev_max[-length(ev_max)])
  prev_burst <- c(NA_integer_, ev_burst[-length(ev_burst)])
  ## Only carry a known previous maximum forward within the same burst. If the
  ## earlier event had no count, the first later observed count is the first
  ## known maximum and is therefore measured from zero.
  same_burst <- !is.na(prev_burst) & ev_burst == prev_burst
  base <- rep(0, length(ev_max))
  known_previous <- same_burst & !is.na(prev_max)
  base[known_previous] <- prev_max[known_previous]
  rep(ev_max - base, times = ends - starts + 1L)
}
