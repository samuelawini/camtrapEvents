#' Simulated camera-trap records for worked examples
#'
#' An educational dataset with the structure of a camera-trap export and a
#' constructed encounter label.
#'
#' In \code{waterhole}, \code{true_group} records which simulated arrival
#' generated each photograph. Counting distinct values gives the constructed
#' target for demonstrating how the three rules behave.
#'
#' The data are simulated, not empirical validation. Constructed labels make
#' software behaviour testable, but they do not establish that one rule is
#' universally accurate. Accuracy depends on the observation process and should
#' also be assessed using simulations spanning plausible conditions and image
#' review based on evidence independent of the metadata being tested.
#'
#' The generating parameters were informed by a survey in Mole National Park,
#' Ghana, and four species span a range of simulated group sizes.
#'
#' @format A data frame with 4,471 rows and 9 columns:
#' \describe{
#'   \item{station}{camera identifier, three stations}
#'   \item{species}{scientific name, four species}
#'   \item{datetime}{POSIXct timestamp of the photograph}
#'   \item{males, females, juveniles}{animals of each class visible in this frame}
#'   \item{group_size}{total animals visible in this frame}
#'   \item{behaviour}{categorical, for demonstrating \code{rule = "any_change"}}
#'   \item{true_group}{constructed label identifying the simulated arrival.}
#' }
#'
#' @source Simulated by \code{data-raw/make_waterhole.R}, with parameters
#'   measured from the survey reported in Awini et al. (2026)
#'   \doi{10.1017/S0030605325102500}.
#'
#' @examples
#' data(waterhole)
#'
#' truth <- length(unique(waterhole$true_group))
#' truth
#'
#' # How close does each rule get?
#' for (r in c("time_only", "running_max", "any_change")) {
#'   n <- sum(independent_events(
#'     waterhole, "datetime", "station", "species",
#'     threshold = 30, rule = r,
#'     metadata = if (r == "time_only") NULL else c("males", "females", "juveniles"),
#'     count    = "group_size"
#'   )$independent)
#'   cat(sprintf("%-12s %4d events (truth %d, bias %+.1f%%)\n",
#'               r, n, truth, 100 * (n - truth) / truth))
#' }
"waterhole"
