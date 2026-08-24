################################################################################
##  Generates `waterhole`, the example dataset shipped with camtrapEvents.
##
##  The dataset that motivated this package (Mole National Park, Ghana) cannot
##  be released, because it contains precise locations of threatened species.
##  This is an educational simulation with the same broad structure, generated
##  from parameters informed by that survey:
##
##    per-frame composition refresh chance     2.83 %
##    mean exponential gap between bursts    1182 min
##    mean records per burst                    6.63
##    mean burst duration                      14.1 min
##
##  Four species span a group-size gradient. The constructed generating label is
##  retained in `true_group`, so users can inspect rule behaviour. This dataset
##  is not presented as empirical validation of rule accuracy.
##
##  Run with: source("data-raw/make_waterhole.R")
################################################################################

## Pin the generator explicitly, not just the seed: R has changed defaults
## before (the sample() algorithm in 3.6.0). Verified to reproduce the shipped
## data/waterhole.rda byte-identically under R 4.6.1.
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(4242)

GAP_LONG  <- 1182  # exponential mean in this educational generator
LINGER_MU <- 14.1
FRAMES_MU <- 6.63
P_CHANGE  <- 0.0283
P_VISIBLE <- 0.75

## Species, mean group size, and probability that the next arrival occurs within
## 30 minutes after the preceding simulated group departs.
spp <- data.frame(
  species    = c("Kobus kob", "Papio anubis", "Tragelaphus scriptus",
                 "Panthera pardus"),
  mean_group = c(6, 8, 2, 1),
  overlap    = c(0.20, 0.25, 0.08, 0.02),
  stringsAsFactors = FALSE
)

stations <- c("CAM01", "CAM02", "CAM03")

sim_one <- function(species, mean_group, overlap, station, n_groups, gid0) {
  t_now <- 0; out <- list(); gid <- gid0
  for (g in seq_len(n_groups)) {
    t_now <- t_now + if (g > 1L && runif(1) < overlap)
      runif(1, 0, 30) else rexp(1, rate = 1 / GAP_LONG)

    n_tot <- max(1L, rpois(1, mean_group))
    n_juv <- rbinom(1, n_tot, 0.15)
    n_ad  <- n_tot - n_juv
    n_m   <- rbinom(1, n_ad, 0.5)
    n_f   <- n_ad - n_m

    linger <- rexp(1, rate = 1 / LINGER_MU)
    nf     <- 1L + rpois(1, FRAMES_MU - 1)
    fire   <- if (nf == 1L) 0 else c(0, sort(runif(nf - 1L, 0, linger)))

    hm <- rbinom(1, n_m, 1 - P_VISIBLE)
    hf <- rbinom(1, n_f, 1 - P_VISIBLE)
    hj <- rbinom(1, n_juv, 1 - P_VISIBLE)

    seen <- FALSE
    for (dt in fire) {
      if (runif(1) < P_CHANGE) {
        hm <- rbinom(1, n_m, 1 - P_VISIBLE)
        hf <- rbinom(1, n_f, 1 - P_VISIBLE)
        hj <- rbinom(1, n_juv, 1 - P_VISIBLE)
      }
      om <- n_m - hm; of <- n_f - hf; oj <- n_juv - hj
      if (om + of + oj == 0L) next
      out[[length(out) + 1L]] <- data.frame(
        station = station, species = species, datetime = t_now + dt,
        males = om, females = of, juveniles = oj,
        group_size = om + of + oj,
        behaviour = sample(c("passing", "drinking", "foraging"), 1,
                           prob = c(0.6, 0.25, 0.15)),
        true_group = gid, stringsAsFactors = FALSE
      )
      seen <- TRUE
    }
    if (seen) gid <- gid + 1L
    t_now <- t_now + linger
  }
  list(dat = do.call(rbind, out), next_gid = gid)
}

rows <- list(); gid <- 1L
for (st in stations) {
  for (i in seq_len(nrow(spp))) {
    r <- sim_one(spp$species[i], spp$mean_group[i], spp$overlap[i], st,
                 n_groups = 60, gid0 = gid)
    rows[[length(rows) + 1L]] <- r$dat
    gid <- r$next_gid
  }
}

waterhole <- do.call(rbind, rows)
waterhole$datetime <- as.POSIXct("2021-02-01 00:00:00", tz = "UTC") +
  waterhole$datetime * 60
waterhole <- waterhole[order(waterhole$station, waterhole$datetime), ]
rownames(waterhole) <- NULL

cat("records:", nrow(waterhole),
    "| true groups:", length(unique(waterhole$true_group)),
    "| species:", length(unique(waterhole$species)), "\n")

save(waterhole, file = "data/waterhole.rda", version = 2, compress = "xz")
