# replicateEverything provenance: connector
# Demeaned AfD triple interactions for Tables S10–S11.
# Execute with run_replication("here", "afd_3way").

source("./helpers_labels.R")
source("./helpers_estimation.R")

make_afd_3way <- function(data) {
  silence_fixest()

  prep_afd_3way <- function(df) {
    df |>
      filter(strategy_2 == strategies[1]) |>
      mutate(
        AFD_supporter = as.integer(party_preference_afd == 1),
        AFD_videos = as.integer(party == "AfD")
      ) |>
      mutate(
        treatment_demeaned = workshop_Treatment - mean(workshop_Treatment, na.rm = TRUE),
        AFD_supporter_demeaned = AFD_supporter - mean(AFD_supporter, na.rm = TRUE),
        AFD_videos_demeaned = AFD_videos - mean(AFD_videos, na.rm = TRUE)
      )
  }

  fit_afd_3way <- function(df, outcome_name) {
    feols(
      as.formula(paste0(
        v_outcomes[[outcome_name]],
        " ~ treatment_demeaned * AFD_supporter_demeaned * AFD_videos_demeaned | video"
      )),
      data = df,
      cluster = ~ID
    )
  }

  df_short <- prep_afd_3way(data$wave_9)
  df_long <- prep_afd_3way(data$wave_10)
  afd_3way_outcomes <- c("Like", "Share", "Bookmark", "Report")

  fit_set <- function(df) {
    lapply(afd_3way_outcomes, function(Y) tidy_fixest_one(fit_afd_3way(df, Y))) |>
      setNames(afd_3way_outcomes)
  }

  out <- list(short = fit_set(df_short), long = fit_set(df_long))
  save_rds_atomic(out, study_path("outputs", "afd_3way.rds"))
  out
}
