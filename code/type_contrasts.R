# replicateEverything provenance: connector
# Wave 1 emotional-vs-other contrasts for Tables S5–S8.
# Execute with run_replication("here", "type_contrasts").

source("./helpers_labels.R")
source("./helpers_estimation.R")

make_type_contrasts <- function(data) {
  silence_fixest()
  compare_outcomes <- c("Like", "Report")
  compare_group_names <- c("ALL", names(party_codes))

  fit_type_contrast <- function(df, outcome_name, type_var) {
    y <- v_outcomes[[outcome_name]]
    feols(
      as.formula(paste0(y, " ~ workshop_Treatment * ", type_var, " | video")),
      data = df,
      cluster = ~ID
    )
  }

  pack <- function(fits) {
    lapply(fits[compare_group_names], tidy_fixest_one)
  }

  df_pol <- data |>
    filter(strategy_2 %in% strategies) |>
    mutate(emotional = as.integer(strategy_2 == strategies[1]))

  models_emo_neu <- lapply(compare_outcomes, function(Y) {
    fits <- c(
      list(ALL = fit_type_contrast(df_pol, Y, "emotional")),
      lapply(names(party_codes), function(P) {
        fit_type_contrast(df_pol |> filter(party == P), Y, "emotional")
      }) |> setNames(names(party_codes))
    )
    pack(fits)
  }) |>
    setNames(compare_outcomes)

  df_ee <- data |>
    filter(strategy_2 %in% c(strategies[1], "entertainment")) |>
    mutate(emotional = as.integer(strategy_2 == strategies[1]))

  models_emo_ent <- lapply(compare_outcomes, function(Y) {
    fits <- c(
      list(ALL = fit_type_contrast(df_ee, Y, "emotional")),
      lapply(names(party_codes), function(P) {
        df_p <- df_ee |> filter(party == P | strategy_2 == "entertainment")
        fit_type_contrast(df_p, Y, "emotional")
      }) |> setNames(names(party_codes))
    )
    pack(fits)
  }) |>
    setNames(compare_outcomes)

  out <- list(
    emo_neu_like = models_emo_neu$Like,
    emo_neu_report = models_emo_neu$Report,
    emo_ent_like = models_emo_ent$Like,
    emo_ent_report = models_emo_ent$Report
  )
  save_rds_atomic(out, study_path("outputs", "type_contrasts.rds"))
  out
}
