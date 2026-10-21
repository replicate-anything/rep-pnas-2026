# replicateEverything provenance: connector
# Party-identification transition counts for Figures S11–S13.
# Execute with run_replication("here", "pid_transitions").

library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(ggalluvial, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")

build_pid_lodes <- function(df_input) {
  labs <- party_names
  from <- df_input |>
    select(id_w9, starts_with("party_id_w9")) |>
    pivot_longer(-id_w9, names_to = "var", values_to = "val") |>
    filter(val == 1) |>
    transmute(
      id_w9,
      from_party = recode(sub("^party_id_w9_(.*)$", "\\1", var), !!!labs)
    )
  to <- df_input |>
    select(id_w9, starts_with("party_id_w10")) |>
    pivot_longer(-id_w9, names_to = "var", values_to = "val") |>
    filter(val == 1) |>
    transmute(
      id_w9,
      to_party = recode(sub("^party_id_w10_(.*)$", "\\1", var), !!!labs)
    )
  transitions <- inner_join(from, to, by = "id_w9") |>
    count(`Wave 1` = from_party, `Wave 2` = to_party, name = "Freq")
  totN <- sum(transitions$Freq)
  share_w9 <- transitions |>
    group_by(`Wave 1`) |>
    summarise(s9 = sum(Freq) / totN, .groups = "drop")
  share_w10 <- transitions |>
    group_by(`Wave 2`) |>
    summarise(s10 = sum(Freq) / totN, .groups = "drop")
  net_pp <- share_w10 |>
    left_join(share_w9, by = c("Wave 2" = "Wave 1")) |>
    mutate(
      delta_pp = (s10 - s9) * 100,
      label_right = sprintf("%s (%+.1f pp)", `Wave 2`, delta_pp)
    ) |>
    select(`Wave 2`, label_right)
  lodes <- transitions |>
    mutate(Share = Freq / totN) |>
    to_lodes_form(axes = 1:2, id = "alluvium") |>
    mutate(
      x = factor(x, levels = c("Wave 1", "Wave 2")),
      stratum_lab = ifelse(
        x == "Wave 2",
        net_pp$label_right[match(stratum, net_pp$`Wave 2`)],
        as.character(stratum)
      )
    )
  party_cols <- c(
    "CDU/CSU" = "#000000",
    "SPD" = "#E3000F",
    "Grüne" = "#0DB02B",
    "FDP" = "#FFED00",
    "Linke" = "#E2382A",
    "AfD" = "#009EE0",
    "BSW" = "#18A19A"
  )
  list(lodes = lodes, party_cols = party_cols)
}

make_pid_transitions <- function(data) {
  df <- data |>
    mutate(citysize = as.numeric(citysize))
  out <- list(
    full = build_pid_lodes(df),
    treat = build_pid_lodes(df |> filter(workshop == 1)),
    control = build_pid_lodes(df |> filter(workshop == 0))
  )
  save_rds_atomic(out, study_path("outputs", "pid_transitions.rds"))
  out
}
