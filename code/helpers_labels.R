# replicateEverything provenance: connector
# Display labels and the file writers shared by saved products.
# Plot helpers: helpers_graphing.R. Shared model tables: helpers_estimation.R.
# Figure 1 estimator: fig_1.R.

strategies <- c("political\n(emotional)", "political\n(neutral)")

v_outcomes <- c(
  "Like" = "videoLikep1r",
  "Share" = "videoSharep1r",
  "Bookmark" = "videoBookmarkp1r",
  "Report" = "videoCommentTickedp1r",
  "Watch Time" = "watch_percentage_",
  "Skip" = "skipped_video_"
)

l_outcomegroups <- list(
  main = c("Like", "Report", "Share", "Bookmark"),
  "_other" = c("Skip", "Watch Time")
)

horizon_wave_levels <- c("Wave 1", "Wave 2")

party_codes <- c(
  "AfD" = "afd",
  "CDU/CSU" = "cdu",
  "Linke" = "linke",
  "SPD" = "spd",
  "Grüne" = "gruene",
  "FDP" = "fdp",
  "BSW" = "bsw"
)

party_names <- c(
  afd = "AfD",
  cdu = "CDU/CSU",
  linke = "Linke",
  spd = "SPD",
  gruene = "Grüne",
  fdp = "FDP",
  bsw = "BSW"
)

strategy_order_article <- c(
  "political\n(emotional)",
  "political\n(neutral)",
  "entertainment"
)

type_contrast_coef_map <- c(
  "workshop_Treatment" = "Treatment (comparison type)",
  "workshop_Treatment:emotional" = "Treatment × type"
)

afd_3way_coef_map <- c(
  "treatment_demeaned" = "Treatment (demeaned)",
  "AFD_supporter_demeaned" = "AfD supporter (demeaned)",
  "treatment_demeaned:AFD_supporter_demeaned" = "Treatment × AfD supporter (demeaned)",
  "treatment_demeaned:AFD_videos_demeaned" = "Treatment × AfD video (demeaned)",
  "AFD_supporter_demeaned:AFD_videos_demeaned" = "AfD supporter × AfD video (demeaned)",
  "treatment_demeaned:AFD_supporter_demeaned:AFD_videos_demeaned" =
    "Treatment × AfD supporter × AfD video (demeaned)"
)

labels_en <- c(
  alter = "Age",
  gender_female = "Female (vs male)",
  citysize = "City size",
  east = "East (vs West)",
  state_baden_wuerttemberg = "State: Baden-Württemberg",
  state_bayern = "State: Bavaria",
  state_berlin = "State: Berlin",
  state_brandenburg = "State: Brandenburg",
  state_bremen = "State: Bremen",
  state_hamburg = "State: Hamburg",
  state_hessen = "State: Hesse",
  state_mecklenburg_vorpommern = "State: Mecklenburg–Western Pomerania",
  state_niedersachsen = "State: Lower Saxony",
  state_nordrhein_westfalen = "State: North Rhine-Westphalia",
  state_rheinland_pfalz = "State: Rhineland-Palatinate",
  state_saarland = "State: Saarland",
  state_sachsen = "State: Saxony",
  state_sachsen_anhalt = "State: Saxony-Anhalt",
  state_schleswig_holstein = "State: Schleswig-Holstein",
  state_thueringen = "State: Thuringia",
  urban_ballungsraum = "Urban: metropolitan area",
  urban_stadtregion = "Urban: city region",
  urban_mittelzentrum = "Urban: medium-sized center",
  urban_high = "High urbanization",
  video_userfacebook = "Uses Facebook",
  video_usertiktok = "Uses TikTok",
  video_usersnapchat = "Uses Snapchat",
  video_userpinterest = "Uses Pinterest",
  video_userx_ehem_twitter = "Uses X/Twitter",
  video_usertwitch = "Uses Twitch",
  politikinteresse = "Political interest",
  lr = "Left–right ideology",
  competence_cdu_csu = "Competent party: CDU/CSU",
  competence_spd = "Competent party: SPD",
  competence_afd = "Competent party: AfD",
  competence_greens = "Competent party: Greens",
  competence_fdp = "Competent party: FDP",
  competence_left = "Competent party: The Left",
  competence_bsw = "Competent party: BSW",
  party_id_w9_cdu = "Party ID: CDU/CSU",
  party_id_w9_spd = "Party ID: SPD",
  party_id_w9_afd = "Party ID: AfD",
  party_id_w9_gruene = "Party ID: Greens",
  party_id_w9_fdp = "Party ID: FDP",
  party_id_w9_linke = "Party ID: The Left",
  party_id_w9_bsw = "Party ID: BSW",
  ptv_w9_pre_cdu = "PTV CDU/CSU (pre)",
  ptv_w9_pre_spd = "PTV SPD (pre)",
  ptv_w9_pre_linke = "PTV The Left (pre)",
  ptv_w9_pre_gruene = "PTV Greens (pre)",
  ptv_w9_pre_fdp = "PTV FDP (pre)",
  ptv_w9_pre_afd = "PTV AfD (pre)",
  ptv_w9_pre_bsw = "PTV BSW (pre)",
  prior_performance_general_2 = "Government performance (prior)",
  solidaritat = "Solidarity",
  sozialer_zusammenhalt = "Social cohesion",
  migration = "Immigration attitudes",
  tiktok_user_yes = "Current TikTok user",
  tiktok_user_no = "Not a TikTok user",
  tiktok_potential_yes = "Open to getting a TikTok account",
  tiktok_potential_no = "Not open to getting a TikTok account",
  employment_fulltime = "Employment: full-time",
  employment_parttime = "Employment: part-time",
  employment_marginal = "Employment: marginal/irregular",
  employment_training_service = "Employment: training/service",
  employment_not_working = "Not currently employed",
  occupation_worker = "Occupation: worker",
  occupation_employee = "Occupation: employee",
  occupation_public_service = "Occupation: public service",
  occupation_self_employed = "Occupation: self-employed",
  school_education_level = "School education level",
  vocational_basic = "Vocational education: basic",
  vocational_advanced = "Vocational education: advanced",
  vocational_other = "Vocational education: other",
  vocational_none = "No vocational qualification",
  eigene_kinder = "Has children",
  haushaltsgrobe = "Household size",
  migrationshintergrund_eltern = "Parental migration background",
  haushaltseinkommen = "Household income"
)

engagement_outcomes <- function() {
  c(l_outcomegroups$main, l_outcomegroups[["_other"]])
}

study_path <- function(...) {
  root <- Sys.getenv("REPLICATE_STUDY_ROOT", unset = "")
  if (!nzchar(root)) {
    root <- getwd()
  }
  file.path(root, ...)
}

replace_file <- function(from, to) {
  if (file.exists(to) && unlink(to) != 0) {
    file.rename(to, paste0(to, ".old"))
  }
  if (!isTRUE(file.rename(from, to))) {
    file.copy(from, to, overwrite = TRUE)
    unlink(from)
  }
  invisible(to)
}

save_rds_atomic <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  saveRDS(object, tmp)
  replace_file(tmp, path)
}

write_text_atomic <- function(text, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  writeLines(text, tmp, useBytes = TRUE)
  replace_file(tmp, path)
}

save_ggplot <- function(plot, path, width, height) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp.png")
  ggplot2::ggsave(
    tmp,
    plot = plot,
    width = width,
    height = height,
    dpi = 150,
    device = "png",
    bg = "white"
  )
  replace_file(tmp, path)
  invisible(path)
}
