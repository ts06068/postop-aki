# Load the same numbered modules used by the command-line pipeline.
script_arguments <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_arguments[grepl("^--file=", script_arguments)])
repo_root <- dirname(dirname(normalizePath(script_file[[1L]], winslash = "/", mustWork = TRUE)))

for (file in sort(list.files(file.path(repo_root, "R"), pattern = "\\.R$", full.names = TRUE))) {
  source(file, local = .GlobalEnv)
}
assert_packages()

sample_path <- file.path(repo_root, "data", "sample.xlsx")
data <- load_analysis_data(sample_path)
assert_synthetic_sample(data)

# Pin the synthetic fixture shape and derived category boundaries.
stopifnot(nrow(data) == 96L)
stopifnot(identical(levels(data$age_group), c("<50", "50-59", "60-69", ">=70")))
stopifnot(identical(levels(data$bmi_group), c("<18.5", "18.5-22.9", "23.0-24.9", "25.0-29.9", ">=30")))
stopifnot(identical(levels(data$egfr_group), c(">=90", "15-29.9", "30-44.9", "45-59.9", "60-89.9")))
stopifnot(all(data$aki == as.integer(data$aki_stage > 0L)))

# Guard the cohort and AKI stage counts used by the smoke fixture.
counts <- cohort_counts(data)
stopifnot(identical(counts$n, c(96L, 32L, 64L)))
stopifnot(identical(counts$aki, c(26L, 8L, 18L)))
stopifnot(identical(counts$aki_stage_1, c(9L, 3L, 6L)))
stopifnot(identical(counts$aki_stage_2, c(9L, 3L, 6L)))
stopifnot(identical(counts$aki_stage_3, c(8L, 2L, 6L)))

# Cover ICU-first, death-first, censoring, and discordant follow-up cases.
endpoint_fixture <- data.frame(
  time_to_icu_days = c(10, 30, 20, 100, 30, 10),
  icu_admission = c(1L, 1L, 0L, 0L, 1L, 0L),
  time_to_death_days = c(30, 10, 20, 100, 10, 30),
  death = c(0L, 1L, 1L, 0L, 0L, 1L)
)
endpoint <- build_competing_endpoint(endpoint_fixture, horizon_day = 90, landmark_day = 7)
stopifnot(identical(endpoint$competing_status, c(1L, 2L, 2L, 0L, 0L, 0L)))
stopifnot(identical(endpoint$competing_time, c(3, 3, 13, 83, 3, 3)))

# Parse modules even when the smoke path does not call every analysis branch.
for (file in list.files(file.path(repo_root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  invisible(parse(file = file))
}
invisible(parse(file = file.path(repo_root, "run_all.R")))

message("All pipeline tests passed.")
