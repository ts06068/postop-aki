# Resolve from this file, not the caller's working directory.
script_arguments <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_arguments[grepl("^--file=", script_arguments)])
repo_root <- if (length(script_file)) {
  dirname(normalizePath(script_file[[1L]], winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

# Numeric filenames are the load order.
for (file in sort(list.files(file.path(repo_root, "R"), pattern = "\\.R$", full.names = TRUE))) {
  source(file, local = .GlobalEnv)
}

parse_cli_arguments <- function(arguments) {
  values <- list()
  for (argument in arguments) {
    if (!startsWith(argument, "--")) next
    stripped <- substring(argument, 3L)
    parts <- strsplit(stripped, "=", fixed = TRUE)[[1L]]
    key <- gsub("-", "_", parts[[1L]], fixed = TRUE)
    values[[key]] <- if (length(parts) == 1L) "true" else paste(parts[-1L], collapse = "=")
  }
  values
}

resolve_path <- function(path, root = repo_root) {
  if (grepl("^([A-Za-z]:[/\\\\]|/)", path)) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  normalizePath(file.path(root, path), winslash = "/", mustWork = FALSE)
}

# Fix contrasts and RNG state before any model runs.
options(contrasts = c("contr.treatment", "contr.poly"), warn = 1)
set.seed(20250331)
assert_packages()

cli <- parse_cli_arguments(commandArgs(trailingOnly = TRUE))
mode <- tolower(cli$mode %||% "sample")
if (!mode %in% c("sample", "full")) stop("`--mode` must be `sample` or `full`.", call. = FALSE)

# Full mode requires an external extract. Never fall back to sample.xlsx.
default_input <- if (mode == "sample") {
  file.path(repo_root, "data", "sample.xlsx")
} else {
  Sys.getenv("POSTOP_AKI_DATA_PATH", unset = "")
}
input_path <- cli$input %||% default_input
if (!nzchar(input_path)) {
  stop("Set POSTOP_AKI_DATA_PATH or pass --input=/path/to/analysis_dataset.xlsx.", call. = FALSE)
}
input_path <- resolve_path(input_path)

default_output <- Sys.getenv(
  "POSTOP_AKI_OUTPUT_DIR",
  unset = file.path("outputs", mode)
)
output_path <- resolve_path(cli$output %||% default_output)

message("Reading: ", input_path)
data <- load_analysis_data(input_path)
if (mode == "sample") assert_synthetic_sample(data)

# Write cheap summaries before expensive fits.
counts <- cohort_counts(data)

dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
write_result_csv(counts, file.path(output_path, "cohort_counts.csv"))
write_result_csv(baseline_table(data, "surgery_group"), file.path(output_path, "baseline_by_surgery.csv"))
write_result_csv(baseline_table(data, "aki"), file.path(output_path, "baseline_by_aki.csv"))
write_result_csv(length_of_stay_summary(data), file.path(output_path, "length_of_stay.csv"))

message("Fitting AKI risk-factor models...")
risk_results <- fit_aki_risk_models(data, mode)
write_result_csv(risk_results, file.path(output_path, "aki_risk_factors.csv"))

message("Fitting time-varying mortality models...")
mortality_results <- fit_time_varying_mortality(data, mode)
write_result_csv(mortality_results, file.path(output_path, "mortality_time_varying.csv"))

message("Fitting one-year landmark models...")
landmark_results <- fit_landmark_mortality(data, mortality_results, mode)
write_result_csv(landmark_results, file.path(output_path, "mortality_landmark.csv"))

message("Fitting ICU competing-risk models...")
icu_results <- fit_icu_competing_risk(data, mode)
write_result_csv(icu_results, file.path(output_path, "icu_competing_risk.csv"))

# Record the run after every analysis finishes.
write_result_csv(
  run_metadata(input_path, mode),
  file.path(output_path, "run_metadata.csv")
)

message("Analysis complete: ", output_path)
