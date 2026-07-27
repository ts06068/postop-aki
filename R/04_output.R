# Refuse sample mode unless every record uses the reserved synthetic ID prefix.
assert_synthetic_sample <- function(data) {
  if (!all(startsWith(as.character(data$patient_id), "SYNTH_"))) {
    stop("Sample mode accepts only records whose patient_id begins with `SYNTH_`.", call. = FALSE)
  }
  invisible(TRUE)
}

write_result_csv <- function(data, path) {
  directory <- dirname(path)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(data, path, na = "NA")
  invisible(path)
}

package_versions <- function(packages = required_packages()) {
  paste(
    vapply(packages, function(package) {
      if (requireNamespace(package, quietly = TRUE)) {
        paste0(package, "=", as.character(utils::packageVersion(package)))
      } else {
        paste0(package, "=missing")
      }
    }, character(1)),
    collapse = ";"
  )
}

# Record the input fingerprint and software environment beside each analysis run.
run_metadata <- function(input_path, mode) {
  data.frame(
    field = c(
      "created_at_utc", "mode", "input_path", "input_sha256", "r_version",
      "platform", "package_versions"
    ),
    value = c(
      format(Sys.time(), tz = "UTC", usetz = TRUE),
      mode,
      normalizePath(input_path, winslash = "/", mustWork = TRUE),
      sha256_file(input_path),
      R.version.string,
      R.version$platform,
      package_versions()
    ),
    stringsAsFactors = FALSE
  )
}
