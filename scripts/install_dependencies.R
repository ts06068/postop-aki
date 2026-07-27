# Read package names and exact version pins from DESCRIPTION.
script_arguments <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_arguments[grepl("^--file=", script_arguments)])
repo_root <- dirname(dirname(normalizePath(script_file[[1L]], winslash = "/", mustWork = TRUE)))
description <- read.dcf(file.path(repo_root, "DESCRIPTION"))
dependency_fields <- intersect(c("Imports", "Suggests"), colnames(description))
fields <- unname(description[1L, dependency_fields])
specifications <- trimws(unlist(strsplit(fields, ",")))
packages <- sub("\\s*\\(.*$", "", specifications)
versions <- sub("^.*==[[:space:]]*([^)]*).*$", "\\1", specifications)
names(versions) <- packages

# Read raw DESCRIPTION text so hyphenated CRAN versions match the pins exactly.
installed_version <- function(package) {
  for (library in .libPaths()) {
    description_path <- file.path(library, package, "DESCRIPTION")
    if (!file.exists(description_path)) next
    return(unname(read.dcf(description_path, fields = "Version")[[1L]]))
  }
  NA_character_
}

installed_at_required_version <- vapply(packages, function(package) {
  identical(installed_version(package), versions[[package]])
}, logical(1))
to_install <- packages[!installed_at_required_version]

if (length(to_install)) {
  # Install into the first active library from the dated CRAN snapshot.
  snapshot <- "https://packagemanager.posit.co/cran/2025-10-01"
  message("Installing pinned dependencies: ", paste(to_install, collapse = ", "))
  utils::install.packages(to_install, lib = .libPaths()[[1L]], repos = snapshot)
} else {
  message("All pinned packages are already installed.")
}

# Fail early if the requested versions are still unavailable after installation.
incorrect <- packages[!vapply(packages, function(package) {
  identical(installed_version(package), versions[[package]])
}, logical(1))]
if (length(incorrect)) {
  stop("Pinned package versions were not restored: ", paste(incorrect, collapse = ", "), call. = FALSE)
}
