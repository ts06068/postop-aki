# Input loading, schema checks, and derived variables.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || identical(x, "")) y else x
}

required_packages <- function() {
  c("cmprsk", "data.table", "readxl", "survival")
}

assert_packages <- function(packages = required_packages()) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing R packages: ", paste(missing, collapse = ", "),
      ". Run: Rscript scripts/install_dependencies.R",
      call. = FALSE
    )
  }
}

# Current column names and accepted old aliases.
legacy_column_candidates <- list(
  hospital_code = c("hospital_code", "HospitalCode"),
  patient_id = c("patient_id", "PID"),
  operation_date = c("operation_date", "OperationDate"),
  cardiac_surgery = c("cardiac_surgery", "CardiacSurgery"),
  icd9cm_code = c("icd9cm_code", "ICD9CMCode"),
  age = c("age", "Age"),
  sex_male = c("sex_male", "Sex"),
  bmi = c("bmi", "BMI"),
  asa_class = c("asa_class", "ASAClassCode", "ASA_cat"),
  nsaid = c("nsaid", "NSAIDS"),
  rasb = c("rasb", "ARB"),
  asthma = c("asthma", "Asthma"),
  cva = c("cva", "CVA"),
  diabetes = c("diabetes", "DM"),
  hepatitis = c("hepatitis", "Hepatitis"),
  hypertension = c("hypertension", "Hypertension"),
  neurologic_disease = c("neurologic_disease", "NeurologicDisease"),
  pulmonary_tuberculosis = c("pulmonary_tuberculosis", "PulTbc"),
  malignancy = c("malignancy", "Malignancy"),
  egfr = c("egfr", "eGFR"),
  systolic_bp = c("systolic_bp", "SBP"),
  diastolic_bp = c("diastolic_bp", "DBP"),
  operation_duration_hours = c("operation_duration_hours", "Duration"),
  urgent_surgery = c("urgent_surgery", "Urgency"),
  packed_rbc = c("packed_rbc", "BloodpRBC", "Blood-pRBC_merged"),
  time_below_map65_minutes = c("time_below_map65_minutes", "TimeBelow65"),
  aki_stage = c("aki_stage", "AKIStage"),
  aki = c("aki", "AKI_binary"),
  time_to_death_days = c("time_to_death_days", "TimeToDeath"),
  death = c("death", "DeathStatus"),
  time_to_icu_days = c("time_to_icu_days", "TimeToICU"),
  icu_admission = c("icu_admission", "ICUStatus"),
  total_los_days = c("total_los_days", "LOS"),
  postoperative_los_days = c("postoperative_los_days", "Op-Out")
)

required_input_columns <- function() {
  c(
    "hospital_code", "patient_id", "operation_date", "cardiac_surgery",
    "age", "sex_male", "bmi", "asa_class", "nsaid", "rasb", "asthma",
    "cva", "diabetes", "hepatitis", "hypertension", "neurologic_disease",
    "pulmonary_tuberculosis", "malignancy", "egfr",
    "operation_duration_hours", "urgent_surgery", "packed_rbc", "time_below_map65_minutes",
    "aki_stage", "aki", "time_to_death_days", "death", "time_to_icu_days",
    "icu_admission", "total_los_days", "postoperative_los_days"
  )
}

read_analysis_data <- function(path) {
  assert_packages(c("data.table", "readxl"))
  if (!file.exists(path)) stop("Input file does not exist: ", path, call. = FALSE)

  extension <- tolower(tools::file_ext(path))
  if (extension %in% c("xlsx", "xls")) {
    data <- as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
  } else if (extension %in% c("csv", "txt")) {
    # A pipe in the header marks an old export.
    first_line <- readLines(path, n = 1L, warn = FALSE)
    separator <- if (grepl("|", first_line, fixed = TRUE)) "|" else ","
    data <- as.data.frame(
      data.table::fread(path, sep = separator, na.strings = c("", "NA", "NaN")),
      stringsAsFactors = FALSE
    )
  } else {
    stop("Supported input formats are CSV, pipe-delimited CSV, XLSX, and XLS.", call. = FALSE)
  }

  normalize_input_columns(data)
}

normalize_input_columns <- function(data) {
  names(data) <- trimws(names(data))
  # Drop index junk written by pandas or R.
  index_columns <- grepl("^(\\.\\.\\.[0-9]+|Unnamed:|X$)", names(data))
  if (any(index_columns)) data <- data[, !index_columns, drop = FALSE]

  # Keep current names. Otherwise take the first matching alias.
  for (target in names(legacy_column_candidates)) {
    if (target %in% names(data)) next
    candidates <- legacy_column_candidates[[target]]
    match <- candidates[candidates %in% names(data)]
    if (length(match)) names(data)[names(data) == match[[1L]]] <- target
  }

  missing <- setdiff(required_input_columns(), names(data))
  if (length(missing)) {
    stop("Input is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  data
}

as_numeric_strict <- function(x, name) {
  if (is.numeric(x)) return(as.numeric(x))
  output <- suppressWarnings(as.numeric(as.character(x)))
  invalid <- !is.na(x) & is.na(output)
  if (any(invalid)) {
    stop("Column `", name, "` contains non-numeric values.", call. = FALSE)
  }
  output
}

as_binary_strict <- function(x, name) {
  output <- as_numeric_strict(x, name)
  observed <- unique(output[!is.na(output)])
  if (!all(observed %in% c(0, 1))) {
    stop("Column `", name, "` must contain only 0, 1, or NA.", call. = FALSE)
  }
  as.integer(output)
}

derive_surgery_specialty <- function(code, cardiac_surgery) {
  text <- trimws(as.character(code))
  numeric_code <- suppressWarnings(as.numeric(text))
  # Reduce decimal or compact ICD-9-CM codes to two-digit categories.
  major <- ifelse(
    grepl("\\.", text),
    floor(numeric_code),
    ifelse(nchar(gsub("[^0-9]", "", text)) >= 3L, floor(numeric_code / 100), numeric_code)
  )

  specialty <- rep("other", length(major))
  specialty[major >= 41 & major <= 54] <- "abdominal"
  specialty[major >= 76 & major <= 84] <- "orthopedic"
  specialty[major >= 55 & major <= 64] <- "urologic"
  specialty[major %in% c(1:5, 7)] <- "neurosurgery"
  specialty[major %in% 38:39] <- "vascular"
  specialty[major >= 30 & major <= 34] <- "thoracic"
  specialty[major >= 35 & major <= 37] <- "cardiac"
  # Trust the curated cardiac flag over the procedure code.
  specialty[cardiac_surgery == 1L] <- "cardiac"
  factor(
    specialty,
    levels = c("cardiac", "abdominal", "orthopedic", "urologic", "neurosurgery", "vascular", "thoracic", "other")
  )
}

derive_analysis_variables <- function(data) {
  binary_columns <- c(
    "cardiac_surgery", "sex_male", "nsaid", "rasb", "asthma", "cva",
    "diabetes", "hepatitis", "hypertension", "neurologic_disease",
    "pulmonary_tuberculosis", "malignancy", "urgent_surgery", "aki",
    "death", "icu_admission"
  )
  binary_columns <- intersect(binary_columns, names(data))
  for (name in binary_columns) data[[name]] <- as_binary_strict(data[[name]], name)

  numeric_columns <- c(
    "age", "bmi", "asa_class", "egfr", "systolic_bp", "diastolic_bp",
    "operation_duration_hours", "packed_rbc", "time_below_map65_minutes",
    "aki_stage", "time_to_death_days", "time_to_icu_days", "total_los_days",
    "postoperative_los_days"
  )
  numeric_columns <- intersect(numeric_columns, names(data))
  for (name in numeric_columns) data[[name]] <- as_numeric_strict(data[[name]], name)

  data$operation_date <- as.Date(data$operation_date)
  if (anyNA(data$operation_date)) stop("`operation_date` contains missing or invalid dates.", call. = FALSE)
  data$operation_year <- as.integer(format(data$operation_date, "%Y"))

  if (any(data$age < 18, na.rm = TRUE)) stop("The analytic cohort contains age < 18.", call. = FALSE)
  if (any(data$egfr < 15, na.rm = TRUE)) stop("The analytic cohort contains baseline eGFR < 15.", call. = FALSE)
  if (any(data$time_to_death_days < 0, na.rm = TRUE) || any(data$time_to_icu_days < 0, na.rm = TRUE)) {
    stop("Follow-up times cannot be negative.", call. = FALSE)
  }

  # `aki` wins. Warn on disagreement and clamp stages to 0..3.
  stage_any <- as.integer(data$aki_stage > 0)
  inconsistent <- !is.na(stage_any) & !is.na(data$aki) & stage_any != data$aki
  if (any(inconsistent)) {
    warning(sum(inconsistent), " rows have inconsistent `aki` and `aki_stage`; `aki` is retained as authoritative.")
  }
  data$aki_stage <- as.integer(pmin(pmax(data$aki_stage, 0), 3))

  # Left-closed intervals put boundary values in the higher band.
  data$age_group <- cut(
    data$age,
    breaks = c(-Inf, 50, 60, 70, Inf),
    right = FALSE,
    labels = c("<50", "50-59", "60-69", ">=70")
  )
  data$bmi_group <- cut(
    data$bmi,
    breaks = c(-Inf, 18.5, 23, 25, 30, Inf),
    right = FALSE,
    labels = c("<18.5", "18.5-22.9", "23.0-24.9", "25.0-29.9", ">=30")
  )
  data$egfr_group <- cut(
    data$egfr,
    breaks = c(15, 30, 45, 60, 90, Inf),
    right = FALSE,
    include.lowest = TRUE,
    labels = c("15-29.9", "30-44.9", "45-59.9", "60-89.9", ">=90")
  )
  data$egfr_group <- stats::relevel(data$egfr_group, ref = ">=90")
  data$asa_group <- factor(
    ifelse(data$asa_class >= 4, "4+", as.character(as.integer(data$asa_class))),
    levels = c("1", "2", "3", "4+")
  )

  # Models need exposure flags, not raw amounts.
  data$rbc_transfusion <- as.integer(data$packed_rbc > 0)
  data$intraoperative_hypotension <- as.integer(data$time_below_map65_minutes > 0)
  data$surgery_group <- factor(
    ifelse(data$cardiac_surgery == 1L, "cardiac", "non_cardiac"),
    levels = c("non_cardiac", "cardiac")
  )
  data$hospital_factor <- factor(data$hospital_code)

  if ("icd9cm_code" %in% names(data)) {
    data$surgery_specialty <- derive_surgery_specialty(data$icd9cm_code, data$cardiac_surgery)
  }

  data
}

load_analysis_data <- function(path) {
  derive_analysis_variables(read_analysis_data(path))
}

sha256_file <- function(path) {
  # Hash the input with the operating system tool.
  command <- if (.Platform$OS.type == "windows") "certutil" else "sha256sum"
  if (.Platform$OS.type == "windows") {
    output <- suppressWarnings(system2(command, c("-hashfile", shQuote(normalizePath(path)), "SHA256"), stdout = TRUE, stderr = TRUE))
    # The hash is on line 2. certutil status text depends on locale.
    hash <- if (length(output) >= 2L) gsub(" ", "", output[[2L]], fixed = TRUE) else character()
    if (!grepl("^[A-Fa-f0-9]{64}$", hash)) hash <- character()
  } else {
    output <- suppressWarnings(system2(command, shQuote(normalizePath(path)), stdout = TRUE, stderr = TRUE))
    hash <- strsplit(output[[1L]], "\\s+")[[1L]][[1L]]
  }
  if (length(hash)) tolower(hash[[1L]]) else NA_character_
}
