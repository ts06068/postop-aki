# Cohort counts and descriptive tables written before model fitting.
safe_p_value <- function(expression) {
  # Sparse sample tables should render with NA instead of stopping the pipeline.
  tryCatch(as.numeric(expression), error = function(...) NA_real_, warning = function(w) {
    suppressWarnings(tryCatch(as.numeric(expression), error = function(...) NA_real_))
  })
}

cohort_counts <- function(data) {
  groups <- list(
    overall = rep(TRUE, nrow(data)),
    cardiac = data$cardiac_surgery == 1L,
    non_cardiac = data$cardiac_surgery == 0L
  )

  rows <- lapply(names(groups), function(group_name) {
    subset <- data[groups[[group_name]], , drop = FALSE]
    data.frame(
      cohort = group_name,
      n = nrow(subset),
      aki = sum(subset$aki == 1L, na.rm = TRUE),
      aki_stage_1 = sum(subset$aki_stage == 1L, na.rm = TRUE),
      aki_stage_2 = sum(subset$aki_stage == 2L, na.rm = TRUE),
      aki_stage_3 = sum(subset$aki_stage == 3L, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

baseline_specification <- function(group_variable) {
  specification <- list(
    sex_male = "categorical",
    age_group = "categorical",
    bmi_group = "categorical",
    nsaid = "categorical",
    rasb = "categorical",
    hypertension = "categorical",
    asthma = "categorical",
    cva = "categorical",
    diabetes = "categorical",
    hepatitis = "categorical",
    neurologic_disease = "categorical",
    pulmonary_tuberculosis = "categorical",
    malignancy = "categorical",
    egfr_group = "categorical",
    systolic_bp = "continuous",
    diastolic_bp = "continuous",
    asa_group = "categorical",
    operation_duration_hours = "continuous",
    cardiac_surgery = "categorical",
    rbc_transfusion = "categorical",
    intraoperative_hypotension = "categorical",
    aki = "categorical"
  )
  # Do not summarize the same variable that defines the table columns.
  specification[names(specification) != group_variable]
}

format_mean_sd <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_character_)
  sprintf("%.2f (%.2f)", mean(x), stats::sd(x))
}

format_count_percent <- function(x, level) {
  denominator <- sum(!is.na(x))
  if (!denominator) return(NA_character_)
  numerator <- sum(as.character(x) == as.character(level), na.rm = TRUE)
  sprintf("%d (%.2f%%)", numerator, 100 * numerator / denominator)
}

baseline_table <- function(data, group_variable) {
  if (!group_variable %in% names(data)) stop("Unknown grouping variable: ", group_variable, call. = FALSE)
  group <- droplevels(factor(data[[group_variable]]))
  group_levels <- levels(group)
  if (length(group_levels) < 2L) stop("Baseline table needs at least two groups.", call. = FALSE)

  specification <- baseline_specification(group_variable)
  rows <- list()
  row_index <- 1L

  for (variable in names(specification)) {
    if (!variable %in% names(data)) next
    values <- data[[variable]]
    type <- specification[[variable]]

    if (identical(type, "continuous")) {
      # With two groups, one-way ANOVA gives the pooled two-sample t-test p-value.
      p_value <- safe_p_value(summary(stats::aov(values ~ group))[[1L]][["Pr(>F)"]][[1L]])
      row <- data.frame(
        variable = variable,
        level = "mean (SD)",
        overall = format_mean_sd(values),
        p_value = p_value,
        stringsAsFactors = FALSE
      )
      for (level in group_levels) {
        row[[paste0("group_", make.names(level))]] <- format_mean_sd(values[group == level])
      }
      rows[[row_index]] <- row
      row_index <- row_index + 1L
      next
    }

    categorical <- factor(values)
    value_levels <- levels(categorical)
    # Binary variables display the event level only; factors display every level.
    if (all(value_levels %in% c("0", "1"))) value_levels <- "1"
    # The same omnibus chi-square p-value applies to all displayed factor levels.
    p_value <- safe_p_value(stats::chisq.test(table(categorical, group), correct = FALSE)$p.value)

    for (level in value_levels) {
      row <- data.frame(
        variable = variable,
        level = level,
        overall = format_count_percent(categorical, level),
        p_value = p_value,
        stringsAsFactors = FALSE
      )
      for (group_level in group_levels) {
        row[[paste0("group_", make.names(group_level))]] <- format_count_percent(
          categorical[group == group_level], level
        )
      }
      rows[[row_index]] <- row
      row_index <- row_index + 1L
    }
  }

  output <- data.table::rbindlist(rows, fill = TRUE)
  as.data.frame(output, stringsAsFactors = FALSE)
}

length_of_stay_summary <- function(data) {
  rows <- list()
  row_index <- 1L

  # Summarize median and IQR by surgery cohort, AKI status, and LOS outcome.
  for (cohort in c("overall", "cardiac", "non_cardiac")) {
    cohort_mask <- switch(
      cohort,
      overall = rep(TRUE, nrow(data)),
      cardiac = data$cardiac_surgery == 1L,
      non_cardiac = data$cardiac_surgery == 0L
    )
    for (aki_value in c(0L, 1L)) {
      mask <- cohort_mask & data$aki == aki_value
      for (outcome in c("total_los_days", "postoperative_los_days")) {
        values <- data[[outcome]][mask]
        values <- values[is.finite(values)]
        quantiles <- if (length(values)) stats::quantile(values, c(0.25, 0.5, 0.75), names = FALSE) else rep(NA_real_, 3L)
        rows[[row_index]] <- data.frame(
          cohort = cohort,
          aki = aki_value,
          outcome = outcome,
          n = length(values),
          median = quantiles[[2L]],
          q1 = quantiles[[1L]],
          q3 = quantiles[[3L]],
          stringsAsFactors = FALSE
        )
        row_index <- row_index + 1L
      }
    }
  }
  do.call(rbind, rows)
}
