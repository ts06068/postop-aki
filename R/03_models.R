# AKI risk, mortality, landmark, and ICU outcome models.
full_adjustment_terms <- function(data, cohort = "overall", mode = "full") {
  # The 96-row sample tests code paths. Do not overfit it.
  if (identical(mode, "sample")) {
    terms <- c("sex_male", "age", "bmi", "egfr", "asa_class", "cardiac_surgery")
  } else {
    terms <- c(
      "sex_male", "age_group", "bmi_group", "nsaid", "rasb", "hypertension",
      "asthma", "cva", "diabetes", "hepatitis", "neurologic_disease",
      "pulmonary_tuberculosis", "malignancy", "egfr_group", "asa_group",
      "operation_duration_hours", "urgent_surgery", "rbc_transfusion",
      "intraoperative_hypotension", "cardiac_surgery"
    )
  }
  # Drop surgery type inside a fixed surgery cohort. A constant term is useless.
  if (!identical(cohort, "overall")) terms <- setdiff(terms, "cardiac_surgery")
  intersect(terms, names(data))
}

cohort_subset <- function(data, cohort) {
  output <- switch(
    cohort,
    overall = data,
    cardiac = data[data$cardiac_surgery == 1L, , drop = FALSE],
    non_cardiac = data[data$cardiac_surgery == 0L, , drop = FALSE],
    stop("Unknown cohort: ", cohort, call. = FALSE)
  )
  droplevels(output)
}

formula_from_terms <- function(response, terms) {
  stats::as.formula(paste(response, "~", paste(terms, collapse = " + ")))
}

coefficient_matches <- function(coefficient, variable) {
  coefficient == variable | startsWith(coefficient, variable)
}

tidy_glm_fit <- function(fit, cohort, model, focus = NULL) {
  coefficients <- stats::coef(fit)
  standard_errors <- sqrt(diag(stats::vcov(fit)))
  keep <- names(coefficients) != "(Intercept)" & !is.na(coefficients) & !is.na(standard_errors)
  # Factors emit several coefficients. Keep every matching level.
  if (!is.null(focus)) keep <- keep & coefficient_matches(names(coefficients), focus)
  coefficients <- coefficients[keep]
  standard_errors <- standard_errors[keep]
  if (!length(coefficients)) return(NULL)

  # Count from the fitted frame. Anything else misstates complete-case n.
  model_frame <- stats::model.frame(fit)
  outcome <- stats::model.response(model_frame)
  data.frame(
    cohort = cohort,
    model = model,
    focus = focus %||% NA_character_,
    term = names(coefficients),
    estimate = unname(coefficients),
    odds_ratio = exp(unname(coefficients)),
    conf_low = exp(unname(coefficients) - 1.96 * unname(standard_errors)),
    conf_high = exp(unname(coefficients) + 1.96 * unname(standard_errors)),
    p_value = 2 * stats::pnorm(abs(unname(coefficients / standard_errors)), lower.tail = FALSE),
    n = stats::nobs(fit),
    events = sum(outcome == 1L, na.rm = TRUE),
    converged = isTRUE(fit$converged),
    stringsAsFactors = FALSE
  )
}

fit_aki_risk_models <- function(data, mode = "full") {
  output <- list()
  output_index <- 1L

  # Fit simple models per exposure. Fit the joint model once per cohort.
  for (cohort in c("overall", "cardiac", "non_cardiac")) {
    cohort_data <- cohort_subset(data, cohort)
    full_terms <- full_adjustment_terms(cohort_data, cohort, mode)
    demographic_terms <- intersect(c("sex_male", "age_group", "bmi_group"), full_terms)

    for (focus in full_terms) {
      unadjusted <- stats::glm(
        formula_from_terms("aki", focus),
        data = cohort_data,
        family = stats::binomial(),
        control = stats::glm.control(maxit = 50L)
      )
      output[[output_index]] <- tidy_glm_fit(unadjusted, cohort, "unadjusted", focus)
      output_index <- output_index + 1L

      demographic_formula_terms <- unique(c(focus, demographic_terms))
      demographic <- stats::glm(
        formula_from_terms("aki", demographic_formula_terms),
        data = cohort_data,
        family = stats::binomial(),
        control = stats::glm.control(maxit = 50L)
      )
      output[[output_index]] <- tidy_glm_fit(demographic, cohort, "demographic_adjusted", focus)
      output_index <- output_index + 1L
    }

    fully_adjusted <- stats::glm(
      formula_from_terms("aki", full_terms),
      data = cohort_data,
      family = stats::binomial(),
      control = stats::glm.control(maxit = 75L)
    )
    for (focus in full_terms) {
      output[[output_index]] <- tidy_glm_fit(fully_adjusted, cohort, "fully_adjusted", focus)
      output_index <- output_index + 1L
    }
  }

  output <- Filter(Negate(is.null), output)
  as.data.frame(data.table::rbindlist(output, fill = TRUE), stringsAsFactors = FALSE)
}

# Use a smaller calendar spline when the data contain too few operation years.
add_calendar_spline <- function(data) {
  probabilities <- c(0.05, 0.275, 0.50, 0.725, 0.95)
  knots <- as.numeric(stats::quantile(data$operation_year, probabilities, na.rm = TRUE, names = FALSE))
  if (length(unique(knots)) < 5L) {
    spline <- splines::ns(data$operation_year, df = min(4L, length(unique(data$operation_year)) - 1L))
  } else {
    spline <- splines::ns(
      data$operation_year,
      knots = knots[2:4],
      Boundary.knots = knots[c(1, 5)]
    )
  }
  colnames(spline) <- paste0("calendar_spline_", seq_len(ncol(spline)))
  cbind(data, as.data.frame(spline, stringsAsFactors = FALSE))
}

# Start at day 7. Stop at day 1825.
prepare_mortality_data <- function(data, horizon_day = 1825, landmark_day = 7) {
  eligible <- is.finite(data$time_to_death_days) & data$time_to_death_days > landmark_day
  output <- droplevels(data[eligible, , drop = FALSE])
  output$landmark_day <- landmark_day
  output$analysis_time <- pmin(output$time_to_death_days, horizon_day)
  output$death_event <- as.integer(output$death == 1L & output$time_to_death_days <= horizon_day)
  add_calendar_spline(output)
}

fit_extended_cox_one <- function(data, cohort = "overall", mode = "full") {
  assert_packages("survival")
  cohort_data <- prepare_mortality_data(cohort_subset(data, cohort))
  adjustment_terms <- full_adjustment_terms(cohort_data, cohort, mode)
  spline_terms <- grep("^calendar_spline_", names(cohort_data), value = TRUE)
  # Model AKI by log(day). Give each hospital its own baseline hazard.
  right_hand_side <- c("aki", "tt(aki)", adjustment_terms, spline_terms, "strata(hospital_factor)")
  formula <- formula_from_terms(
    "survival::Surv(landmark_day, analysis_time, death_event)",
    right_hand_side
  )

  model_variables <- unique(c(
    "landmark_day", "analysis_time", "death_event", "aki", adjustment_terms,
    spline_terms, "hospital_factor"
  ))
  complete <- stats::complete.cases(cohort_data[, model_variables, drop = FALSE])
  cohort_data <- droplevels(cohort_data[complete, , drop = FALSE])
  if (sum(cohort_data$death_event) < 5L) stop("Too few deaths for the extended Cox model.", call. = FALSE)

  fit <- survival::coxph(
    formula,
    data = cohort_data,
    ties = "efron",
    tt = function(x, t, ...) x * log(pmax(t, 1)),
    singular.ok = TRUE
  )

  list(fit = fit, data = cohort_data, cohort = cohort)
}

evaluate_extended_cox <- function(model_object, days = c(30, 90, 365, 1095, 1825)) {
  fit <- model_object$fit
  coefficients <- stats::coef(fit)
  covariance <- stats::vcov(fit)
  aki_index <- match("aki", names(coefficients))
  interaction_index <- match("tt(aki)", names(coefficients))
  if (anyNA(c(aki_index, interaction_index))) stop("AKI time-interaction coefficients were not estimable.", call. = FALSE)

  # Compute HR(day) and its Wald interval from the full covariance matrix.
  rows <- lapply(days, function(day) {
    contrast <- rep(0, length(coefficients))
    contrast[aki_index] <- 1
    contrast[interaction_index] <- log(day)
    estimate <- sum(contrast * coefficients)
    standard_error <- sqrt(drop(t(contrast) %*% covariance %*% contrast))
    data.frame(
      cohort = model_object$cohort,
      day = day,
      estimate = estimate,
      hazard_ratio = exp(estimate),
      conf_low = exp(estimate - 1.96 * standard_error),
      conf_high = exp(estimate + 1.96 * standard_error),
      p_value = 2 * stats::pnorm(abs(estimate / standard_error), lower.tail = FALSE),
      n = nrow(model_object$data),
      events = sum(model_object$data$death_event),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

fit_time_varying_mortality <- function(data, mode = "full") {
  rows <- lapply(c("overall", "cardiac", "non_cardiac"), function(cohort) {
    evaluate_extended_cox(fit_extended_cox_one(data, cohort, mode))
  })
  do.call(rbind, rows)
}

fit_one_year_landmark_one <- function(data, cohort = "overall", mode = "full") {
  assert_packages("survival")
  source_data <- cohort_subset(data, cohort)
  # Censoring before day 365 does not establish one-year survival.
  known_one_year <- source_data$time_to_death_days >= 365 | source_data$death == 1L
  weight_data <- droplevels(source_data[known_one_year, , drop = FALSE])
  weight_data$survived_one_year <- as.integer(
    !(weight_data$death == 1L & weight_data$time_to_death_days <= 365) &
      weight_data$time_to_death_days >= 365
  )

  adjustment_terms <- full_adjustment_terms(weight_data, cohort, mode)
  weight_formula <- formula_from_terms("survived_one_year", unique(c("aki", adjustment_terms)))
  weight_model <- stats::glm(weight_formula, data = weight_data, family = stats::binomial())

  survivors <- droplevels(weight_data[weight_data$survived_one_year == 1L, , drop = FALSE])
  predicted <- stats::predict(weight_model, newdata = survivors, type = "response")
  marginal <- mean(weight_data$survived_one_year)
  # Stabilize inverse-survival weights. Floor p to avoid division blow-ups.
  survivors$survival_weight <- marginal / pmax(predicted, 1e-6)
  survivors$landmark_day <- 365
  survivors$analysis_time <- pmin(survivors$time_to_death_days, 1825)
  survivors$death_event <- as.integer(survivors$death == 1L & survivors$time_to_death_days <= 1825)
  survivors <- add_calendar_spline(survivors)

  spline_terms <- grep("^calendar_spline_", names(survivors), value = TRUE)
  # Stratify by hospital. Cluster the robust variance by patient.
  formula <- formula_from_terms(
    "survival::Surv(landmark_day, analysis_time, death_event)",
    c("aki", adjustment_terms, spline_terms, "strata(hospital_factor)", "cluster(patient_id)")
  )
  model_variables <- unique(c(
    "landmark_day", "analysis_time", "death_event", "survival_weight", "aki",
    adjustment_terms, spline_terms, "hospital_factor", "patient_id"
  ))
  complete <- stats::complete.cases(survivors[, model_variables, drop = FALSE])
  survivors <- droplevels(survivors[complete, , drop = FALSE])
  if (sum(survivors$death_event) < 5L) stop("Too few late deaths for the 1-year landmark model.", call. = FALSE)

  fit <- survival::coxph(
    formula,
    data = survivors,
    weights = survival_weight,
    robust = TRUE,
    ties = "efron",
    singular.ok = TRUE
  )
  estimate <- stats::coef(fit)[["aki"]]
  standard_error <- sqrt(stats::vcov(fit)["aki", "aki"])
  data.frame(
    cohort = cohort,
    interval = "1_to_5_years",
    estimate = estimate,
    hazard_ratio = exp(estimate),
    conf_low = exp(estimate - 1.96 * standard_error),
    conf_high = exp(estimate + 1.96 * standard_error),
    p_value = 2 * stats::pnorm(abs(estimate / standard_error), lower.tail = FALSE),
    n = nrow(survivors),
    events = sum(survivors$death_event),
    stringsAsFactors = FALSE
  )
}

fit_landmark_mortality <- function(data, mortality_results, mode = "full") {
  # This is the day-365 contrast, not an average over the first year.
  early <- mortality_results[mortality_results$day == 365, , drop = FALSE]
  early$interval <- "0_to_1_year"
  early <- early[, c("cohort", "interval", "estimate", "hazard_ratio", "conf_low", "conf_high", "p_value", "n", "events")]
  late <- do.call(rbind, lapply(c("overall", "cardiac", "non_cardiac"), function(cohort) {
    fit_one_year_landmark_one(data, cohort, mode)
  }))
  rbind(early, late)
}

# After day 7: 0=censoring, 1=ICU, 2=death.
build_competing_endpoint <- function(data, horizon_day = 365, landmark_day = 7) {
  icu_event_time <- ifelse(data$icu_admission == 1L, data$time_to_icu_days, Inf)
  death_event_time <- ifelse(data$death == 1L, data$time_to_death_days, Inf)
  icu_censor_time <- ifelse(data$icu_admission == 0L, data$time_to_icu_days, Inf)
  death_censor_time <- ifelse(data$death == 0L, data$time_to_death_days, Inf)
  censor_time <- pmin(icu_censor_time, death_censor_time, horizon_day, na.rm = TRUE)
  first_time <- pmin(icu_event_time, death_event_time, censor_time, horizon_day, na.rm = TRUE)

  # Use the first observed time. ICU wins an exact tie.
  status <- ifelse(
    is.finite(icu_event_time) & icu_event_time == first_time & icu_event_time <= death_event_time, 1L,
    ifelse(is.finite(death_event_time) & death_event_time == first_time, 2L, 0L)
  )
  eligible <- is.finite(first_time) & first_time > landmark_day
  output <- droplevels(data[eligible, , drop = FALSE])
  output$competing_time <- first_time[eligible] - landmark_day
  output$competing_status <- status[eligible]
  output
}

fit_fine_gray_one <- function(data, cohort = "overall", mode = "full") {
  assert_packages("cmprsk")
  cohort_data <- build_competing_endpoint(cohort_subset(data, cohort))
  cohort_data <- add_calendar_spline(cohort_data)
  if (identical(mode, "sample")) {
    # Sample mode checks the endpoint and solver. AKI is enough.
    adjustment_terms <- character()
    spline_terms <- character()
    matrix_terms <- "aki"
  } else {
    adjustment_terms <- full_adjustment_terms(cohort_data, cohort, mode)
    spline_terms <- grep("^calendar_spline_", names(cohort_data), value = TRUE)
    # crr has no strata term. Put hospital in the design matrix.
    matrix_terms <- unique(c("aki", adjustment_terms, spline_terms, "hospital_factor"))
  }
  complete <- stats::complete.cases(cohort_data[, c("competing_time", "competing_status", matrix_terms), drop = FALSE])
  cohort_data <- droplevels(cohort_data[complete, , drop = FALSE])
  if (sum(cohort_data$competing_status == 1L) < 5L) stop("Too few ICU events for Fine-Gray analysis.", call. = FALSE)

  # No intercept. Drop constants before crr sees a singular matrix.
  matrix_formula <- stats::as.formula(paste("~ 0 +", paste(matrix_terms, collapse = " + ")))
  covariate_matrix <- stats::model.matrix(matrix_formula, data = cohort_data)
  constant <- apply(covariate_matrix, 2L, function(x) length(unique(x)) <= 1L)
  covariate_matrix <- covariate_matrix[, !constant, drop = FALSE]
  aki_index <- match("aki", colnames(covariate_matrix))
  if (is.na(aki_index)) stop("AKI column is not estimable in Fine-Gray model.", call. = FALSE)

  # Fit AKI by log(time) first. Keep it only when the Wald p-value is below 0.05.
  extended_fit <- cmprsk::crr(
    ftime = cohort_data$competing_time,
    fstatus = cohort_data$competing_status,
    cov1 = covariate_matrix,
    cov2 = matrix(cohort_data$aki, ncol = 1L, dimnames = list(NULL, "aki_log_time")),
    # Time starts at day 7. Add 7 before taking log(time).
    tf = function(t) matrix(log(t + 7), ncol = 1L),
    failcode = 1L,
    cencode = 0L,
    variance = TRUE
  )
  interaction_index <- length(extended_fit$coef)
  interaction_se <- sqrt(extended_fit$var[interaction_index, interaction_index])
  interaction_p <- 2 * stats::pnorm(abs(extended_fit$coef[interaction_index] / interaction_se), lower.tail = FALSE)

  if (is.finite(interaction_p) && interaction_p < 0.05) {
    fit <- extended_fit
    time_varying <- TRUE
  } else {
    fit <- cmprsk::crr(
      ftime = cohort_data$competing_time,
      fstatus = cohort_data$competing_status,
      cov1 = covariate_matrix,
      failcode = 1L,
      cencode = 0L,
      variance = TRUE
    )
    time_varying <- FALSE
    interaction_index <- NA_integer_
  }

  list(
    fit = fit,
    cohort = cohort,
    data = cohort_data,
    aki_index = match("aki", names(fit$coef)),
    interaction_index = interaction_index,
    time_varying = time_varying,
    interaction_p = interaction_p
  )
}

evaluate_fine_gray <- function(model_object, days = c(30, 90, 365)) {
  fit <- model_object$fit
  coefficients <- fit$coef
  covariance <- fit$var
  if (is.na(model_object$aki_index)) stop("AKI coefficient is absent from Fine-Gray model.", call. = FALSE)

  # Report postoperative days. Count events on the day-7-relative scale.
  rows <- lapply(days, function(day) {
    contrast <- rep(0, length(coefficients))
    contrast[model_object$aki_index] <- 1
    if (model_object$time_varying) contrast[model_object$interaction_index] <- log(day)
    estimate <- sum(contrast * coefficients)
    standard_error <- sqrt(drop(t(contrast) %*% covariance %*% contrast))
    data.frame(
      cohort = model_object$cohort,
      day = day,
      estimate = estimate,
      subdistribution_hazard_ratio = exp(estimate),
      conf_low = exp(estimate - 1.96 * standard_error),
      conf_high = exp(estimate + 1.96 * standard_error),
      p_value = 2 * stats::pnorm(abs(estimate / standard_error), lower.tail = FALSE),
      time_varying = model_object$time_varying,
      interaction_p = model_object$interaction_p,
      n = nrow(model_object$data),
      events = sum(model_object$data$competing_status == 1L & model_object$data$competing_time <= day - 7),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

fit_icu_competing_risk <- function(data, mode = "full") {
  rows <- lapply(c("overall", "cardiac", "non_cardiac"), function(cohort) {
    evaluate_fine_gray(fit_fine_gray_one(data, cohort, mode))
  })
  do.call(rbind, rows)
}
