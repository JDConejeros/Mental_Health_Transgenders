# 4.0 Linear regression models --------------------------------------

# Dependent variables: GAD-2, PHQ-2, PHQ-4 total scores
# Independent variable: trans_from_gender
# Covariates: age_group_4, education_level, indigenous_status, nationality,
#             health_insurance, quality_of_life
#
# Twelve models: unweighted / weighted × raw / standardized outcome
# Coefficient tables: OLS or WLS (lm with sampling weights) with HC3
# heteroskedasticity-robust standard errors (sandwich::vcovHC, lmtest::coeftest).
# Diagnostics (KS, BP, VIF, residual plots): same lm objects as for inference.

source("00_Code/0.1 Settings.R")
source("00_Code/0.2 Packages.R")

outcomes <- c("gad2", "phq2", "phq4")
outcome_labels <- c(
  gad2 = "GAD-2 total score",
  phq2 = "PHQ-2 total score",
  phq4 = "PHQ-4 total score"
)
covariates <- c(
  "age_group_4",
  "education_level",
  "indigenous_status",
  "nationality",
  "health_insurance",
  "quality_of_life"
)

vars_base <- unique(c(
  outcomes,
  "trans_from_gender",
  covariates,
  "varunit",
  "varstrat",
  "w_personas_cal"
))

data <- rio::import("02_Output/Data/ENSSEX_trans_population.RData") |>
  dplyr::select(dplyr::any_of(vars_base))

w_ok <- !is.na(data$w_personas_cal) & data$w_personas_cal > 0 & is.finite(data$w_personas_cal)
data <- data[w_ok, , drop = FALSE]

build_formula <- function(y_chr) {
  stats::as.formula(paste0(
    y_chr,
    " ~ trans_from_gender + ",
    paste(covariates, collapse = " + ")
  ))
}

fmt_b <- function(x, digits = 2L) {
  format(
    round(as.numeric(x), digits),
    nsmall = digits,
    decimal.mark = ".",
    trim = TRUE
  )
}

fmt_p_reg <- function(p) {
  p <- as.numeric(p)
  out <- rep(NA_character_, length(p))
  ok <- !is.na(p)
  out[ok & p < 0.01] <- "0.000"
  out[ok & p >= 0.01] <- fmt_b(p[ok & p >= 0.01], 3L)
  out
}

coef_table_lm_hc3 <- function(fit) {
  v <- sandwich::vcovHC(fit, type = "HC3")
  ct <- lmtest::coeftest(fit, vcov. = v)
  ci <- lmtest::coefci(fit, vcov. = v, level = 0.95)
  rn <- rownames(ct)
  sm <- summary(fit)
  n_obs <- stats::nobs(fit)
  r_sq <- unname(sm$r.squared)
  tibble::tibble(
    term = rn,
    b = fmt_b(ct[, 1L], 2L),
    ci_95 = paste0("[", fmt_b(ci[rn, 1L], 2L), ", ", fmt_b(ci[rn, 2L], 2L), "]"),
    p_value = fmt_p_reg(ct[, 4L]),
    n = n_obs,
    r_squared = fmt_b(r_sq, 4L)
  )
}

weighted_mean_var <- function(x, w) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]
  w <- w[ok]
  m <- sum(w * x) / sum(w)
  v <- sum(w * (x - m)^2) / sum(w)
  c(mean = m, var = v)
}

add_outcome_wz <- function(df, y) {
  wv <- weighted_mean_var(df[[y]], df$w_personas_cal)
  z <- (df[[y]] - wv["mean"]) / sqrt(wv["var"])
  out <- df
  out[[paste0(y, "_wz")]] <- as.numeric(z)
  out
}

vif_to_long <- function(fit_lm, model_name) {
  v <- car::vif(fit_lm)
  if (is.matrix(v)) {
    gcol <- if ("GVIF^(1/(2*Df))" %in% colnames(v)) {
      "GVIF^(1/(2*Df))"
    } else {
      colnames(v)[[1L]]
    }
    tibble::tibble(
      model = model_name,
      term = rownames(v),
      vif = fmt_b(v[, gcol], 2L)
    )
  } else {
    tibble::tibble(
      model = model_name,
      term = names(v),
      vif = fmt_b(as.numeric(v), 2L)
    )
  }
}

diag_row <- function(fit_lm, model_name) {
  r <- stats::residuals(fit_lm)
  r <- as.numeric(r)
  r <- r[is.finite(r)]
  sr <- as.numeric(scale(r))
  ks <- suppressWarnings(stats::ks.test(sr, stats::pnorm))
  bp <- lmtest::bptest(fit_lm)
  tibble::tibble(
    model = model_name,
    ks_statistic = fmt_b(unname(ks$statistic), 2L),
    ks_p_value = fmt_p_reg(ks$p.value),
    bp_statistic = fmt_b(unname(bp$statistic), 2L),
    bp_p_value = fmt_p_reg(bp$p.value)
  )
}

plot_studentized <- function(fit_lm, path, title) {
  f <- stats::fitted(fit_lm)
  rs <- stats::rstudent(fit_lm)
  d <- tibble::tibble(fitted = f, rstudent = rs)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = fitted, y = rstudent)) +
    ggplot2::geom_point(alpha = 0.12, size = 0.7, colour = "#333333") +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, linewidth = 0.35, colour = "gray40") +
    ggplot2::labs(
      title = title,
      x = "Fitted values",
      y = "Studentized residuals"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 10, face = "bold"),
      axis.title = ggplot2::element_text(size = 9)
    )
  ggplot2::ggsave(filename = path, plot = p, width = 5.5, height = 4, dpi = 300, bg = "white")
}

output_dir <- "02_Output/Models"
fig_dir <- file.path(output_dir, "residual_plots")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

excel_sheets <- list()
diag_rows <- list()
vif_rows <- list()
sample_rows <- list()

model_ids <- unlist(
  lapply(outcomes, function(y) paste0(y, c("_uw_raw", "_uw_std", "_w_raw", "_w_std"))),
  use.names = FALSE
)

for (y in outcomes) {
  vars_cc <- c(
    y,
    "trans_from_gender",
    covariates,
    "varunit",
    "varstrat",
    "w_personas_cal"
  )
  dat_y <- data |>
    dplyr::select(dplyr::all_of(vars_cc)) |>
    tidyr::drop_na(dplyr::everything()) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.factor), droplevels))

  stopifnot(nrow(dat_y) > 0L)

  frm_raw <- build_formula(y)
  y_wz <- paste0(y, "_wz")
  dat_w <- add_outcome_wz(dat_y, y)
  frm_std_uw <- build_formula(paste0("scale(", y, ")"))
  frm_std_w <- build_formula(y_wz)

  # ----- Unweighted raw -----
  fit_uw_raw <- stats::lm(frm_raw, data = dat_y)
  nm <- paste0(y, "_uw_raw")
  excel_sheets[[nm]] <- coef_table_lm_hc3(fit_uw_raw)
  diag_rows[[nm]] <- diag_row(fit_uw_raw, nm)
  vif_rows[[nm]] <- vif_to_long(fit_uw_raw, nm)
  sample_rows[[nm]] <- tibble::tibble(model = nm, n = nrow(dat_y))
  plot_studentized(
    fit_uw_raw,
    file.path(fig_dir, paste0(nm, ".png")),
    paste0(outcome_labels[[y]], " — unweighted, raw")
  )

  # ----- Unweighted standardized (outcome scaled to unit SD) -----
  fit_uw_std <- stats::lm(frm_std_uw, data = dat_y)
  nm <- paste0(y, "_uw_std")
  excel_sheets[[nm]] <- coef_table_lm_hc3(fit_uw_std)
  diag_rows[[nm]] <- diag_row(fit_uw_std, nm)
  vif_rows[[nm]] <- vif_to_long(fit_uw_std, nm)
  sample_rows[[nm]] <- tibble::tibble(model = nm, n = nrow(dat_y))
  plot_studentized(
    fit_uw_std,
    file.path(fig_dir, paste0(nm, ".png")),
    paste0(outcome_labels[[y]], " — unweighted, standardized outcome")
  )

  # ----- Weighted raw (WLS + HC3) -----
  fit_w_raw <- stats::lm(frm_raw, data = dat_w, weights = w_personas_cal)
  nm <- paste0(y, "_w_raw")
  excel_sheets[[nm]] <- coef_table_lm_hc3(fit_w_raw)
  diag_rows[[nm]] <- diag_row(fit_w_raw, nm)
  vif_rows[[nm]] <- vif_to_long(fit_w_raw, nm)
  sample_rows[[nm]] <- tibble::tibble(model = nm, n = nrow(dat_w))
  plot_studentized(
    fit_w_raw,
    file.path(fig_dir, paste0(nm, ".png")),
    paste0(outcome_labels[[y]], " — weighted, raw")
  )

  # ----- Weighted standardized outcome (weighted z) + HC3 -----
  fit_w_std <- stats::lm(frm_std_w, data = dat_w, weights = w_personas_cal)
  nm <- paste0(y, "_w_std")
  excel_sheets[[nm]] <- coef_table_lm_hc3(fit_w_std)
  diag_rows[[nm]] <- diag_row(fit_w_std, nm)
  vif_rows[[nm]] <- vif_to_long(fit_w_std, nm)
  sample_rows[[nm]] <- tibble::tibble(model = nm, n = nrow(dat_w))
  plot_studentized(
    fit_w_std,
    file.path(fig_dir, paste0(nm, ".png")),
    paste0(outcome_labels[[y]], " — weighted, standardized outcome")
  )
}

all_models_tbl <- purrr::map_dfr(model_ids, function(nm) {
  dplyr::mutate(excel_sheets[[nm]], model = nm, .before = 1L)
})

excel_sheets$Diagnostics <- dplyr::bind_rows(diag_rows)
excel_sheets$VIF <- dplyr::bind_rows(vif_rows)
excel_sheets$Sample_sizes <- dplyr::bind_rows(sample_rows) |> dplyr::distinct()
excel_sheets$Notes <- tibble::tibble(
  topic = c(
    "Coefficients / CI",
    "p-values",
    "Robust SE",
    "Unweighted standardized",
    "Weighted standardized",
    "Weighted estimation",
    "Diagnostics",
    "KS test",
    "Breusch–Pagan",
    "VIF",
    "Residual plots",
    "All_models sheet"
  ),
  detail = c(
    "Estimates and 95% CI (Wald) formatted with 2 decimals (decimal point). Columns n and r_squared repeat per row (model fit statistics).",
    "Three decimals; values with p < 0.01 shown as 0.000.",
    "Heteroskedasticity-robust covariance HC3 (sandwich::vcovHC); tests via lmtest::coeftest / coefci.",
    "lm with scale(outcome) on the left-hand side (SD units of the outcome).",
    "lm with design-weighted z-score of the outcome as response, weights = w_personas_cal.",
    "Weighted models use lm(..., weights = w_personas_cal) (WLS); SE are HC3-robust, not design-based (strata/PSU).",
    "Kolmogorov–Smirnov (normality of residuals), Breusch–Pagan (homoskedasticity), VIF from the same lm.",
    "One-sample KS on standardized residuals vs standard normal; p-value reported.",
    "lmtest::bptest on the same lm; statistic and p-value.",
    "car::vif (GVIF^(1/(2*Df)) when applicable).",
    "Studentized residuals vs fitted; saved under 02_Output/Models/residual_plots/.",
    "All 12 coefficient tables stacked with a model identifier column."
  )
)

coef_sheets <- stats::setNames(
  lapply(model_ids, function(nm) excel_sheets[[nm]]),
  model_ids
)

excel_out <- c(
  list(All_models = all_models_tbl),
  coef_sheets,
  list(
    Diagnostics = excel_sheets$Diagnostics,
    VIF = excel_sheets$VIF,
    Sample_sizes = excel_sheets$Sample_sizes,
    Notes = excel_sheets$Notes
  )
)

writexl::write_xlsx(
  excel_out,
  path = file.path(output_dir, "linear_regression_results.xlsx")
)
