# 4.1 Logistic regression (binary screening flags) ---------------------

# Outcomes: phq2_flag, gad2_flag, phq4_flag (positive screening)
# Predictor: trans_from_gender
# Covariates: age_group_4, education_level, indigenous_status, nationality,
#             health_insurance, quality_of_life
#
# Six models: each outcome × unweighted glm / weighted survey::svyglm
# (quasibinomial for weighted non-integer effective counts).
# Tables: OR, 95% CI, p (broom::tidy, exponentiate = TRUE, conf.int = TRUE).
# Per model: log-likelihood, deviance, AIC, BIC, n (repeated on coef rows;
# BIC for svyglm is k*log(n) − 2*logLik using survey pseudo log-likelihood).

## Logistic models -------

source("00_Code/0.1 Settings.R")
source("00_Code/0.2 Packages.R")

options(survey.lonely.psu = "adjust")

outcomes <- c("phq2_flag", "gad2_flag", "phq4_flag")
outcome_labels <- c(
  phq2_flag = "Positive depression screening (PHQ-2 >= 3)",
  gad2_flag = "Positive anxiety screening (GAD-2 >= 3)",
  phq4_flag = "Positive distress screening (PHQ-4 >= 6)"
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
  x <- as.numeric(x)
  out <- format(
    round(x, digits),
    nsmall = digits,
    decimal.mark = ".",
    trim = TRUE
  )
  out[!is.finite(x)] <- ""
  out
}

fmt_p_reg <- function(p) {
  p <- as.numeric(p)
  out <- rep(NA_character_, length(p))
  ok <- !is.na(p)
  out[ok & p < 0.01] <- "0.000"
  out[ok & p >= 0.01] <- fmt_b(p[ok & p >= 0.01], 3L)
  out
}

fit_indices <- function(fit) {
  if (inherits(fit, "svyglm")) {
    mf <- stats::model.frame(fit)
    n_obs <- nrow(mf)
    k <- length(stats::coef(fit))
    ll <- suppressWarnings(as.numeric(stats::logLik(fit)))
    aic_vec <- stats::AIC(fit)
    aic_val <- unname(aic_vec[["AIC"]])
    bic_val <- if (is.finite(ll) && is.finite(n_obs) && n_obs > 0L) {
      k * log(n_obs) - 2 * ll
    } else {
      NA_real_
    }
    tibble::tibble(
      n = n_obs,
      log_likelihood = ll,
      deviance = stats::deviance(fit),
      aic = aic_val,
      bic = bic_val
    )
  } else {
    tibble::tibble(
      n = stats::nobs(fit),
      log_likelihood = as.numeric(stats::logLik(fit)),
      deviance = stats::deviance(fit),
      aic = stats::AIC(fit),
      bic = stats::BIC(fit)
    )
  }
}

coef_table_logistic <- function(fit) {
  td <- broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)
  mf <- fit_indices(fit)
  tibble::tibble(
    term = td$term,
    or = fmt_b(td$estimate, 2L),
    ci_95 = paste0("[", fmt_b(td$conf.low, 2L), ", ", fmt_b(td$conf.high, 2L), "]"),
    p_value = fmt_p_reg(td$p.value),
    n = mf$n,
    log_likelihood = fmt_b(mf$log_likelihood, 2L),
    deviance = fmt_b(mf$deviance, 2L),
    aic = fmt_b(mf$aic, 2L),
    bic = fmt_b(mf$bic, 2L)
  )
}

output_dir <- "02_Output/Models"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

excel_sheets <- list()
sample_rows <- list()

model_ids <- unlist(
  lapply(outcomes, function(y) paste0(y, c("_uw", "_w"))),
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
  dat <- data |>
    dplyr::select(dplyr::all_of(vars_cc)) |>
    tidyr::drop_na(dplyr::everything()) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.factor), droplevels))

  stopifnot(nrow(dat) > 0L)

  frm <- build_formula(y)

  # ----- Unweighted -----
  fit_uw <- stats::glm(frm, data = dat, family = stats::binomial(link = "logit"))
  nm <- paste0(y, "_uw")
  excel_sheets[[nm]] <- coef_table_logistic(fit_uw)
  sample_rows[[nm]] <- tibble::tibble(model = nm, n = nrow(dat))

  # ----- Weighted (survey, quasibinomial) -----
  des <- survey::svydesign(
    ids = ~varunit,
    strata = ~varstrat,
    weights = ~w_personas_cal,
    data = dat,
    nest = TRUE
  )
  fit_w <- survey::svyglm(frm, design = des, family = stats::quasibinomial())
  nm <- paste0(y, "_w")
  excel_sheets[[nm]] <- coef_table_logistic(fit_w)
  sample_rows[[nm]] <- tibble::tibble(model = nm, n = nrow(dat))
}

fit_summary_tbl <- purrr::map_dfr(model_ids, function(nm) {
  d <- excel_sheets[[nm]]
  d[1L, c("n", "log_likelihood", "deviance", "aic", "bic")] |>
    dplyr::mutate(model = nm, .before = 1L)
}) |>
  dplyr::distinct()

all_models_tbl <- purrr::map_dfr(model_ids, function(nm) {
  dplyr::mutate(excel_sheets[[nm]], model = nm, .before = 1L)
})

excel_sheets$Model_fit <- fit_summary_tbl
excel_sheets$Sample_sizes <- dplyr::bind_rows(sample_rows) |> dplyr::distinct()
excel_sheets$Notes <- tibble::tibble(
  topic = c(
    "Odds ratios",
    "p-values",
    "Unweighted",
    "Weighted",
    "Fit indices (weighted)",
    "Model_fit sheet",
    "All_models sheet"
  ),
  detail = c(
    "OR and 95% CI from broom::tidy(..., exponentiate = TRUE, conf.int = TRUE); 2 decimals.",
    "Three decimals; p < 0.01 shown as 0.000.",
    "stats::glm(..., family = binomial(logit)); ML log-likelihood, deviance, AIC, BIC.",
    "survey::svyglm(..., family = quasibinomial) with varunit, varstrat, w_personas_cal.",
    "Log-likelihood is survey pseudo–log-likelihood; AIC uses survey AIC component; BIC ≈ k*log(n) − 2*logLik (approximate).",
    "One row per model: n, log-likelihood, deviance, AIC, BIC.",
    "Six coefficient tables stacked with model identifier."
  )
)

coef_sheets <- stats::setNames(
  lapply(model_ids, function(nm) excel_sheets[[nm]]),
  model_ids
)

excel_out <- c(
  list(All_models = all_models_tbl),
  list(Model_fit = excel_sheets$Model_fit),
  coef_sheets,
  list(
    Sample_sizes = excel_sheets$Sample_sizes,
    Notes = excel_sheets$Notes
  )
)

writexl::write_xlsx(
  excel_out,
  path = file.path(output_dir, "logistic_regression_results.xlsx")
)

## Ordinal models -------

# GAD item 1 (sm3), GAD item 2 (sm4), PHQ item 1 (sm1), PHQ item 2 (sm2)
# Response: 0 Never, 1 Some days, 2 More than half the days, 3 Almost every day (ordered)
# Same predictors as logistic models. CLM (ordinal::clm), proportional odds / logit link.
# Weighted fits use weights renormalized to sum to n (same relative weights; improves CLM convergence).

ordinal_items <- c("sm3", "sm4", "sm1", "sm2")
ordinal_panels <- c(
  "A. GAD 1",
  "B. GAD 2",
  "C. PHQ 1",
  "D. PHQ 2"
)
vars_ord <- unique(c(
  ordinal_items,
  "trans_from_gender",
  covariates,
  "w_personas_cal"
))

data_ord <- rio::import("02_Output/Data/ENSSEX_trans_population.RData") |>
  dplyr::select(dplyr::any_of(vars_ord))

w_ok_ord <- !is.na(data_ord$w_personas_cal) &
  data_ord$w_personas_cal > 0 &
  is.finite(data_ord$w_personas_cal)
data_ord <- data_ord[w_ok_ord, , drop = FALSE]

build_formula_ord <- function(y_chr) {
  stats::as.formula(paste0(
    y_chr,
    " ~ trans_from_gender + ",
    paste(covariates, collapse = " + ")
  ))
}

coef_table_clm <- function(fit) {
  td <- broom::tidy(fit, conf.int = FALSE)
  z <- stats::qnorm(0.975)
  mf <- tibble::tibble(
    n = stats::nobs(fit),
    log_likelihood = as.numeric(stats::logLik(fit)),
    aic = stats::AIC(fit),
    bic = stats::BIC(fit)
  )
  td |>
    dplyr::mutate(
      or_num = exp(.data$estimate),
      ci_lo = exp(.data$estimate - z * .data$std.error),
      ci_hi = exp(.data$estimate + z * .data$std.error)
    ) |>
    dplyr::transmute(
      term = .data$term,
      coef_type = .data$coef.type,
      or = fmt_b(.data$or_num, 2L),
      ci_95 = paste0("[", fmt_b(.data$ci_lo, 2L), ", ", fmt_b(.data$ci_hi, 2L), "]"),
      p_value = fmt_p_reg(.data$p.value),
      n = mf$n,
      log_likelihood = fmt_b(mf$log_likelihood, 2L),
      aic = fmt_b(mf$aic, 2L),
      bic = fmt_b(mf$bic, 2L)
    )
}

newdata_trans_cis_trans <- function(dat) {
  n <- 2L
  pieces <- vector("list", length(covariates))
  names(pieces) <- covariates
  for (nm in covariates) {
    x <- dat[[nm]]
    if (is.factor(x)) {
      pieces[[nm]] <- factor(rep(levels(x)[[1L]], n), levels = levels(x))
    } else {
      pieces[[nm]] <- rep(mean(x, na.rm = TRUE), n)
    }
  }
  dplyr::bind_cols(
    tibble::tibble(
      trans_from_gender = factor(
        c("No", "Yes"),
        levels = levels(dat$trans_from_gender)
      )
    ),
    as.data.frame(pieces, stringsAsFactors = TRUE)
  )
}

cumprob_plot_df <- function(fit, newdat, group_labs = c("Cisgender", "Transgender")) {
  pr <- stats::predict(
    fit,
    newdata = newdat,
    type = "cum.prob",
    se.fit = TRUE,
    interval = TRUE,
    level = 0.95
  )
  cp <- pr$cprob1
  lw <- pr$lwr1
  up <- pr$upr1
  xlabs <- paste0("P(Y≤", colnames(cp), ")")
  purrr::map_dfr(seq_len(nrow(cp)), function(i) {
    tibble::tibble(
      group = group_labs[i],
      threshold = factor(xlabs, levels = xlabs),
      x_num = seq_along(xlabs),
      cum = as.numeric(cp[i, ]),
      lwr = as.numeric(lw[i, ]),
      upr = as.numeric(up[i, ])
    )
  }) |>
    dplyr::mutate(group = factor(.data$group, levels = group_labs))
}

plot_cumprob_panel <- function(plot_df, panel_title) {
  cols <- c("Cisgender" = "#5B9BD5", "Transgender" = "#ED7D31")
  ggplot2::ggplot(plot_df, ggplot2::aes(
    x = .data$x_num,
    y = .data$cum,
    color = .data$group,
    fill = .data$group
  )) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lwr, ymax = .data$upr),
      alpha = 0.22,
      linewidth = 0,
      show.legend = FALSE
    ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::geom_text(
      ggplot2::aes(label = fmt_b(.data$cum, 2L)),
      nudge_y = 0.045,
      size = 2.8,
      show.legend = FALSE
    ) +
    ggplot2::scale_x_continuous(
      breaks = plot_df$x_num[!duplicated(plot_df$x_num)],
      labels = levels(plot_df$threshold)
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.25)) +
    ggplot2::scale_color_manual(values = cols, name = NULL) +
    ggplot2::scale_fill_manual(values = cols, name = NULL) +
    ggplot2::labs(
      title = panel_title,
      x = "Cumulative response category",
      y = "Cumulative probability"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = "plain"),
      legend.position = "top",
      legend.justification = "center",
      legend.box.just = "center",
      panel.grid.minor = ggplot2::element_blank()
    )
}

ord_sheets_uw <- list()
ord_sheets_w <- list()
plots_uw <- list()
plots_w <- list()
ord_fit_uw <- list()
ord_fit_w <- list()

for (i in seq_along(ordinal_items)) {
  y <- ordinal_items[[i]]
  vars_cc <- c(y, "trans_from_gender", covariates, "w_personas_cal")
  dat <- data_ord |>
    dplyr::select(dplyr::all_of(vars_cc)) |>
    tidyr::drop_na(dplyr::everything()) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.factor), droplevels)) |>
    dplyr::mutate(
      !!y := ordered(.data[[y]], levels = 0:3)
    )

  stopifnot(nrow(dat) > 0L)

  frm <- build_formula_ord(y)
  dat <- dat |>
    dplyr::mutate(
      w_fit = .data$w_personas_cal * nrow(dat) / sum(.data$w_personas_cal)
    )

  ctl <- ordinal::clm.control(maxIter = 4000L, gradTol = 1e-4)

  fit_uw <- ordinal::clm(frm, data = dat, control = ctl)
  nm_uw <- paste0(y, "_uw")
  ord_sheets_uw[[nm_uw]] <- coef_table_clm(fit_uw)
  ord_fit_uw[[y]] <- fit_uw

  fit_w <- ordinal::clm(frm, data = dat, weights = w_fit, control = ctl)
  nm_w <- paste0(y, "_w")
  ord_sheets_w[[nm_w]] <- coef_table_clm(fit_w)
  ord_fit_w[[y]] <- fit_w

  nd <- newdata_trans_cis_trans(dat)
  plots_uw[[y]] <- plot_cumprob_panel(
    cumprob_plot_df(fit_uw, nd),
    ordinal_panels[[i]]
  )
  plots_w[[y]] <- plot_cumprob_panel(
    cumprob_plot_df(fit_w, nd),
    ordinal_panels[[i]]
  )
}

all_ord_uw <- purrr::map_dfr(names(ord_sheets_uw), function(nm) {
  dplyr::mutate(ord_sheets_uw[[nm]], model = nm, .before = 1L)
})
all_ord_w <- purrr::map_dfr(names(ord_sheets_w), function(nm) {
  dplyr::mutate(ord_sheets_w[[nm]], model = nm, .before = 1L)
})

model_fit_ord_uw <- purrr::map_dfr(names(ord_sheets_uw), function(nm) {
  ord_sheets_uw[[nm]][1L, c("n", "log_likelihood", "aic", "bic")] |>
    dplyr::mutate(model = nm, .before = 1L)
}) |>
  dplyr::distinct()
model_fit_ord_w <- purrr::map_dfr(names(ord_sheets_w), function(nm) {
  ord_sheets_w[[nm]][1L, c("n", "log_likelihood", "aic", "bic")] |>
    dplyr::mutate(model = nm, .before = 1L)
}) |>
  dplyr::distinct()

notes_ord <- tibble::tibble(
  topic = c(
    "Model",
    "OR",
    "Threshold parameters",
    "Covariate profile (figures)",
    "Weighted CLM",
    "Groups in figures"
  ),
  detail = c(
    "ordinal::clm (proportional odds, logit link). Same formula as logistic modules.",
    "All parameters on exponentiated scale (exp(estimate)); Wald 95% CI from std.error.",
    "Rows coef_type = intercept are cutpoints (exp(threshold) on cumulative logit scale, not a covariate OR).",
    "Predicted cumulative P(Y≤k) with other covariates at reference category (first factor level).",
    "Weights = w_personas_cal renormalized to sum to n for numerical stability; point estimates match unscaled WCLM up to tolerance.",
    "Cisgender = trans_from_gender No; Transgender = Yes."
  )
)

coef_ord_uw <- stats::setNames(
  lapply(names(ord_sheets_uw), function(nm) ord_sheets_uw[[nm]]),
  names(ord_sheets_uw)
)
coef_ord_w <- stats::setNames(
  lapply(names(ord_sheets_w), function(nm) ord_sheets_w[[nm]]),
  names(ord_sheets_w)
)

excel_ord_uw <- c(
  list(All_models = all_ord_uw),
  list(Model_fit = model_fit_ord_uw),
  coef_ord_uw,
  list(Notes = notes_ord)
)
excel_ord_w <- c(
  list(All_models = all_ord_w),
  list(Model_fit = model_fit_ord_w),
  coef_ord_w,
  list(Notes = notes_ord)
)

writexl::write_xlsx(
  excel_ord_uw,
  path = file.path(output_dir, "ordinal_clm_results_unweighted.xlsx")
)
writexl::write_xlsx(
  excel_ord_w,
  path = file.path(output_dir, "ordinal_clm_results_weighted.xlsx")
)

fig_uw <- ggpubr::ggarrange(
  plotlist = plots_uw[ordinal_items],
  ncol = 2,
  nrow = 2,
  common.legend = TRUE,
  legend = "top",
  align = "hv"
) &
  ggplot2::theme(
    legend.position = "top",
    legend.justification = "center",
    legend.box.just = "center"
  )

fig_w <- ggpubr::ggarrange(
  plotlist = plots_w[ordinal_items],
  ncol = 2,
  nrow = 2,
  common.legend = TRUE,
  legend = "top",
  align = "hv"
) &
  ggplot2::theme(
    legend.position = "top",
    legend.justification = "center",
    legend.box.just = "center"
  )

ggplot2::ggsave(
  filename = file.path(output_dir, "figure_ordinal_cumprob_unweighted.png"),
  plot = fig_uw,
  width = 10,
  height = 9,
  dpi = 300,
  bg = "white"
)
ggplot2::ggsave(
  filename = file.path(output_dir, "figure_ordinal_cumprob_weighted.png"),
  plot = fig_w,
  width = 10,
  height = 9,
  dpi = 300,
  bg = "white"
)
