# 3.1 Correlation analysis -----------------------------------------

# Load project settings
source("00_Code/0.1 Settings.R")
source("00_Code/0.2 Packages.R")
source("00_Code/0.3 Functions.R")

options(survey.lonely.psu = "adjust")

# Analysis dataset: ENSSEX population file
data <- rio::import("02_Output/Data/ENSSEX_trans_population.RData")

data <- data |>
  dplyr::select(-starts_with("suicidal_")) |>
  dplyr::select(-c(gender_identity, trans_from_q3, sexual_orientation, sexual_talk_family, edad, sex)) |>
  tidyr::drop_na()

data_general <- data

## Helpers -----------------------------------------------------------------

fmt_r <- function(x, digits = 2L) {
  format(round(as.numeric(x), digits), nsmall = digits, decimal.mark = ".", trim = TRUE)
}

fmt_p_corr <- function(p) {
  p <- as.numeric(p)
  if (length(p) != 1L || is.na(p)) {
    return("P=NA")
  }
  if (p < 0.01) {
    return("P=0.00")
  }
  paste0("P=", format(round(p, 3), nsmall = 3, decimal.mark = ".", trim = TRUE))
}

# Weighted Cronbach's alpha from weighted covariance (ML in cov.wt)
cronbach_weighted <- function(x, w) {
  x <- as.matrix(x)
  w <- as.numeric(w)
  ok <- stats::complete.cases(x) & !is.na(w) & w > 0
  x <- x[ok, , drop = FALSE]
  w <- w[ok]
  k <- ncol(x)
  if (k < 2L || nrow(x) < 3L) {
    return(NA_real_)
  }
  cw <- stats::cov.wt(x, wt = w, cor = FALSE, center = TRUE, method = "ML")
  C <- cw$cov
  sum_diag <- sum(diag(C))
  var_sum <- sum(C)
  if (!is.finite(var_sum) || var_sum <= 0) {
    return(NA_real_)
  }
  (k / (k - 1)) * (1 - sum_diag / var_sum)
}

# Cluster bootstrap SE for weighted alpha (global + if-item-deleted)
cronbach_weighted_boot <- function(df, item_cols, weight_col = "w_personas_cal",
                                   strata_col = "varstrat", cluster_col = "varunit",
                                   B = 120L, seed = 42L) {
  set.seed(seed)
  d <- df[, c(item_cols, weight_col, strata_col, cluster_col), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d$.clust <- interaction(d[[strata_col]], d[[cluster_col]], drop = TRUE)
  idx_by_clust <- split(seq_len(nrow(d)), d$.clust)
  u <- names(idx_by_clust)
  n_cl <- length(u)
  if (n_cl < 2L) {
    return(list(se_global = NA_real_, se_drop = rep(NA_real_, length(item_cols))))
  }

  k <- length(item_cols)
  xnam <- item_cols

  one_draw <- function() {
    pick <- sample(u, size = n_cl, replace = TRUE)
    idx <- unlist(idx_by_clust[pick], use.names = FALSE)
    sub <- d[idx, , drop = FALSE]
    w <- sub[[weight_col]]
    x <- as.matrix(sub[, xnam, drop = FALSE])
    g <- cronbach_weighted(x, w)
    dr <- vapply(seq_len(k), function(j) {
      cols <- xnam[-j]
      if (length(cols) < 2L) {
        NA_real_
      } else {
        cronbach_weighted(sub[, cols, drop = FALSE], w)
      }
    }, numeric(1L))
    c(global = g, dr)
  }

  draws <- replicate(B, one_draw())
  if (!is.matrix(draws)) {
    draws <- matrix(draws, nrow = k + 1L)
  }
  se_global <- stats::sd(draws[1L, ], na.rm = TRUE)
  se_drop <- apply(draws[-1L, , drop = FALSE], 1L, stats::sd, na.rm = TRUE)
  list(se_global = se_global, se_drop = se_drop)
}

build_spearman_matrix_uw <- function(df, vars, col_labs) {
  k <- length(vars)
  mat <- matrix("", k, k, dimnames = list(col_labs, col_labs))
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      if (i == j) {
        mat[i, j] <- ""
      } else {
        xi <- df[[vars[i]]]
        xj <- df[[vars[j]]]
        ok <- !is.na(xi) & !is.na(xj)
        ct <- stats::cor.test(xi[ok], xj[ok], method = "spearman", exact = FALSE)
        if (i > j) {
          mat[i, j] <- fmt_r(unname(ct$estimate))
        } else {
          mat[i, j] <- fmt_p_corr(ct$p.value)
        }
      }
    }
  }
  tibble::as_tibble(mat, rownames = "Variable")
}

build_spearman_matrix_w <- function(design, rank_vars, col_labs) {
  k <- length(rank_vars)
  mat <- matrix("", k, k, dimnames = list(col_labs, col_labs))
  V <- as.matrix(survey::svyvar(stats::reformulate(rank_vars), design, na.rm = TRUE))
  R <- stats::cov2cor(V)

  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      if (i == j) {
        mat[i, j] <- ""
      } else if (i > j) {
        mat[i, j] <- fmt_r(R[i, j])
      } else {
        form <- stats::as.formula(paste(rank_vars[i], "~", rank_vars[j]))
        fit <- survey::svyglm(form, design)
        sm <- summary(fit)$coefficients
        rn <- rownames(sm)
        jname <- rank_vars[j]
        p <- if (jname %in% rn) {
          sm[jname, "Pr(>|t|)"]
        } else {
          NA_real_
        }
        mat[i, j] <- fmt_p_corr(p)
      }
    }
  }
  tibble::as_tibble(mat, rownames = "Variable")
}

fmt_or_na <- function(x, digits = 2L) {
  if (length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }
  fmt_r(x, digits)
}

reliability_table_uw <- function(df, item_cols, item_labels, scale_label) {
  x <- as.data.frame(df[, item_cols, drop = FALSE])
  x <- x[stats::complete.cases(x), , drop = FALSE]
  k <- length(item_cols)
  a <- psych::alpha(x, warnings = FALSE)
  glob_a <- unname(a$total$raw_alpha)
  glob_se <- unname(a$total$ase)
  ad <- a$alpha.drop
  drop_a <- ad$raw_alpha
  drop_se <- ad[["alpha se"]]

  item_drop_labs <- if (!is.null(names(item_labels))) {
    paste0("If ", names(item_labels), " deleted")
  } else {
    paste0("If item ", seq_len(k), " deleted")
  }

  tibble::tibble(
    scale = scale_label,
    item = c("Global", item_drop_labs),
    alpha = c(
      fmt_or_na(glob_a),
      vapply(seq_len(k), function(j) fmt_or_na(unname(drop_a[j])), character(1L))
    ),
    se = c(
      fmt_or_na(glob_se, digits = 3L),
      vapply(seq_len(k), function(j) fmt_or_na(unname(drop_se[j]), digits = 3L), character(1L))
    )
  )
}

reliability_table_w <- function(df, item_cols, item_labels, scale_label,
                                weight_col = "w_personas_cal",
                                strata_col = "varstrat", cluster_col = "varunit") {
  d <- df[, c(item_cols, weight_col, strata_col, cluster_col), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  w <- d[[weight_col]]
  x <- as.matrix(d[, item_cols, drop = FALSE])
  k <- length(item_cols)

  glob_a <- cronbach_weighted(x, w)
  boot <- cronbach_weighted_boot(d, item_cols, weight_col, strata_col, cluster_col)
  glob_se <- boot$se_global

  drop_a <- vapply(seq_len(k), function(j) {
    cols <- item_cols[-j]
    if (length(cols) < 2L) {
      cronbach_weighted(d[, cols, drop = FALSE], w)
    } else {
      cronbach_weighted(d[, cols, drop = FALSE], w)
    }
  }, numeric(1L))
  drop_se <- boot$se_drop

  item_drop_labs <- if (!is.null(names(item_labels))) {
    paste0("If ", names(item_labels), " deleted")
  } else {
    paste0("If item ", seq_len(k), " deleted")
  }

  tibble::tibble(
    scale = scale_label,
    item = c("Global", item_drop_labs),
    alpha = c(
      fmt_or_na(glob_a),
      vapply(seq_len(k), function(j) fmt_or_na(drop_a[j]), character(1L))
    ),
    se = c(
      fmt_or_na(glob_se, digits = 3L),
      vapply(seq_len(k), function(j) fmt_or_na(drop_se[j], digits = 3L), character(1L))
    )
  )
}

## Data for correlations / reliability --------------------------------------

mh_vars <- c("gad2", "phq2", "phq4")
mh_labs <- c("GAD-2", "PHQ-2", "PHQ-4")

corr_df <- data_general |>
  dplyr::filter(dplyr::if_all(dplyr::all_of(c(mh_vars, "varunit", "varstrat", "w_personas_cal")), ~ !is.na(.x))) |>
  dplyr::mutate(
    r_gad2 = rank(gad2, ties.method = "average"),
    r_phq2 = rank(phq2, ties.method = "average"),
    r_phq4 = rank(phq4, ties.method = "average")
  )

rank_names <- c("r_gad2", "r_phq2", "r_phq4")

design_corr <- survey::svydesign(
  ids = ~varunit,
  strata = ~varstrat,
  weights = ~w_personas_cal,
  data = corr_df,
  nest = TRUE
)

## Table 1. Spearman correlation matrices -----------------------------------

spearman_uw <- build_spearman_matrix_uw(corr_df, mh_vars, mh_labs)
spearman_w <- build_spearman_matrix_w(design_corr, rank_names, mh_labs)

## Table 2. Cronbach PHQ-4 as GAD-2 + PHQ-2 (two parts) ----------------------

labels_gad_phq2 <- c("GAD-2", "PHQ-2")
names(labels_gad_phq2) <- labels_gad_phq2

alpha_gad_phq2_uw <- reliability_table_uw(
  data_general,
  c("gad2", "phq2"),
  labels_gad_phq2,
  "PHQ-4 components (GAD-2 and PHQ-2 subscale scores)"
)

alpha_gad_phq2_w <- reliability_table_w(
  data_general,
  c("gad2", "phq2"),
  labels_gad_phq2,
  "PHQ-4 components (GAD-2 and PHQ-2 subscale scores)"
)

## Table 3. Cronbach PHQ-4 items (GAD 1, GAD 2, PHQ 1, PHQ 2) ---------------

item4 <- c("sm3", "sm4", "sm1", "sm2")
labels4 <- c("GAD 1", "GAD 2", "PHQ 1", "PHQ 2")
names(labels4) <- labels4

alpha_items_uw <- reliability_table_uw(
  data_general,
  item4,
  labels4,
  "PHQ-4 items (GAD 1, GAD 2, PHQ 1, PHQ 2)"
)

alpha_items_w <- reliability_table_w(
  data_general,
  item4,
  labels4,
  "PHQ-4 items (GAD 1, GAD 2, PHQ 1, PHQ 2)"
)

## Notes --------------------------------------------------------------------

notes_tbl <- tibble::tibble(
  topic = c(
    "Spearman (unweighted)",
    "Spearman (weighted)",
    "Cronbach alpha (unweighted)",
    "Cronbach alpha (weighted)",
    "Reliability columns alpha, se",
    "p-values"
  ),
  description = c(
    "Below diagonal: Spearman rho; above diagonal: two-sided p-value from cor.test(..., exact = FALSE).",
    "Design-based Pearson correlation on survey-weighted ranks (average ranks); above diagonal: p-value from Wald test in svyglm(rank_i ~ rank_j).",
    "psych::alpha: raw alpha with asymptotic SE (ase) for Global; alpha if item deleted with alpha se when available.",
    "Alpha from weighted covariance (cov.wt, ML); SE from cluster bootstrap resampling clusters (stratum x PSU), 120 replicates.",
    "Alpha with two decimals; SE with three decimals so small asymptotic SEs do not round to zero (NA if SE not defined).",
    "If p < 0.01, reported as P=0.00; otherwise three decimals (P=0.XXX)."
  )
)

out_dir <- "02_Output/Descriptives"

out_xlsx <- file.path(out_dir, "correlation_reliability_tables.xlsx")

writexl::write_xlsx(
  list(
    Spearman_unweighted = spearman_uw,
    Spearman_weighted = spearman_w,
    Alpha_PHQ4_GAD2_PHQ2_unweighted = alpha_gad_phq2_uw,
    Alpha_PHQ4_GAD2_PHQ2_weighted = alpha_gad_phq2_w,
    Alpha_PHQ4_items_unweighted = alpha_items_uw,
    Alpha_PHQ4_items_weighted = alpha_items_w,
    Notes = notes_tbl
  ),
  path = out_xlsx
)

