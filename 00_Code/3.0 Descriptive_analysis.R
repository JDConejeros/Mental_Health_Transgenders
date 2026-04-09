# 3.0 Descriptive analysis -----------------------------------------

# Load project settings
source("00_Code/0.1 Settings.R")
source("00_Code/0.2 Packages.R")
source("00_Code/0.3 Functions.R")

options(survey.lonely.psu = "adjust")

# Analysis dataset: ENSSEX population file; trans subgroup = trans_from_gender == "Yes"
data <- rio::import("02_Output/Data/ENSSEX_trans_population.RData")

data <- data |>
  dplyr::select(-starts_with("suicidal_")) |>
  dplyr::select(-c(gender_identity, trans_from_q3, sexual_orientation, sexual_talk_family, edad, sex)) |>
  tidyr::drop_na()

data_general <- data
data_trans <- data |> dplyr::filter(trans_from_gender == "Yes")

## Table 1. Descriptive statistics for requested variables in general ------
# Variables requested
numeric_vars <- c("sm1", "sm2", "sm3", "sm4", "phq2", "gad2", "phq4")
categorical_vars <- c(
  "phq2_flag", "gad2_flag", "phq4_flag",
  "education_level", "age_group_4", "indigenous_status",
  "nationality", "health_insurance", "quality_of_life"
)

# English labels for output
var_labels <- c(
  sm1 = "PHQ item 1: Little interest or pleasure in doing things",
  sm2 = "PHQ item 2: Feeling down, depressed, or hopeless",
  sm3 = "GAD item 1: Feeling nervous, anxious, or on edge",
  sm4 = "GAD item 2: Not being able to stop or control worrying",
  phq2 = "PHQ-2 total score",
  gad2 = "GAD-2 total score",
  phq4 = "PHQ-4 total score",
  phq2_flag = "Positive depression screening (PHQ-2 >= 3)",
  gad2_flag = "Positive anxiety screening (GAD-2 >= 3)",
  phq4_flag = "Positive general distress screening (PHQ-4 >= 6)",
  education_level = "Education level",
  age_group_4 = "Age group",
  indigenous_status = "Indigenous status",
  nationality = "Nationality",
  health_insurance = "Health insurance",
  quality_of_life = "Quality of life"
)

fmt_num <- function(x, digits = 2) {
  format(round(x, digits), nsmall = digits, decimal.mark = ".", trim = TRUE)
}

to_yes_no <- function(x) {
  if (is.logical(x)) {
    factor(if_else(x, "Yes", "No"), levels = c("No", "Yes"))
  } else if (is.factor(x)) {
    x
  } else {
    as.factor(x)
  }
}

describe_numeric_sample <- function(df, vars) {
  purrr::map_dfr(vars, function(v) {
    x <- df[[v]]
    x <- x[!is.na(x)]
    tibble::tibble(
      Variable = var_labels[[v]],
      Level = NA_character_,
      Statistic = paste0(
        fmt_num(mean(x)), " (SD=", fmt_num(stats::sd(x)), ") [",
        fmt_num(min(x)), " - ", fmt_num(max(x)), "]"
      )
    )
  })
}

describe_numeric_weighted <- function(svy, vars) {
  purrr::map_dfr(vars, function(v) {
    valid <- svy |>
      dplyr::filter(!is.na(.data[[v]]))
    x <- valid$variables[[v]]
    mean_out <- valid |>
      summarise(mean = survey_mean(.data[[v]], vartype = NULL))
    sd_weighted <- tryCatch(
      as.numeric(
        valid |>
          summarise(sd = survey_sd(.data[[v]], vartype = NULL)) |>
          dplyr::pull(sd)
      ),
      error = function(e) stats::sd(x)
    )
    tibble::tibble(
      Variable = var_labels[[v]],
      Level = NA_character_,
      Statistic = paste0(
        fmt_num(mean_out$mean), " (SD=", fmt_num(sd_weighted), ") [",
        fmt_num(min(x)), " - ", fmt_num(max(x)), "]"
      )
    )
  })
}

describe_categorical_sample <- function(df, vars) {
  purrr::map_dfr(vars, function(v) {
    x <- to_yes_no(df[[v]])
    tab <- tibble::tibble(value = x) |>
      dplyr::filter(!is.na(value)) |>
      dplyr::count(value, name = "n") |>
      dplyr::mutate(pct = n / sum(n) * 100)
    tab |>
      dplyr::transmute(
        Variable = var_labels[[v]],
        Level = as.character(value),
        Statistic = paste0(fmt_num(pct), "% (n=", n, ")")
      )
  })
}

describe_categorical_weighted <- function(svy, vars) {
  purrr::map_dfr(vars, function(v) {
    svy2 <- svy |>
      mutate(.cat = to_yes_no(.data[[v]])) |>
      dplyr::filter(!is.na(.cat))
    levs <- unique(as.character(svy2$variables$.cat))
    purrr::map_dfr(levs, function(lev) {
      tmp <- svy2 |>
        mutate(.hit = as.integer(as.character(.cat) == lev))
      out <- tmp |>
        summarise(
          pct = survey_mean(.hit, vartype = NULL) * 100,
          n_w = survey_total(.hit, vartype = NULL)
        )
      tibble::tibble(
        Variable = var_labels[[v]],
        Level = lev,
        Statistic = paste0(fmt_num(out$pct), "% (n=", fmt_num(out$n_w, digits = 0), ")")
      )
    })
  })
}

build_descriptive_table <- function(df, weighted = FALSE) {
  if (weighted) {
    enssex_srvyr <- df |>
      as_survey_design(
        ids = varunit,
        strata = varstrat,
        weights = w_personas_cal,
        nest = TRUE
      )
    dplyr::bind_rows(
      describe_numeric_weighted(enssex_srvyr, numeric_vars),
      describe_categorical_weighted(enssex_srvyr, categorical_vars)
    )
  } else {
    dplyr::bind_rows(
      describe_numeric_sample(df, numeric_vars),
      describe_categorical_sample(df, categorical_vars)
    )
  }
}

# Survey designs for sample-size reporting
design_general <- data_general |>
  as_survey_design(
    ids = varunit,
    strata = varstrat,
    weights = w_personas_cal,
    nest = TRUE
  )

design_trans <- data_trans |>
  as_survey_design(
    ids = varunit,
    strata = varstrat,
    weights = w_personas_cal,
    nest = TRUE
  )

# Sample sizes (unweighted and weighted)
n_general_unw <- nrow(data_general)
n_trans_unw <- nrow(data_trans)
n_general_w <- as.numeric(summarise(design_general, n_w = survey_total(vartype = NULL))$n_w)
n_trans_w <- as.numeric(summarise(design_trans, n_w = survey_total(vartype = NULL))$n_w)

sample_sizes <- tibble::tibble(
  sample = c("General_sample", "General_weighted", "Trans_sample", "Trans_weighted"),
  n_unweighted = c(n_general_unw, n_general_unw, n_trans_unw, n_trans_unw),
  n_weighted = c(n_general_w, n_general_w, n_trans_w, n_trans_w)
)

# Build requested tables
desc_general_sample <- build_descriptive_table(data_general, weighted = FALSE)
desc_general_weighted <- build_descriptive_table(data_general, weighted = TRUE)
desc_trans_sample <- build_descriptive_table(data_trans, weighted = FALSE)
desc_trans_weighted <- build_descriptive_table(data_trans, weighted = TRUE)

# Consolidated table with four samples
desc_general_sample <- desc_general_sample |>
  mutate(Sample = paste0("General_sample (n=", n_general_unw, ")"))

desc_general_weighted <- desc_general_weighted |>
  mutate(Sample = paste0(
    "General_weighted (n_unw=", n_general_unw,
    ", n_w=", fmt_num(n_general_w, digits = 0), ")"
  ))

desc_trans_sample <- desc_trans_sample |>
  mutate(Sample = paste0("Trans_sample (n=", n_trans_unw, ")"))

desc_trans_weighted <- desc_trans_weighted |>
  mutate(Sample = paste0(
    "Trans_weighted (n_unw=", n_trans_unw,
    ", n_w=", fmt_num(n_trans_w, digits = 0), ")"
  ))

desc_consolidated <- dplyr::bind_rows(
  desc_general_sample,
  desc_general_weighted,
  desc_trans_sample,
  desc_trans_weighted
) |>
  mutate(Level = dplyr::coalesce(Level, "")) |>
  tidyr::pivot_wider(
    id_cols = c(Variable, Level),
    names_from = Sample,
    values_from = Statistic
  ) |>
  arrange(Variable, Level)

# Export to Excel using a named list
output_dir <- "02_Output/Descriptives"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

descriptive_results <- list(
  General_sample = desc_general_sample |>
    dplyr::select(-Sample),
  General_weighted = desc_general_weighted |>
    dplyr::select(-Sample),
  Trans_sample = desc_trans_sample |>
    dplyr::select(-Sample),
  Trans_weighted = desc_trans_weighted |>
    dplyr::select(-Sample),
  Consolidated = desc_consolidated,
  Sample_sizes = sample_sizes
)

## Figure 1. GAD-2 / PHQ-2 / PHQ-4 boxplots (unweighted, no violin) ------

plot_data <- data_general |>
  dplyr::mutate(
    trans = factor(
      dplyr::if_else(trans_from_gender == "Yes", "Transgender", "General"),
      levels = c("General", "Transgender")
    )
  )

fill_cols <- c(General = "#d9d9d9", Transgender = "#b3b3b3")
outline_cols <- c(General = "#4d4d4d", Transgender = "#333333")

fmt_dec <- function(x, digits = 2L) {
  format(round(as.numeric(x), digits), nsmall = digits, trim = TRUE, decimal.mark = ".")
}

fmt_p <- function(p, digits = 3L) {
  fmt_dec(p, digits)
}

welch_caption <- function(df, y_chr) {
  res <- suppressWarnings(
    statsExpressions::two_sample_test(
      data = df,
      x = trans,
      y = !!rlang::sym(y_chr),
      type = "parametric",
      paired = FALSE,
      var.equal = FALSE,
      conf.level = 0.95,
      digits = 3L
    )
  )
  r <- res[1, ]
  paste0(
    "Welch's t-test: tWelch(",
    fmt_dec(r$df.error), ") = ", fmt_dec(r$statistic),
    ", p = ", fmt_p(r$p.value),
    "\nHedges' g = ", fmt_dec(r$estimate),
    ", 95% CI [", fmt_dec(r$conf.low), ", ", fmt_dec(r$conf.high),
    "], n_obs = ", nrow(df), "."
  )
}
ggbetween_mh <- function(df, y_chr, panel_title, y_max, y_breaks) {
  # insight::format_value (used for mean labels) follows getOption("OutDec")
  old_outdec <- options(OutDec = ".")
  on.exit(options(old_outdec), add = TRUE)
  cap <- welch_caption(df, y_chr)
  p <- ggstatsplot::ggbetweenstats(
    data = df,
    x = trans,
    y = !!rlang::sym(y_chr),
    var.equal = FALSE,
    type = "parametric",
    conf.level = 0.95,
    digits = 2L,
    bf.message = FALSE,
    results.subtitle = FALSE,
    subtitle = NULL,
    title = panel_title,
    caption = cap,
    point.args = list(alpha = 0),
    centrality.plotting = TRUE,
    centrality.point.args = list(size = 2, color = "firebrick"),
    centrality.label.args = list(size = 2.5, color = "black", nudge_x = 0.22, segment.linetype = 0),
    boxplot.args = list(
      width = 0.35,
      alpha = 0.88,
      linewidth = 0.45,
      na.rm = TRUE
    ),
    package = "RColorBrewer",
    palette = "Dark2",
    ggplot.component = list(
      ggplot2::scale_fill_manual(values = fill_cols),
      ggplot2::scale_color_manual(values = outline_cols),
      ggplot2::scale_y_continuous(limits = c(0, y_max), breaks = y_breaks)
    ),
    ggtheme = ggplot2::theme_light()
  )
  viol_idx <- vapply(p$layers, function(L) inherits(L$geom, "GeomViolin"), logical(1))
  if (any(viol_idx)) p$layers <- p$layers[!viol_idx]
  p <- p +
    ggplot2::theme(
      legend.position = "none",
      plot.title = element_text(size = 10),
      plot.caption = element_text(size = 6, hjust = 0, lineheight = 1.05),
      axis.title = element_blank(),
      axis.text = element_text(size = 8),
      panel.grid = element_blank()
    )
}

g_gad_u <- ggbetween_mh(plot_data, "gad2", "C. GAD-2 score", 6, 0:6)
g_phq2_u <- ggbetween_mh(plot_data, "phq2", "D. PHQ-2 score", 6, 0:6)
g_phq4_u <- ggbetween_mh(plot_data, "phq4", "E. PHQ-4 score", 12, 0:12)

patch_row1 <- patchwork::wrap_plots(g_gad_u, g_phq2_u, g_phq4_u, nrow = 1)

ggplot2::ggsave(
  filename = file.path(output_dir, "figure_mh_gad_phq_boxplots_unweighted_only.png"),
  plot = patch_row1,
  width = 10,
  height = 4,
  dpi = 300,
  bg = "white"
)

## Figure 2. Item-level stacked % (rows: GAD 1, GAD 2, PHQ 1, PHQ 2) ------
# GAD item 1 (sm3): Feeling nervous, anxious, or on edge -> GAD 1
# GAD item 2 (sm4): Not being able to stop or control worrying -> GAD 2
# PHQ item 1 (sm1): Little interest or pleasure in doing things -> PHQ 1
# PHQ item 2 (sm2): Feeling down, depressed, or hopeless -> PHQ 2
# Response coding: 0 Never, 1 Some days, 2 More than half the days, 3 Almost every day

item_plot_order <- c("sm3", "sm4", "sm1", "sm2")
item_plot_labels <- c(
  sm3 = "GAD 1",
  sm4 = "GAD 2",
  sm1 = "PHQ 1",
  sm2 = "PHQ 2"
)
response_levels_item <- 0:3
response_labels_item <- c(
  "Never",
  "Some days",
  "More than half the days",
  "Almost every day"
)

build_item_response_pct <- function(df) {
  purrr::map_dfr(item_plot_order, function(v) {
    x <- df[[v]]
    x <- x[!is.na(x)]
    x <- x[x %in% response_levels_item]
    tab <- table(factor(x, levels = response_levels_item))
    tibble::tibble(
      response_raw = response_levels_item,
      n = as.integer(tab[as.character(response_levels_item)])
    ) |>
      dplyr::mutate(
        item = item_plot_labels[[v]],
        response = factor(
          response_raw,
          levels = response_levels_item,
          labels = response_labels_item
        )
      ) |>
      dplyr::select(-response_raw)
  }) |>
    dplyr::group_by(item) |>
    dplyr::mutate(pct = n / sum(n) * 100) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      item = factor(item, levels = unname(item_plot_labels[item_plot_order]))
    )
}

# Collected legend for A|B and for composite (top, horizontal, no title)
theme_item_legend_top <- ggplot2::theme(
  legend.position = "top",
  legend.direction = "horizontal",
  legend.title = ggplot2::element_blank(),
  legend.justification = "center",
  legend.box.just = "center",
  legend.key.size = grid::unit(0.4, "cm"),
  legend.text = ggplot2::element_text(size = 7.5),
  legend.margin = ggplot2::margin(b = 2, t = 2)
)

plot_item_stacked <- function(df, panel_title) {
  d <- build_item_response_pct(df) |>
    dplyr::mutate(
      pct_lbl = paste0(as.integer(round(pct)), "%"),
      lbl_color = dplyr::if_else(
        as.integer(response) >= 3L,
        "white",
        "gray10"
      )
    )
  ggplot2::ggplot(d, ggplot2::aes(x = pct, y = item, fill = response)) +
    ggplot2::geom_col(width = 0.72, color = "white", linewidth = 0.25) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = dplyr::if_else(pct >= 1.5, pct_lbl, ""),
        colour = lbl_color
      ),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 2.6,
      fontface = "bold",
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_identity(guide = "none") +
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      limits = c(0, 100),
      labels = function(x) paste0(x, "%")
    ) +
    ggplot2::scale_y_discrete(limits = rev(levels(d$item))) +
    ggplot2::scale_fill_brewer(palette = "PuBu", direction = 1) +
    ggplot2::labs(title = panel_title, x = NULL, y = NULL) +
    ggplot2::theme_light() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 8),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

p_items_full <- plot_item_stacked(
  data_general,
  paste0("A. Full Sample (n=", n_general_unw, ")")
)
p_items_trans <- plot_item_stacked(
  data_trans,
  paste0("B. Transgender and gender-diverse (n=", n_trans_unw, ")")
)

patch_items_ab <- (p_items_full | p_items_trans) +
  patchwork::plot_layout(widths = c(1, 1), guides = "collect") &
  theme_item_legend_top

ggplot2::ggsave(
  filename = file.path(output_dir, "figure_item_response_stacked_full_vs_trans.png"),
  plot = patch_items_ab,
  width = 10,
  height = 4.6,
  dpi = 300,
  bg = "white"
)

## Figure 3. Composite: stacked items (A/B) | gap | boxplots (C–E) ------

patch_items_col <- (p_items_full / p_items_trans) +
  patchwork::plot_layout(heights = c(1, 1), guides = "collect") &
  theme_item_legend_top

patch_boxes_col <- (g_gad_u / g_phq2_u / g_phq4_u) +
  patchwork::plot_layout(nrow = 3, heights = c(1, 1, 1))

fig_items_and_boxplots <- (
  patch_items_col | patchwork::plot_spacer() | patch_boxes_col
) +
  patchwork::plot_layout(widths = c(0.6, 0.01, 0.39))

ggplot2::ggsave(
  filename = file.path(output_dir, "figure_items_stacked_and_mh_boxplots_composite.png"),
  plot = fig_items_and_boxplots,
  width = 9,
  height = 8,
  dpi = 300,
  bg = "white"
)

## Table: Scoring by groups (GAD-2, PHQ-2, PHQ-4) -------------------------

# Trans vs cis: Welch t-test; education / insurance / QoL: Welch one-way ANOVA;
# indigenous / nationality: Welch t-test. Weighted: svyttest / svyglm + regTermTest.

mh_outcomes <- c("gad2", "phq2", "phq4")
mh_group_specs <- tibble::tibble(
  comparison_id = c("trans", "edu", "indig", "nat", "hi", "qol"),
  group_var = c(
    "trans_from_gender",
    "education_level",
    "indigenous_status",
    "nationality",
    "health_insurance",
    "quality_of_life"
  ),
  test_type = c("ttest", "anova", "ttest", "ttest", "anova", "anova"),
  comparison_label = c(
    "Transgender vs cisgender",
    "Education level",
    "Indigenous status",
    "Nationality (Chilean vs migrant)",
    "Health insurance",
    "Quality of life"
  )
)

vars_mh_groups <- unique(c(
  mh_outcomes,
  mh_group_specs$group_var,
  "varunit",
  "varstrat",
  "w_personas_cal"
))

d_mh <- data_general |>
  dplyr::select(dplyr::any_of(vars_mh_groups)) |>
  tidyr::drop_na(dplyr::everything()) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.factor), droplevels))

design_mh <- d_mh |>
  as_survey_design(
    ids = varunit,
    strata = varstrat,
    weights = w_personas_cal,
    nest = TRUE
  )

fmt1 <- function(x) {
  sprintf("%.1f", round(as.numeric(x), 1L))
}

fmt_mean_ci95 <- function(m, lo, hi) {
  if (any(!is.finite(c(m, lo, hi)))) {
    return(NA_character_)
  }
  paste0(fmt1(m), " [", fmt1(lo), " - ", fmt1(hi), "]")
}

fmt_p_mh <- function(p) {
  p <- as.numeric(p)
  if (!is.finite(p)) {
    return(NA_character_)
  }
  if (p < 0.01) {
    return("0.00")
  }
  sprintf("%.2f", p)
}

fmt_stat2 <- function(x) {
  sprintf("%.2f", as.numeric(x))
}

mean_ci_uw <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 2L) {
    return(list(m = NA_real_, lo = NA_real_, hi = NA_real_))
  }
  m <- mean(x)
  se <- stats::sd(x) / sqrt(n)
  tc <- stats::qt(0.975, df = n - 1L)
  list(m = m, lo = m - tc * se, hi = m + tc * se)
}

relabel_group_level <- function(comparison_id, lev_chr) {
  if (comparison_id == "trans") {
    if (identical(lev_chr, "No")) {
      return("Cisgender")
    }
    if (identical(lev_chr, "Yes")) {
      return("Transgender")
    }
  }
  if (comparison_id == "nat" && identical(lev_chr, "Other")) {
    return("Migrant")
  }
  as.character(lev_chr)
}

build_one_comparison_uw <- function(dat, y_chr, spec_row) {
  g <- spec_row$group_var
  comp_id <- spec_row$comparison_id
  comp_lab <- spec_row$comparison_label
  tt <- spec_row$test_type
  yl <- var_labels[[y_chr]]

  d <- dat |>
    dplyr::filter(!is.na(.data[[y_chr]]), !is.na(.data[[g]])) |>
    dplyr::mutate(grp_tmp = droplevels(as.factor(.data[[g]])))

  if (nrow(d) < 4L || nlevels(d$grp_tmp) < 2L) {
    return(NULL)
  }

  levs <- levels(d$grp_tmp)
  rows <- list()

  if (identical(tt, "ttest")) {
    if (nlevels(d$grp_tmp) != 2L) {
      return(NULL)
    }
    wt <- stats::t.test(
      d[[y_chr]] ~ d$grp_tmp,
      var.equal = FALSE
    )
    test_str <- paste0("t=", fmt_stat2(wt$statistic))
    p_str <- fmt_p_mh(wt$p.value)
    for (j in seq_along(levs)) {
      xv <- d[[y_chr]][d$grp_tmp == levs[j]]
      mc <- mean_ci_uw(xv)
      rows[[j]] <- tibble::tibble(
        outcome = yl,
        comparison = comp_lab,
        group = relabel_group_level(comp_id, levs[j]),
        mean_ci_95 = fmt_mean_ci95(mc$m, mc$lo, mc$hi),
        test_statistic = if (j == 1L) test_str else NA_character_,
        p_value = if (j == 1L) p_str else NA_character_
      )
    }
  } else {
    ow <- stats::oneway.test(
      d[[y_chr]] ~ d$grp_tmp,
      var.equal = FALSE
    )
    test_str <- paste0("F=", fmt_stat2(ow$statistic))
    p_str <- fmt_p_mh(ow$p.value)
    for (j in seq_along(levs)) {
      xv <- d[[y_chr]][d$grp_tmp == levs[j]]
      mc <- mean_ci_uw(xv)
      rows[[j]] <- tibble::tibble(
        outcome = yl,
        comparison = comp_lab,
        group = relabel_group_level(comp_id, levs[j]),
        mean_ci_95 = fmt_mean_ci95(mc$m, mc$lo, mc$hi),
        test_statistic = if (j == 1L) test_str else NA_character_,
        p_value = if (j == 1L) p_str else NA_character_
      )
    }
  }

  dplyr::bind_rows(rows)
}

build_one_comparison_w <- function(dat, design, y_chr, spec_row) {
  g <- spec_row$group_var
  comp_id <- spec_row$comparison_id
  comp_lab <- spec_row$comparison_label
  tt <- spec_row$test_type
  yl <- var_labels[[y_chr]]

  d <- dat |>
    dplyr::filter(!is.na(.data[[y_chr]]), !is.na(.data[[g]])) |>
    dplyr::mutate(grp_tmp = droplevels(as.factor(.data[[g]])))

  if (nrow(d) < 4L || nlevels(d$grp_tmp) < 2L) {
    return(NULL)
  }

  des <- design |>
    dplyr::filter(!is.na(.data[[y_chr]]), !is.na(.data[[g]])) |>
    dplyr::mutate(grp_tmp = droplevels(as.factor(.data[[g]])))

  levs <- levels(d$grp_tmp)
  rows <- list()

  by_f <- stats::as.formula(paste0("~", y_chr))
  by_g <- stats::as.formula("~grp_tmp")
  mb <- survey::svyby(by_f, by_g, des, survey::svymean, na.rm = TRUE, covmat = TRUE)
  ci_mat <- stats::confint(mb)

  if (identical(tt, "ttest")) {
    if (nlevels(d$grp_tmp) != 2L) {
      return(NULL)
    }
    st <- survey::svyttest(
      stats::reformulate("grp_tmp", response = y_chr),
      des
    )
    test_str <- paste0("t=", fmt_stat2(st$statistic))
    p_str <- fmt_p_mh(st$p.value)
  } else {
    fit <- survey::svyglm(
      stats::reformulate("grp_tmp", response = y_chr),
      design = des
    )
    rt <- survey::regTermTest(fit, ~grp_tmp)
    test_str <- paste0("F=", fmt_stat2(as.numeric(rt$Ftest)))
    p_str <- fmt_p_mh(as.numeric(rt$p))
  }

  mf <- as.data.frame(mb)
  ycol <- match(y_chr, names(mf))
  if (is.na(ycol)) {
    ycol <- 2L
  }
  gcol <- match("grp_tmp", names(mf))
  if (is.na(gcol)) {
    gcol <- 1L
  }

  for (j in seq_len(nrow(mf))) {
    lev_j <- as.character(mf[j, gcol])
    mval <- as.numeric(mf[j, ycol])
    lo <- as.numeric(ci_mat[j, 1L])
    hi <- as.numeric(ci_mat[j, 2L])
    rows[[j]] <- tibble::tibble(
      outcome = yl,
      comparison = comp_lab,
      group = relabel_group_level(comp_id, lev_j),
      mean_ci_95 = fmt_mean_ci95(mval, lo, hi),
      test_statistic = if (j == 1L) test_str else NA_character_,
      p_value = if (j == 1L) p_str else NA_character_
    )
  }

  dplyr::bind_rows(rows)
}

mh_by_groups_uw <- purrr::map_dfr(mh_outcomes, function(y) {
  purrr::map_dfr(
    seq_len(nrow(mh_group_specs)),
    function(i) {
      build_one_comparison_uw(d_mh, y, mh_group_specs[i, ])
    }
  )
})

mh_by_groups_w <- purrr::map_dfr(mh_outcomes, function(y) {
  purrr::map_dfr(
    seq_len(nrow(mh_group_specs)),
    function(i) {
      build_one_comparison_w(d_mh, design_mh, y, mh_group_specs[i, ])
    }
  )
})

descriptive_results <- c(
  descriptive_results,
  list(
    MH_by_groups_unweighted = mh_by_groups_uw,
    MH_by_groups_weighted = mh_by_groups_w,
    MH_by_groups_notes = tibble::tibble(
      topic = c(
        "Means",
        "Tests (unweighted)",
        "Tests (weighted)",
        "p-values",
        "Nationality",
        "Trans"
      ),
      detail = c(
        "Group mean and 95% CI; one decimal.",
        "Welch t-test (2 groups) or Welch one-way ANOVA (oneway.test, var.equal=FALSE).",
        "Design-based t-test (svyttest) or Wald F-test from svyglm + regTermTest.",
        "Two decimals; values < 0.01 shown as 0.00.",
        "Other nationality labelled Migrant in the group column.",
        "Cisgender = trans_from_gender No; Transgender = Yes."
      )
    )
  )
)

writexl::write_xlsx(
  descriptive_results,
  path = file.path(output_dir, "descriptive_tables.xlsx")
)
