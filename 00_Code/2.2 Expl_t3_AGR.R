# 2.2 Análisis Descriptivos - Variables de Agresiones, Violencia y Acoso --------

# Cargamos configuraciones previas 
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")
source("Code/0.3 Funciones.R")

# Cargamos librería para diseño muestral complejo
install_load(c("survey", "srvyr"))

# Abrimos los datos ------
data <- rio::import("Output/ENSSEX_TRANS.RData")
glimpse(data); names(data)

# Verificamos estructura de variables
table(data$agr1, useNA = "always")
table(data$violen1, useNA = "always")
table(data$acoso1, useNA = "always")

table(data$n_agr1, useNA = "always")
table(data$n_violen1, useNA = "always")
table(data$n_acoso1, useNA = "always")

table(data$trans1, useNA = "always")
table(data$trans2, useNA = "always")

# Preparamos datos: creamos variables trans1 y trans2 como factores
data_clean <- data |>
  mutate(
    # Variables binarias de agresiones (0-1)
    agr1 = as.numeric(agr1),
    agr2 = as.numeric(agr2),
    agr3 = as.numeric(agr3),
    agr4 = as.numeric(agr4),
    agr5 = as.numeric(agr5),
    agr6 = as.numeric(agr6),
    agr7 = as.numeric(agr7),
    # Variables numéricas de número de agresiones (1-4)
    n_agr1 = as.numeric(n_agr1),
    n_agr2 = as.numeric(n_agr2),
    n_agr3 = as.numeric(n_agr3),
    n_agr4 = as.numeric(n_agr4),
    n_agr5 = as.numeric(n_agr5),
    n_agr6 = as.numeric(n_agr6),
    n_agr7 = as.numeric(n_agr7),
    # Variables binarias de violencia sexual (0-1)
    violen1 = as.numeric(violen1),
    violen2 = as.numeric(violen2),
    violen3 = as.numeric(violen3),
    # Variables numéricas de frecuencia de violencia sexual
    n_violen1 = as.numeric(n_violen1),
    n_violen2 = as.numeric(n_violen2),
    # Variables binarias de acoso (0-1)
    acoso1 = as.numeric(acoso1),
    acoso2 = as.numeric(acoso2),
    acoso3 = as.numeric(acoso3),
    acoso4 = as.numeric(acoso4),
    # Variables numéricas de frecuencia de acoso
    n_acoso1 = as.numeric(n_acoso1),
    n_acoso2 = as.numeric(n_acoso2),
    n_acoso3 = as.numeric(n_acoso3),
    n_acoso4 = as.numeric(n_acoso4),
    # Variables de lugar del acoso
    l_acoso1 = as.numeric(l_acoso1),
    l_acoso2 = as.numeric(l_acoso2),
    l_acoso3 = as.numeric(l_acoso3),
    l_acoso4 = as.numeric(l_acoso4),
    # Creamos variables de grupo para trans1 y trans2
    trans1_group = factor(if_else(trans1 == 1, "Trans", "No trans", missing = "No trans")),
    trans2_group = factor(if_else(trans2 == 1, "Trans", "No trans", missing = "No trans"))
  )

# Definimos listas de variables por tipo
vars_binarias <- list(
  "Agresiones" = paste0("agr", 1:7),
  "Violencia_sexual" = c("violen1", "violen2", "violen3"),
  "Acoso" = paste0("acoso", 1:4)
)

vars_numericas <- list(
  "Numero_agresiones" = paste0("n_agr", 1:7),
  "Frecuencia_violencia" = c("n_violen1", "n_violen2"),
  "Frecuencia_acoso" = paste0("n_acoso", 1:4)
)

vars_lugar_acoso <- paste0("l_acoso", 1:4)

# Configuramos opción para manejar estratos con un solo PSU
options(survey.lonely.psu = "adjust")

# Creamos UN SOLO diseño muestral complejo con todos los datos
design_completo <- data_clean |>
  as_survey_design(
    ids = varunit,
    strata = varstrat,
    weights = w_personas_cal,
    nest = TRUE
  )

# FUNCIONES PARA VARIABLES BINARIAS (0-1) ------

# Función para calcular estadísticas de variables binarias por grupo
calcular_estadisticas_binarias_por_grupo <- function(design_obj, var_name, grupo_var, filtro_adicional = NULL) {
  
  # Aplicamos filtro adicional si existe
  if (!is.null(filtro_adicional)) {
    design_obj <- design_obj |>
      filter(!!filtro_adicional)
  }
  
  # Creamos variables indicadoras para cada categoría (0 y 1)
  design_obj <- design_obj |>
    mutate(
      cat_0 = as.numeric(.data[[var_name]] == 0),
      cat_1 = as.numeric(.data[[var_name]] == 1),
      has_value = as.numeric(!is.na(.data[[var_name]]))
    )
  
  # Frecuencias ponderadas por grupo
  freq_pond <- design_obj |>
    group_by(across(all_of(grupo_var))) |>
    summarize(
      freq_0 = survey_total(cat_0, na.rm = TRUE, vartype = NULL),
      freq_1 = survey_total(cat_1, na.rm = TRUE, vartype = NULL),
      total = survey_total(has_value, na.rm = TRUE, vartype = NULL)
    ) |>
    mutate(
      pct_0 = round(freq_0 / total * 100, 2),
      pct_1 = round(freq_1 / total * 100, 2)
    ) |>
    select(!!sym(grupo_var), freq_0, pct_0, freq_1, pct_1, total) |>
    as.data.frame()
  
  # Frecuencias sin ponderar por grupo
  freq_no_pond <- design_obj$variables |>
    group_by(across(all_of(grupo_var))) |>
    summarize(
      freq_0 = sum(cat_0, na.rm = TRUE),
      freq_1 = sum(cat_1, na.rm = TRUE),
      total = sum(has_value, na.rm = TRUE)
    ) |>
    mutate(
      pct_0 = round(freq_0 / total * 100, 2),
      pct_1 = round(freq_1 / total * 100, 2)
    ) |>
    as.data.frame()
  
  # % NA y n por grupo (ponderado) - n efectivo del grupo filtrado
  na_n_pond <- design_obj |>
    group_by(across(all_of(grupo_var))) |>
    summarize(
      pct_na = survey_mean(is.na(.data[[var_name]]), na.rm = TRUE, vartype = NULL) * 100,
      n = survey_total(na.rm = TRUE, vartype = NULL)
    ) |>
    as.data.frame()
  
  # % NA y n por grupo (sin ponderar) - n efectivo del grupo filtrado
  na_n_no_pond <- design_obj$variables |>
    group_by(across(all_of(grupo_var))) |>
    summarize(
      pct_na = mean(is.na(.data[[var_name]]), na.rm = TRUE) * 100,
      n = n()
    ) |>
    as.data.frame()
  
  return(list(
    freq_pond = freq_pond,
    freq_no_pond = freq_no_pond,
    na_n_pond = na_n_pond,
    na_n_no_pond = na_n_no_pond
  ))
}

# Función para construir la tabla final para una variable binaria
construir_tabla_binaria <- function(stats_result, var_name) {
  
  # Extraemos datos por grupo
  trans_pond <- stats_result$freq_pond |> filter(trans_group == "Trans")
  no_trans_pond <- stats_result$freq_pond |> filter(trans_group == "No trans")
  trans_no_pond <- stats_result$freq_no_pond |> filter(trans_group == "Trans")
  no_trans_no_pond <- stats_result$freq_no_pond |> filter(trans_group == "No trans")
  
  trans_na_n_pond <- stats_result$na_n_pond |> filter(trans_group == "Trans")
  no_trans_na_n_pond <- stats_result$na_n_pond |> filter(trans_group == "No trans")
  trans_na_n_no_pond <- stats_result$na_n_no_pond |> filter(trans_group == "Trans")
  no_trans_na_n_no_pond <- stats_result$na_n_no_pond |> filter(trans_group == "No trans")
  
  # Construimos tabla de frecuencias (solo 0 y 1)
  freq_table <- data.frame(
    Categoria = c("No", "Sí"),
    `Trans - Ponderado` = c(
      ifelse(nrow(trans_pond) > 0, paste0(sprintf("%.2f", trans_pond$pct_0[1]), " (", round(trans_pond$freq_0[1]), ")"), "-"),
      ifelse(nrow(trans_pond) > 0, paste0(sprintf("%.2f", trans_pond$pct_1[1]), " (", round(trans_pond$freq_1[1]), ")"), "-")
    ),
    `Trans sin ponderar` = c(
      ifelse(nrow(trans_no_pond) > 0, paste0(sprintf("%.2f", trans_no_pond$pct_0[1]), " (", trans_no_pond$freq_0[1], ")"), "-"),
      ifelse(nrow(trans_no_pond) > 0, paste0(sprintf("%.2f", trans_no_pond$pct_1[1]), " (", trans_no_pond$freq_1[1], ")"), "-")
    ),
    `No trans ponderado` = c(
      ifelse(nrow(no_trans_pond) > 0, paste0(sprintf("%.2f", no_trans_pond$pct_0[1]), " (", round(no_trans_pond$freq_0[1]), ")"), "-"),
      ifelse(nrow(no_trans_pond) > 0, paste0(sprintf("%.2f", no_trans_pond$pct_1[1]), " (", round(no_trans_pond$freq_1[1]), ")"), "-")
    ),
    `No trans sin ponderar` = c(
      ifelse(nrow(no_trans_no_pond) > 0, paste0(sprintf("%.2f", no_trans_no_pond$pct_0[1]), " (", no_trans_no_pond$freq_0[1], ")"), "-"),
      ifelse(nrow(no_trans_no_pond) > 0, paste0(sprintf("%.2f", no_trans_no_pond$pct_1[1]), " (", no_trans_no_pond$freq_1[1], ")"), "-")
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # Filas adicionales para % NA y n
  na_n_table <- data.frame(
    Categoria = c("% NA", "n"),
    `Trans - Ponderado` = c(
      ifelse(nrow(trans_na_n_pond) > 0, sprintf("%.2f", trans_na_n_pond$pct_na[1]), "-"),
      ifelse(nrow(trans_na_n_pond) > 0, sprintf("%.0f", trans_na_n_pond$n[1]), "-")
    ),
    `Trans sin ponderar` = c(
      ifelse(nrow(trans_na_n_no_pond) > 0, sprintf("%.2f", trans_na_n_no_pond$pct_na[1]), "-"),
      ifelse(nrow(trans_na_n_no_pond) > 0, sprintf("%.0f", trans_na_n_no_pond$n[1]), "-")
    ),
    `No trans ponderado` = c(
      ifelse(nrow(no_trans_na_n_pond) > 0, sprintf("%.2f", no_trans_na_n_pond$pct_na[1]), "-"),
      ifelse(nrow(no_trans_na_n_pond) > 0, sprintf("%.0f", no_trans_na_n_pond$n[1]), "-")
    ),
    `No trans sin ponderar` = c(
      ifelse(nrow(no_trans_na_n_no_pond) > 0, sprintf("%.2f", no_trans_na_n_no_pond$pct_na[1]), "-"),
      ifelse(nrow(no_trans_na_n_no_pond) > 0, sprintf("%.0f", no_trans_na_n_no_pond$n[1]), "-")
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # Combinamos frecuencias y filas adicionales (sin estadísticas continuas)
  tabla_var <- rbind(freq_table, na_n_table)
  tabla_var$Variable <- var_name
  
  return(tabla_var)
}

# FUNCIONES PARA VARIABLES NUMÉRICAS/LIKERT -----

# Función para calcular estadísticas numéricas por grupo (adaptada para rangos variables)
calcular_estadisticas_numericas_por_grupo <- function(design_obj, var_name, grupo_var, filtro_adicional = NULL) {
  
  # Aplicamos filtro adicional si existe
  if (!is.null(filtro_adicional)) {
    design_obj <- design_obj |>
      filter(!!filtro_adicional)
  }
  
  # Obtenemos valores únicos de la variable (excluyendo NA)
  valores_unicos <- sort(unique(design_obj$variables[[var_name]][!is.na(design_obj$variables[[var_name]])]))
  
  # Creamos variables indicadoras dinámicamente
  design_obj <- design_obj |>
    mutate(has_value = as.numeric(!is.na(.data[[var_name]])))
  
  # Agregamos indicadoras para cada valor único
  for (val in valores_unicos) {
    design_obj <- design_obj |>
      mutate(!!paste0("cat_", val) := as.numeric(.data[[var_name]] == val))
  }
  
  # Construimos summarize usando svytable por grupo (más simple y robusto)
  # Primero obtenemos frecuencias por grupo usando svyby
  freq_pond_list <- list()
  grupos <- unique(design_obj$variables[[grupo_var]])
  grupos <- grupos[!is.na(grupos)]
  
  for (grupo in grupos) {
    design_subset <- design_obj[design_obj$variables[[grupo_var]] == grupo & !is.na(design_obj$variables[[grupo_var]]), ]
    if (length(design_subset) > 0) {
      freq_table <- survey::svytable(as.formula(paste("~", var_name)), design = design_subset)
      freq_df <- data.frame(
        grupo = grupo,
        categoria = names(freq_table),
        freq = as.numeric(freq_table),
        stringsAsFactors = FALSE
      )
      freq_df$total <- sum(freq_df$freq)
      freq_df$pct <- round(freq_df$freq / freq_df$total * 100, 2)
      freq_pond_list[[as.character(grupo)]] <- freq_df
    }
  }
  
  # Convertimos a formato ancho
  if (length(freq_pond_list) > 0) {
    freq_pond_long <- do.call(rbind, freq_pond_list)
    freq_pond <- freq_pond_long |>
      filter(categoria %in% as.character(valores_unicos)) |>
      tidyr::pivot_wider(
        id_cols = grupo,
        names_from = categoria,
        values_from = c(freq, pct),
        names_sep = "_"
      ) |>
      rename(!!sym(grupo_var) := grupo) |>
      as.data.frame()
    
    # Aseguramos que todas las columnas necesarias existan
    for (val in valores_unicos) {
      if (!paste0("freq_", val) %in% names(freq_pond)) {
        freq_pond[[paste0("freq_", val)]] <- 0
        freq_pond[[paste0("pct_", val)]] <- 0
      }
    }
    
    # Calculamos total
    freq_pond$total <- 0
    for (val in valores_unicos) {
      freq_pond$total <- freq_pond$total + freq_pond[[paste0("freq_", val)]]
    }
  } else {
    # Si no hay datos, creamos estructura vacía
    freq_pond <- data.frame(
      grupo_var = character(),
      total = numeric(),
      stringsAsFactors = FALSE
    )
    colnames(freq_pond)[1] <- grupo_var
    for (val in valores_unicos) {
      freq_pond[[paste0("freq_", val)]] <- numeric()
      freq_pond[[paste0("pct_", val)]] <- numeric()
    }
  }
  
  # Estadísticas continuas ponderadas por grupo
  stats_pond <- design_obj |>
    group_by(across(all_of(grupo_var))) |>
    summarize(
      mean_val = survey_mean(.data[[var_name]], na.rm = TRUE, vartype = NULL),
      median_val = survey_median(.data[[var_name]], na.rm = TRUE),
      sd_val = survey_sd(.data[[var_name]], na.rm = TRUE),
      min_val = min(.data[[var_name]], na.rm = TRUE),
      max_val = max(.data[[var_name]], na.rm = TRUE)
    ) |>
    as.data.frame()
  
  # Frecuencias sin ponderar por grupo
  freq_no_pond <- design_obj$variables |>
    group_by(across(all_of(grupo_var))) |>
    summarize(
      total = sum(!is.na(.data[[var_name]]))
    ) |>
    as.data.frame()
  
  for (val in valores_unicos) {
    freq_no_pond[[paste0("freq_", val)]] <- sum(design_obj$variables[[var_name]] == val, na.rm = TRUE)
    freq_no_pond[[paste0("pct_", val)]] <- round(freq_no_pond[[paste0("freq_", val)]] / freq_no_pond$total * 100, 2)
  }
  
  # Estadísticas continuas sin ponderar por grupo
  stats_no_pond <- design_obj$variables |>
    group_by(across(all_of(grupo_var))) |>
    summarize(
      mean_val = mean(.data[[var_name]], na.rm = TRUE),
      median_val = median(.data[[var_name]], na.rm = TRUE),
      sd_val = sd(.data[[var_name]], na.rm = TRUE),
      min_val = min(.data[[var_name]], na.rm = TRUE),
      max_val = max(.data[[var_name]], na.rm = TRUE)
    ) |>
    as.data.frame()
  
  # % NA y n por grupo (ponderado) - n efectivo del grupo filtrado
  na_n_pond <- design_obj |>
    group_by(across(all_of(grupo_var))) |>
    summarize(
      pct_na = survey_mean(is.na(.data[[var_name]]), na.rm = TRUE, vartype = NULL) * 100,
      n = survey_total(na.rm = TRUE, vartype = NULL)
    ) |>
    as.data.frame()
  
  # % NA y n por grupo (sin ponderar) - n efectivo del grupo filtrado
  na_n_no_pond <- design_obj$variables |>
    group_by(across(all_of(grupo_var))) |>
    summarize(
      pct_na = mean(is.na(.data[[var_name]]), na.rm = TRUE) * 100,
      n = n()
    ) |>
    as.data.frame()
  
  return(list(
    freq_pond = freq_pond,
    stats_pond = stats_pond,
    freq_no_pond = freq_no_pond,
    stats_no_pond = stats_no_pond,
    na_n_pond = na_n_pond,
    na_n_no_pond = na_n_no_pond,
    valores_unicos = valores_unicos
  ))
}

# Función para construir la tabla final para una variable numérica
construir_tabla_numerica <- function(stats_result, var_name) {
  
  valores_unicos <- stats_result$valores_unicos
  
  # Extraemos datos por grupo
  trans_pond <- stats_result$freq_pond |> filter(trans_group == "Trans")
  no_trans_pond <- stats_result$freq_pond |> filter(trans_group == "No trans")
  trans_no_pond <- stats_result$freq_no_pond |> filter(trans_group == "Trans")
  no_trans_no_pond <- stats_result$freq_no_pond |> filter(trans_group == "No trans")
  
  trans_stats_pond <- stats_result$stats_pond |> filter(trans_group == "Trans")
  no_trans_stats_pond <- stats_result$stats_pond |> filter(trans_group == "No trans")
  trans_stats_no_pond <- stats_result$stats_no_pond |> filter(trans_group == "Trans")
  no_trans_stats_no_pond <- stats_result$stats_no_pond |> filter(trans_group == "No trans")
  
  trans_na_n_pond <- stats_result$na_n_pond |> filter(trans_group == "Trans")
  no_trans_na_n_pond <- stats_result$na_n_pond |> filter(trans_group == "No trans")
  trans_na_n_no_pond <- stats_result$na_n_no_pond |> filter(trans_group == "Trans")
  no_trans_na_n_no_pond <- stats_result$na_n_no_pond |> filter(trans_group == "No trans")
  
  # Construimos tabla de frecuencias dinámicamente
  freq_rows <- list()
  for (val in valores_unicos) {
    freq_rows[[length(freq_rows) + 1]] <- data.frame(
      Categoria = as.character(val),
      `Trans - Ponderado` = ifelse(nrow(trans_pond) > 0 && paste0("freq_", val) %in% names(trans_pond),
                                   paste0(sprintf("%.2f", trans_pond[[paste0("pct_", val)]][1]), 
                                          " (", round(trans_pond[[paste0("freq_", val)]][1]), ")"), "-"),
      `Trans sin ponderar` = ifelse(nrow(trans_no_pond) > 0 && paste0("freq_", val) %in% names(trans_no_pond),
                                     paste0(sprintf("%.2f", trans_no_pond[[paste0("pct_", val)]][1]), 
                                            " (", trans_no_pond[[paste0("freq_", val)]][1], ")"), "-"),
      `No trans ponderado` = ifelse(nrow(no_trans_pond) > 0 && paste0("freq_", val) %in% names(no_trans_pond),
                                     paste0(sprintf("%.2f", no_trans_pond[[paste0("pct_", val)]][1]), 
                                            " (", round(no_trans_pond[[paste0("freq_", val)]][1]), ")"), "-"),
      `No trans sin ponderar` = ifelse(nrow(no_trans_no_pond) > 0 && paste0("freq_", val) %in% names(no_trans_no_pond),
                                        paste0(sprintf("%.2f", no_trans_no_pond[[paste0("pct_", val)]][1]), 
                                               " (", no_trans_no_pond[[paste0("freq_", val)]][1], ")"), "-"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  freq_table <- do.call(rbind, freq_rows)
  
  # Estadísticas continuas
  stats_table <- data.frame(
    Categoria = c("Promedio", "P50", "SD", "Min", "Max"),
    `Trans - Ponderado` = c(
      ifelse(nrow(trans_stats_pond) > 0, sprintf("%.2f", trans_stats_pond$mean_val[1]), "-"),
      ifelse(nrow(trans_stats_pond) > 0, sprintf("%.2f", trans_stats_pond$median_val[1]), "-"),
      ifelse(nrow(trans_stats_pond) > 0, sprintf("%.2f", trans_stats_pond$sd_val[1]), "-"),
      ifelse(nrow(trans_stats_pond) > 0, sprintf("%.2f", trans_stats_pond$min_val[1]), "-"),
      ifelse(nrow(trans_stats_pond) > 0, sprintf("%.2f", trans_stats_pond$max_val[1]), "-")
    ),
    `Trans sin ponderar` = c(
      ifelse(nrow(trans_stats_no_pond) > 0, sprintf("%.2f", trans_stats_no_pond$mean_val[1]), "-"),
      ifelse(nrow(trans_stats_no_pond) > 0, sprintf("%.2f", trans_stats_no_pond$median_val[1]), "-"),
      ifelse(nrow(trans_stats_no_pond) > 0, sprintf("%.2f", trans_stats_no_pond$sd_val[1]), "-"),
      ifelse(nrow(trans_stats_no_pond) > 0, sprintf("%.2f", trans_stats_no_pond$min_val[1]), "-"),
      ifelse(nrow(trans_stats_no_pond) > 0, sprintf("%.2f", trans_stats_no_pond$max_val[1]), "-")
    ),
    `No trans ponderado` = c(
      ifelse(nrow(no_trans_stats_pond) > 0, sprintf("%.2f", no_trans_stats_pond$mean_val[1]), "-"),
      ifelse(nrow(no_trans_stats_pond) > 0, sprintf("%.2f", no_trans_stats_pond$median_val[1]), "-"),
      ifelse(nrow(no_trans_stats_pond) > 0, sprintf("%.2f", no_trans_stats_pond$sd_val[1]), "-"),
      ifelse(nrow(no_trans_stats_pond) > 0, sprintf("%.2f", no_trans_stats_pond$min_val[1]), "-"),
      ifelse(nrow(no_trans_stats_pond) > 0, sprintf("%.2f", no_trans_stats_pond$max_val[1]), "-")
    ),
    `No trans sin ponderar` = c(
      ifelse(nrow(no_trans_stats_no_pond) > 0, sprintf("%.2f", no_trans_stats_no_pond$mean_val[1]), "-"),
      ifelse(nrow(no_trans_stats_no_pond) > 0, sprintf("%.2f", no_trans_stats_no_pond$median_val[1]), "-"),
      ifelse(nrow(no_trans_stats_no_pond) > 0, sprintf("%.2f", no_trans_stats_no_pond$sd_val[1]), "-"),
      ifelse(nrow(no_trans_stats_no_pond) > 0, sprintf("%.2f", no_trans_stats_no_pond$min_val[1]), "-"),
      ifelse(nrow(no_trans_stats_no_pond) > 0, sprintf("%.2f", no_trans_stats_no_pond$max_val[1]), "-")
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # Filas adicionales para % NA y n
  na_n_table <- data.frame(
    Categoria = c("% NA", "n"),
    `Trans - Ponderado` = c(
      ifelse(nrow(trans_na_n_pond) > 0, sprintf("%.2f", trans_na_n_pond$pct_na[1]), "-"),
      ifelse(nrow(trans_na_n_pond) > 0, sprintf("%.0f", trans_na_n_pond$n[1]), "-")
    ),
    `Trans sin ponderar` = c(
      ifelse(nrow(trans_na_n_no_pond) > 0, sprintf("%.2f", trans_na_n_no_pond$pct_na[1]), "-"),
      ifelse(nrow(trans_na_n_no_pond) > 0, sprintf("%.0f", trans_na_n_no_pond$n[1]), "-")
    ),
    `No trans ponderado` = c(
      ifelse(nrow(no_trans_na_n_pond) > 0, sprintf("%.2f", no_trans_na_n_pond$pct_na[1]), "-"),
      ifelse(nrow(no_trans_na_n_pond) > 0, sprintf("%.0f", no_trans_na_n_pond$n[1]), "-")
    ),
    `No trans sin ponderar` = c(
      ifelse(nrow(no_trans_na_n_no_pond) > 0, sprintf("%.2f", no_trans_na_n_no_pond$pct_na[1]), "-"),
      ifelse(nrow(no_trans_na_n_no_pond) > 0, sprintf("%.0f", no_trans_na_n_no_pond$n[1]), "-")
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # Combinamos todo
  tabla_var <- rbind(freq_table, stats_table, na_n_table)
  tabla_var$Variable <- var_name
  
  return(tabla_var)
}

# ANÁLISIS PARA trans1 Y trans2 ------

# Función para procesar todas las variables de una categoría
procesar_categoria <- function(design_obj, vars_list, tipo_var, grupo_var, grupo_nombre, filtros_vars = NULL) {
  resultados <- list()
  
  for (categoria_nombre in names(vars_list)) {
    cat("\n--- Procesando categoría:", categoria_nombre, "para", grupo_nombre, "---\n")
    vars_categoria <- vars_list[[categoria_nombre]]
    
    for (var in vars_categoria) {
      cat("Procesando", var, "...\n")
      
      # Obtenemos el filtro específico para esta variable si existe
      filtro_var <- NULL
      if (!is.null(filtros_vars) && var %in% names(filtros_vars)) {
        filtro_var <- filtros_vars[[var]]
      }
      
      if (tipo_var == "binaria") {
        stats_result <- calcular_estadisticas_binarias_por_grupo(design_obj, var, grupo_var, filtro_var)
        tabla_var <- construir_tabla_binaria(stats_result, var)
      } else {
        stats_result <- calcular_estadisticas_numericas_por_grupo(design_obj, var, grupo_var, filtro_var)
        tabla_var <- construir_tabla_numerica(stats_result, var)
      }
      
      resultados[[var]] <- tabla_var
    }
  }
  
  return(resultados)
}

# Preparar diseños para trans1 y trans2
design_trans1 <- design_completo |>
  mutate(trans_group = trans1_group)

design_trans2 <- design_completo |>
  mutate(trans_group = trans2_group)

# Definimos filtros específicos para cada variable
# agr -> solo trans==1
filtros_agr <- setNames(
  lapply(paste0("agr", 1:7), function(x) quo(trans1 == 1)),
  paste0("agr", 1:7)
)

# n_agr -> solo trans==1 y agr==1 (cada n_agr corresponde a su agr respectivo)
filtros_n_agr <- list()
for (i in 1:7) {
  filtros_n_agr[[paste0("n_agr", i)]] <- quo(trans1 == 1 & !!sym(paste0("agr", i)) == 1)
}

# n_acoso -> acoso==1 (cada n_acoso corresponde a su acoso respectivo)
filtros_n_acoso <- list()
for (i in 1:4) {
  filtros_n_acoso[[paste0("n_acoso", i)]] <- quo(!!sym(paste0("acoso", i)) == 1)
}

# Combinamos todos los filtros para variables binarias y numéricas
filtros_binarias_trans1 <- filtros_agr
filtros_numericas_trans1 <- c(filtros_n_agr, filtros_n_acoso)

filtros_binarias_trans2 <- setNames(
  lapply(paste0("agr", 1:7), function(x) quo(trans2 == 1)),
  paste0("agr", 1:7)
)
filtros_n_agr_trans2 <- list()
for (i in 1:7) {
  filtros_n_agr_trans2[[paste0("n_agr", i)]] <- quo(trans2 == 1 & !!sym(paste0("agr", i)) == 1)
}
filtros_numericas_trans2 <- c(filtros_n_agr_trans2, filtros_n_acoso)

# Procesar variables binarias para trans1
resultados_binarias_trans1 <- procesar_categoria(design_trans1, vars_binarias, "binaria", "trans_group", "trans1", filtros_binarias_trans1)

# Procesar variables numéricas para trans1
resultados_numericas_trans1 <- procesar_categoria(design_trans1, vars_numericas, "numerica", "trans_group", "trans1", filtros_numericas_trans1)

# Procesar variables binarias para trans2
resultados_binarias_trans2 <- procesar_categoria(design_trans2, vars_binarias, "binaria", "trans_group", "trans2", filtros_binarias_trans2)

# Procesar variables numéricas para trans2
resultados_numericas_trans2 <- procesar_categoria(design_trans2, vars_numericas, "numerica", "trans_group", "trans2", filtros_numericas_trans2)

# Procesar variables de lugar del acoso (l_acoso) - solo para quienes reportaron acoso
filtros_l_acoso <- list()
for (i in 1:4) {
  filtros_l_acoso[[paste0("l_acoso", i)]] <- quo(!!sym(paste0("acoso", i)) == 1)
}

# Procesar l_acoso para trans1
resultados_l_acoso_trans1 <- list()
for (var in vars_lugar_acoso) {
  cat("Procesando", var, "para trans1...\n")
  filtro_var <- filtros_l_acoso[[var]]
  stats_result <- calcular_estadisticas_numericas_por_grupo(design_trans1, var, "trans_group", filtro_var)
  tabla_var <- construir_tabla_numerica(stats_result, var)
  resultados_l_acoso_trans1[[var]] <- tabla_var
}

# Procesar l_acoso para trans2
resultados_l_acoso_trans2 <- list()
for (var in vars_lugar_acoso) {
  cat("Procesando", var, "para trans2...\n")
  filtro_var <- filtros_l_acoso[[var]]
  stats_result <- calcular_estadisticas_numericas_por_grupo(design_trans2, var, "trans_group", filtro_var)
  tabla_var <- construir_tabla_numerica(stats_result, var)
  resultados_l_acoso_trans2[[var]] <- tabla_var
}

# Combinar resultados por categoría y grupo
tablas_finales <- list()

# Para trans1
for (categoria_nombre in names(vars_binarias)) {
  vars_cat <- vars_binarias[[categoria_nombre]]
  tablas_cat <- do.call(rbind, resultados_binarias_trans1[vars_cat])
  tablas_finales[[paste0("trans1_", categoria_nombre, "_binarias")]] <- tablas_cat |>
    dplyr::select(Variable, Categoria, `Trans - Ponderado`, `Trans sin ponderar`, 
           `No trans ponderado`, `No trans sin ponderar`) |>
    mutate_all(~ifelse(is.na(.), "-", .))
}

for (categoria_nombre in names(vars_numericas)) {
  vars_cat <- vars_numericas[[categoria_nombre]]
  tablas_cat <- do.call(rbind, resultados_numericas_trans1[vars_cat])
  tablas_finales[[paste0("trans1_", categoria_nombre, "_numericas")]] <- tablas_cat |>
    dplyr::select(Variable, Categoria, `Trans - Ponderado`, `Trans sin ponderar`, 
           `No trans ponderado`, `No trans sin ponderar`) |>
    mutate_all(~ifelse(is.na(.), "-", .))
}

# Para trans2
for (categoria_nombre in names(vars_binarias)) {
  vars_cat <- vars_binarias[[categoria_nombre]]
  tablas_cat <- do.call(rbind, resultados_binarias_trans2[vars_cat])
  tablas_finales[[paste0("trans2_", categoria_nombre, "_binarias")]] <- tablas_cat |>
    dplyr::select(Variable, Categoria, `Trans - Ponderado`, `Trans sin ponderar`, 
           `No trans ponderado`, `No trans sin ponderar`) |>
    mutate_all(~ifelse(is.na(.), "-", .))
}

for (categoria_nombre in names(vars_numericas)) {
  vars_cat <- vars_numericas[[categoria_nombre]]
  tablas_cat <- do.call(rbind, resultados_numericas_trans2[vars_cat])
  tablas_finales[[paste0("trans2_", categoria_nombre, "_numericas")]] <- tablas_cat |>
    dplyr::select(Variable, Categoria, `Trans - Ponderado`, `Trans sin ponderar`, 
           `No trans ponderado`, `No trans sin ponderar`) |>
    mutate_all(~ifelse(is.na(.), "-", .))
}

# Agregar tablas de lugar del acoso
tablas_cat_l_acoso_trans1 <- do.call(rbind, resultados_l_acoso_trans1)
tablas_finales[["trans1_Lugar_acoso"]] <- tablas_cat_l_acoso_trans1 |>
  dplyr::select(Variable, Categoria, `Trans - Ponderado`, `Trans sin ponderar`, 
         `No trans ponderado`, `No trans sin ponderar`) |>
  mutate_all(~ifelse(is.na(.), "-", .))

tablas_cat_l_acoso_trans2 <- do.call(rbind, resultados_l_acoso_trans2)
tablas_finales[["trans2_Lugar_acoso"]] <- tablas_cat_l_acoso_trans2 |>
  dplyr::select(Variable, Categoria, `Trans - Ponderado`, `Trans sin ponderar`, 
         `No trans ponderado`, `No trans sin ponderar`) |>
  mutate_all(~ifelse(is.na(.), "-", .))

# Guardar resultados en Excel
rio::export(
  tablas_finales,
  "Output/T3_Agresiones.xlsx"
)

