# 2.0 Análisis Descriptivos -----------------------------------------

# Cargamos configuraciones previas 
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")
source("Code/0.3 Funciones.R")

# Cargamos librería para diseño muestral complejo
install_load(c("survey", "srvyr"))

# Abrimos los datos ------
data <- rio::import("Output/ENSSEX_TRANS.RData")
glimpse(data); names(data)

# Verificamos estructura de variables sm1-sm4
table(data$sm1, useNA = "always")
table(data$sm2, useNA = "always")
table(data$sm3, useNA = "always")
table(data$sm4, useNA = "always")
table(data$trans1, useNA = "always")
table(data$trans2, useNA = "always")

# Preparamos datos: creamos variables trans1 y trans2 como factores
data_clean <- data |>
  mutate(
    # Aseguramos que sm1-sm4 sean numéricas
    sm1 = as.numeric(sm1),
    sm2 = as.numeric(sm2),
    sm3 = as.numeric(sm3),
    sm4 = as.numeric(sm4),
    # Creamos variables de grupo para trans1 y trans2
    trans1_group = factor(if_else(trans1 == 1, "Trans", "No trans", missing = "No trans")),
    trans2_group = factor(if_else(trans2 == 1, "Trans", "No trans", missing = "No trans"))
  )

# Etiquetas para las categorías de salud mental
sm_labels <- c("1" = "Nunca", 
               "2" = "Algunos días", 
               "3" = "Más de la mitad de los días", 
               "4" = "Casi todos los días")

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

# Función para calcular estadísticas por grupo usando group_by
calcular_estadisticas_por_grupo <- function(design_obj, var_name, grupo_var) {
  
  # Creamos variables indicadoras para cada categoría
  design_obj <- design_obj |>
    mutate(
      cat_1 = as.numeric(.data[[var_name]] == 1),
      cat_2 = as.numeric(.data[[var_name]] == 2),
      cat_3 = as.numeric(.data[[var_name]] == 3),
      cat_4 = as.numeric(.data[[var_name]] == 4),
      has_value = as.numeric(!is.na(.data[[var_name]]))
    )
  
  # Frecuencias ponderadas por grupo
  freq_pond <- design_obj |>
    group_by(across(all_of(grupo_var))) |>
    summarize(
      freq_1 = survey_total(cat_1, na.rm = TRUE, vartype = NULL),
      freq_2 = survey_total(cat_2, na.rm = TRUE, vartype = NULL),
      freq_3 = survey_total(cat_3, na.rm = TRUE, vartype = NULL),
      freq_4 = survey_total(cat_4, na.rm = TRUE, vartype = NULL),
      total = survey_total(has_value, na.rm = TRUE, vartype = NULL)
    ) |>
    mutate(
      pct_1 = round(freq_1 / total * 100, 2),
      pct_2 = round(freq_2 / total * 100, 2),
      pct_3 = round(freq_3 / total * 100, 2),
      pct_4 = round(freq_4 / total * 100, 2)
    ) |>
    select(!!sym(grupo_var), freq_1, pct_1, freq_2, pct_2, freq_3, pct_3, freq_4, pct_4, total) |>
    as.data.frame()
  
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
      freq_1 = sum(.data[[var_name]] == 1, na.rm = TRUE),
      freq_2 = sum(.data[[var_name]] == 2, na.rm = TRUE),
      freq_3 = sum(.data[[var_name]] == 3, na.rm = TRUE),
      freq_4 = sum(.data[[var_name]] == 4, na.rm = TRUE),
      total = sum(!is.na(.data[[var_name]]))
    ) |>
    mutate(
      pct_1 = round(freq_1 / total * 100, 2),
      pct_2 = round(freq_2 / total * 100, 2),
      pct_3 = round(freq_3 / total * 100, 2),
      pct_4 = round(freq_4 / total * 100, 2)
    ) |>
    as.data.frame()
  
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
  
  # % NA y n por grupo (ponderado)
  na_n_pond <- design_obj |>
    group_by(across(all_of(grupo_var))) |>
    summarize(
      pct_na = survey_mean(is.na(.data[[var_name]]), na.rm = TRUE, vartype = NULL) * 100,
      n = survey_total(na.rm = TRUE, vartype = NULL)
    ) |>
    as.data.frame()
  
  # % NA y n por grupo (sin ponderar)
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
    na_n_no_pond = na_n_no_pond
  ))
}

# Función para construir la tabla final para una variable
construir_tabla_variable <- function(stats_result, var_name) {
  
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
  
  # Construimos tabla de frecuencias
  freq_table <- data.frame(
    Categoria = c("Nunca", "Algunos días", "Más de la mitad de los días", "Casi todos los días"),
    `Trans - Ponderado` = c(
      ifelse(nrow(trans_pond) > 0, paste0(sprintf("%.2f", trans_pond$pct_1[1]), " (", round(trans_pond$freq_1[1]), ")"), "-"),
      ifelse(nrow(trans_pond) > 0, paste0(sprintf("%.2f", trans_pond$pct_2[1]), " (", round(trans_pond$freq_2[1]), ")"), "-"),
      ifelse(nrow(trans_pond) > 0, paste0(sprintf("%.2f", trans_pond$pct_3[1]), " (", round(trans_pond$freq_3[1]), ")"), "-"),
      ifelse(nrow(trans_pond) > 0, paste0(sprintf("%.2f", trans_pond$pct_4[1]), " (", round(trans_pond$freq_4[1]), ")"), "-")
    ),
    `Trans sin ponderar` = c(
      ifelse(nrow(trans_no_pond) > 0, paste0(sprintf("%.2f", trans_no_pond$pct_1[1]), " (", trans_no_pond$freq_1[1], ")"), "-"),
      ifelse(nrow(trans_no_pond) > 0, paste0(sprintf("%.2f", trans_no_pond$pct_2[1]), " (", trans_no_pond$freq_2[1], ")"), "-"),
      ifelse(nrow(trans_no_pond) > 0, paste0(sprintf("%.2f", trans_no_pond$pct_3[1]), " (", trans_no_pond$freq_3[1], ")"), "-"),
      ifelse(nrow(trans_no_pond) > 0, paste0(sprintf("%.2f", trans_no_pond$pct_4[1]), " (", trans_no_pond$freq_4[1], ")"), "-")
    ),
    `No trans ponderado` = c(
      ifelse(nrow(no_trans_pond) > 0, paste0(sprintf("%.2f", no_trans_pond$pct_1[1]), " (", round(no_trans_pond$freq_1[1]), ")"), "-"),
      ifelse(nrow(no_trans_pond) > 0, paste0(sprintf("%.2f", no_trans_pond$pct_2[1]), " (", round(no_trans_pond$freq_2[1]), ")"), "-"),
      ifelse(nrow(no_trans_pond) > 0, paste0(sprintf("%.2f", no_trans_pond$pct_3[1]), " (", round(no_trans_pond$freq_3[1]), ")"), "-"),
      ifelse(nrow(no_trans_pond) > 0, paste0(sprintf("%.2f", no_trans_pond$pct_4[1]), " (", round(no_trans_pond$freq_4[1]), ")"), "-")
    ),
    `No trans sin ponderar` = c(
      ifelse(nrow(no_trans_no_pond) > 0, paste0(sprintf("%.2f", no_trans_no_pond$pct_1[1]), " (", no_trans_no_pond$freq_1[1], ")"), "-"),
      ifelse(nrow(no_trans_no_pond) > 0, paste0(sprintf("%.2f", no_trans_no_pond$pct_2[1]), " (", no_trans_no_pond$freq_2[1], ")"), "-"),
      ifelse(nrow(no_trans_no_pond) > 0, paste0(sprintf("%.2f", no_trans_no_pond$pct_3[1]), " (", no_trans_no_pond$freq_3[1], ")"), "-"),
      ifelse(nrow(no_trans_no_pond) > 0, paste0(sprintf("%.2f", no_trans_no_pond$pct_4[1]), " (", no_trans_no_pond$freq_4[1], ")"), "-")
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
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

# ANÁLISIS PARA trans1 ------

# Actualizamos el diseño con la variable de grupo trans1
design_trans1 <- design_completo |>
  mutate(trans_group = trans1_group)

# Inicializamos lista para almacenar resultados
resultados_trans1 <- list()

# Calculamos estadísticas para cada variable sm1-sm4
for (var in c("sm1", "sm2", "sm3", "sm4")) {
  cat("\nProcesando", var, "para trans1...\n")
  
  stats_result <- calcular_estadisticas_por_grupo(design_trans1, var, "trans_group")
  tabla_var <- construir_tabla_variable(stats_result, var)
  
  resultados_trans1[[var]] <- tabla_var
}

# Combinamos todas las tablas para trans1
tabla1_trans1 <- do.call(rbind, resultados_trans1)

# Reordenamos columnas
tabla1_trans1 <- tabla1_trans1 |>
  dplyr::select(Variable, Categoria, `Trans - Ponderado`, `Trans sin ponderar`, 
         `No trans ponderado`, `No trans sin ponderar`)

# Reemplazamos NAs con "-"
tabla1_trans1[is.na(tabla1_trans1)] <- "-"
tabla1_trans1

# ANÁLISIS PARA trans1 ------

# Actualizamos el diseño con la variable de grupo trans2
design_trans2 <- design_completo |>
  mutate(trans_group = trans2_group)

# Inicializamos lista para almacenar resultados
resultados_trans2 <- list()

# Calculamos estadísticas para cada variable sm1-sm4
for (var in c("sm1", "sm2", "sm3", "sm4")) {
  cat("\nProcesando", var, "para trans2...\n")
  
  stats_result <- calcular_estadisticas_por_grupo(design_trans2, var, "trans_group")
  tabla_var <- construir_tabla_variable(stats_result, var)
  
  resultados_trans2[[var]] <- tabla_var
}

# Combinamos todas las tablas para trans2
tabla1_trans2 <- do.call(rbind, resultados_trans2)

# Reordenamos columnas
tabla1_trans2 <- tabla1_trans2 |>
  dplyr::select(Variable, Categoria, `Trans - Ponderado`, `Trans sin ponderar`, 
         `No trans ponderado`, `No trans sin ponderar`)

# Reemplazamos NAs con "-"
tabla1_trans2[is.na(tabla1_trans2)] <- "-"

tabla1_trans2

# Guardamos resultados (listas en hojas de excel) -----
rio::export(
  list(
    "trans1" = tabla1_trans1,
    "trans2" = tabla1_trans2
  ),
  "Output/T1_SaludMental.xlsx"
)
