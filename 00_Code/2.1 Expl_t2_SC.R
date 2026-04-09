# 2.1 Análisis Descriptivos - Variables de Suicidio (Binarias) --------

# Cargamos configuraciones previas 
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")
source("Code/0.3 Funciones.R")

# Cargamos librería para diseño muestral complejo
install_load(c("survey", "srvyr"))

# Abrimos los datos ------
data <- rio::import("Output/ENSSEX_TRANS.RData")
glimpse(data); names(data)

# Verificamos estructura de variables de suicidio
table(data$pen_suc, useNA = "always")
table(data$pen_suc12m, useNA = "always")
table(data$plan_suc, useNA = "always")
table(data$plan_suc12m, useNA = "always")
table(data$trans1, useNA = "always")
table(data$trans2, useNA = "always")

# Preparamos datos: creamos variables trans1 y trans2 como factores
data_clean <- data |>
  mutate(
    # Aseguramos que las variables de suicidio sean numéricas
    pen_suc = as.numeric(pen_suc),
    pen_suc12m = as.numeric(pen_suc12m),
    plan_suc = as.numeric(plan_suc),
    plan_suc12m = as.numeric(plan_suc12m),
    # Creamos variables de grupo para trans1 y trans2
    trans1_group = factor(if_else(trans1 == 1, "Trans", "No trans", missing = "No trans")),
    trans2_group = factor(if_else(trans2 == 1, "Trans", "No trans", missing = "No trans"))
  )

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

# Función para calcular estadísticas de variables binarias por grupo
calcular_estadisticas_binarias_por_grupo <- function(design_obj, var_name, grupo_var) {
  
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

# ANÁLISIS PARA trans1 -----

# Actualizamos el diseño con la variable de grupo trans1
design_trans1 <- design_completo |>
  mutate(trans_group = trans1_group)

# Inicializamos lista para almacenar resultados
resultados_trans1 <- list()

# Calculamos estadísticas para cada variable de suicidio
for (var in c("pen_suc", "pen_suc12m", "plan_suc", "plan_suc12m")) {
  cat("\nProcesando", var, "para trans1...\n")
  
  stats_result <- calcular_estadisticas_binarias_por_grupo(design_trans1, var, "trans_group")
  tabla_var <- construir_tabla_binaria(stats_result, var)
  
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

# ANÁLISIS PARA trans2 -------

# Actualizamos el diseño con la variable de grupo trans2
design_trans2 <- design_completo |>
  mutate(trans_group = trans2_group)

# Inicializamos lista para almacenar resultados
resultados_trans2 <- list()

# Calculamos estadísticas para cada variable de suicidio
for (var in c("pen_suc", "pen_suc12m", "plan_suc", "plan_suc12m")) {
  cat("\nProcesando", var, "para trans2...\n")
  
  stats_result <- calcular_estadisticas_binarias_por_grupo(design_trans2, var, "trans_group")
  tabla_var <- construir_tabla_binaria(stats_result, var)
  
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

# GUARDAMOS RESULTADOS ----
# Guardamos resultados en Excel (listas en hojas separadas)
rio::export(
  list(
    "trans1" = tabla1_trans1,
    "trans2" = tabla1_trans2
  ),
  "Output/T2_Suicidio.xlsx"
)

