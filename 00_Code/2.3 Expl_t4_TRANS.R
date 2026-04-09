# 2.3 Análisis Descriptivos - Variables de Transición de Género --------

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
table(data$edad_genero_trans, useNA = "always")
table(data$transicion_actual, useNA = "always")
table(data$tiempo_transicion, useNA = "always")
table(data$trans_social, useNA = "always")
table(data$trans_hormo, useNA = "always")
table(data$trans_otro, useNA = "always")
table(data$cambio_nom, useNA = "always")
table(data$lugar_horm, useNA = "always")
table(data$intervenciones, useNA = "always")
table(data$disforia, useNA = "always")
table(data$trat, useNA = "always")
table(data$trans1, useNA = "always")
table(data$trans2, useNA = "always")

# Preparamos datos: creamos variables trans1 y trans2 como factores
data_clean <- data |>
  mutate(
    # Variables numéricas de edad y tiempo
    edad_genero_trans = as.numeric(edad_genero_trans),
    tiempo_transicion = as.numeric(tiempo_transicion),
    # Variables binarias/categóricas de transición
    transicion_actual = as.numeric(transicion_actual),
    trans_social = as.numeric(trans_social),
    trans_hormo = as.numeric(trans_hormo),
    trans_otro = as.numeric(trans_otro),
    cambio_nom = as.numeric(cambio_nom),
    lugar_horm = as.numeric(lugar_horm),
    intervenciones = as.numeric(intervenciones),
    disforia = as.numeric(disforia),
    trat = as.numeric(trat),
    # Creamos variables de grupo para trans1 y trans2
    trans1_group = factor(if_else(trans1 == 1, "Trans", "No trans", missing = "No trans")),
    trans2_group = factor(if_else(trans2 == 1, "Trans", "No trans", missing = "No trans"))
  )

# Clasificamos variables según su tipo (se determinará dinámicamente)
# Variables numéricas continuas
vars_numericas_continuas <- c("edad_genero_trans", "tiempo_transicion")

# Variables que necesitamos clasificar dinámicamente
vars_a_clasificar <- c("transicion_actual", "trans_social", "trans_hormo", "trans_otro", 
                       "cambio_nom", "lugar_horm", "intervenciones", "disforia", "trat")

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

# Función para determinar si una variable es binaria (solo 0 y 1)
es_binaria <- function(var_data) {
  valores_unicos <- sort(unique(var_data[!is.na(var_data)]))
  return(length(valores_unicos) <= 2 && all(valores_unicos %in% c(0, 1)))
}

# Clasificamos variables dinámicamente
vars_binarias <- c()
vars_categoricas <- c()

for (var in vars_a_clasificar) {
  var_data <- data_clean[[var]]
  if (es_binaria(var_data)) {
    vars_binarias <- c(vars_binarias, var)
  } else {
    vars_categoricas <- c(vars_categoricas, var)
  }
}

cat("\nVariables binarias detectadas:", paste(vars_binarias, collapse = ", "), "\n")
cat("Variables categóricas detectadas:", paste(vars_categoricas, collapse = ", "), "\n")

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

# FUNCIONES PARA VARIABLES NUMÉRICAS CONTINUAS -----

# Función para calcular estadísticas numéricas continuas por grupo
calcular_estadisticas_numericas_continuas_por_grupo <- function(design_obj, var_name, grupo_var, filtro_adicional = NULL) {
  
  # Aplicamos filtro adicional si existe
  if (!is.null(filtro_adicional)) {
    design_obj <- design_obj |>
      filter(!!filtro_adicional)
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
    stats_pond = stats_pond,
    stats_no_pond = stats_no_pond,
    na_n_pond = na_n_pond,
    na_n_no_pond = na_n_no_pond
  ))
}

# Función para construir la tabla final para una variable numérica continua
construir_tabla_numerica_continua <- function(stats_result, var_name) {
  
  # Extraemos datos por grupo
  trans_stats_pond <- stats_result$stats_pond |> filter(trans_group == "Trans")
  no_trans_stats_pond <- stats_result$stats_pond |> filter(trans_group == "No trans")
  trans_stats_no_pond <- stats_result$stats_no_pond |> filter(trans_group == "Trans")
  no_trans_stats_no_pond <- stats_result$stats_no_pond |> filter(trans_group == "No trans")
  
  trans_na_n_pond <- stats_result$na_n_pond |> filter(trans_group == "Trans")
  no_trans_na_n_pond <- stats_result$na_n_pond |> filter(trans_group == "No trans")
  trans_na_n_no_pond <- stats_result$na_n_no_pond |> filter(trans_group == "Trans")
  no_trans_na_n_no_pond <- stats_result$na_n_no_pond |> filter(trans_group == "No trans")
  
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
  tabla_var <- rbind(stats_table, na_n_table)
  tabla_var$Variable <- var_name
  
  return(tabla_var)
}

# FUNCIONES PARA VARIABLES CATEGÓRICAS -----

# Función para calcular estadísticas categóricas por grupo (misma que numéricas pero sin estadísticas continuas)
calcular_estadisticas_categoricas_por_grupo <- function(design_obj, var_name, grupo_var, filtro_adicional = NULL) {
  
  # Aplicamos filtro adicional si existe
  if (!is.null(filtro_adicional)) {
    design_obj <- design_obj |>
      filter(!!filtro_adicional)
  }
  
  # Obtenemos valores únicos de la variable (excluyendo NA)
  valores_unicos <- sort(unique(design_obj$variables[[var_name]][!is.na(design_obj$variables[[var_name]])]))
  
  # Construimos summarize usando svytable por grupo
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
  
  # Frecuencias sin ponderar por grupo
  freq_no_pond <- design_obj$variables |>
    group_by(across(all_of(grupo_var))) |>
    summarize(
      total = sum(!is.na(.data[[var_name]]))
    ) |>
    as.data.frame()
  
  # Calculamos frecuencias por grupo y por valor
  for (val in valores_unicos) {
    freq_por_grupo <- design_obj$variables |>
      group_by(across(all_of(grupo_var))) |>
      summarize(
        freq_val = sum(.data[[var_name]] == val, na.rm = TRUE)
      ) |>
      as.data.frame()
    
    # Agregamos las frecuencias al dataframe
    freq_no_pond <- freq_no_pond |>
      left_join(freq_por_grupo, by = grupo_var)
    
    # Renombramos la columna y calculamos porcentaje
    colnames(freq_no_pond)[ncol(freq_no_pond)] <- paste0("freq_", val)
    freq_no_pond[[paste0("pct_", val)]] <- round(freq_no_pond[[paste0("freq_", val)]] / freq_no_pond$total * 100, 2)
  }
  
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
    na_n_no_pond = na_n_no_pond,
    valores_unicos = valores_unicos
  ))
}

# Función para construir la tabla final para una variable categórica
construir_tabla_categorica <- function(stats_result, var_name) {
  
  valores_unicos <- stats_result$valores_unicos
  
  # Extraemos datos por grupo
  trans_pond <- stats_result$freq_pond |> filter(trans_group == "Trans")
  no_trans_pond <- stats_result$freq_pond |> filter(trans_group == "No trans")
  trans_no_pond <- stats_result$freq_no_pond |> filter(trans_group == "Trans")
  no_trans_no_pond <- stats_result$freq_no_pond |> filter(trans_group == "No trans")
  
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
  tabla_var <- rbind(freq_table, na_n_table)
  tabla_var$Variable <- var_name
  
  return(tabla_var)
}

# ANÁLISIS PARA trans1 Y trans2 ------

# Preparar diseños para trans1 y trans2
design_trans1 <- design_completo |>
  mutate(trans_group = trans1_group)

design_trans2 <- design_completo |>
  mutate(trans_group = trans2_group)

# Definimos filtros específicos para cada variable según los comentarios del código
# Todas estas variables son para trans1==1 (personas trans)
filtros_trans1 <- list()
filtros_trans2 <- list()

# Variables básicas: solo trans==1
vars_basicas <- c("edad_genero_trans", "transicion_actual", "tiempo_transicion", "trat")
for (var in vars_basicas) {
  filtros_trans1[[var]] <- quo(trans1 == 1)
  filtros_trans2[[var]] <- quo(trans2 == 1)
}

# Variables con filtro adicional: trans==1 Y transicion_actual==1
vars_transicion <- c("trans_social", "trans_hormo", "trans_otro", "cambio_nom", "disforia")
for (var in vars_transicion) {
  filtros_trans1[[var]] <- quo(trans1 == 1 & transicion_actual == 1)
  filtros_trans2[[var]] <- quo(trans2 == 1 & transicion_actual == 1)
}

# Variables con filtro adicional: trans==1 Y transicion_actual==1 Y trans_hormo==1
vars_hormonas <- c("lugar_horm", "intervenciones")
for (var in vars_hormonas) {
  filtros_trans1[[var]] <- quo(trans1 == 1 & transicion_actual == 1 & trans_hormo == 1)
  filtros_trans2[[var]] <- quo(trans2 == 1 & transicion_actual == 1 & trans_hormo == 1)
}

# Función para procesar todas las variables
procesar_variables_transicion <- function(design_obj, vars_list, tipo_var, grupo_var, grupo_nombre, filtros_vars) {
  resultados <- list()
  
  for (var in vars_list) {
    cat("Procesando", var, "para", grupo_nombre, "...\n")
    
    # Obtenemos el filtro específico para esta variable
    filtro_var <- filtros_vars[[var]]
    
    if (tipo_var == "binaria") {
      stats_result <- calcular_estadisticas_binarias_por_grupo(design_obj, var, grupo_var, filtro_var)
      tabla_var <- construir_tabla_binaria(stats_result, var)
    } else if (tipo_var == "numerica_continua") {
      stats_result <- calcular_estadisticas_numericas_continuas_por_grupo(design_obj, var, grupo_var, filtro_var)
      tabla_var <- construir_tabla_numerica_continua(stats_result, var)
    } else if (tipo_var == "categorica") {
      stats_result <- calcular_estadisticas_categoricas_por_grupo(design_obj, var, grupo_var, filtro_var)
      tabla_var <- construir_tabla_categorica(stats_result, var)
    }
    
    resultados[[var]] <- tabla_var
  }
  
  return(resultados)
}

# Procesar variables numéricas continuas para trans1
resultados_numericas_trans1 <- procesar_variables_transicion(
  design_trans1, vars_numericas_continuas, "numerica_continua", "trans_group", "trans1", filtros_trans1
)

# Procesar variables binarias para trans1
resultados_binarias_trans1 <- procesar_variables_transicion(
  design_trans1, vars_binarias, "binaria", "trans_group", "trans1", filtros_trans1
)

# Procesar variables categóricas para trans1
resultados_categoricas_trans1 <- procesar_variables_transicion(
  design_trans1, vars_categoricas, "categorica", "trans_group", "trans1", filtros_trans1
)

# Procesar variables numéricas continuas para trans2
resultados_numericas_trans2 <- procesar_variables_transicion(
  design_trans2, vars_numericas_continuas, "numerica_continua", "trans_group", "trans2", filtros_trans2
)

# Procesar variables binarias para trans2
resultados_binarias_trans2 <- procesar_variables_transicion(
  design_trans2, vars_binarias, "binaria", "trans_group", "trans2", filtros_trans2
)

# Procesar variables categóricas para trans2
resultados_categoricas_trans2 <- procesar_variables_transicion(
  design_trans2, vars_categoricas, "categorica", "trans_group", "trans2", filtros_trans2
)

# Combinar resultados por tipo y grupo
tablas_finales <- list()

# Para trans1
if (length(resultados_numericas_trans1) > 0) {
  tablas_cat <- do.call(rbind, resultados_numericas_trans1)
  tablas_finales[["trans1_Numericas_continuas"]] <- tablas_cat |>
    dplyr::select(Variable, Categoria, `Trans - Ponderado`, `Trans sin ponderar`, 
           `No trans ponderado`, `No trans sin ponderar`) |>
    mutate_all(~ifelse(is.na(.), "-", .))
}

if (length(resultados_binarias_trans1) > 0) {
  tablas_cat <- do.call(rbind, resultados_binarias_trans1)
  tablas_finales[["trans1_Binarias"]] <- tablas_cat |>
    dplyr::select(Variable, Categoria, `Trans - Ponderado`, `Trans sin ponderar`, 
           `No trans ponderado`, `No trans sin ponderar`) |>
    mutate_all(~ifelse(is.na(.), "-", .))
}

if (length(resultados_categoricas_trans1) > 0) {
  tablas_cat <- do.call(rbind, resultados_categoricas_trans1)
  tablas_finales[["trans1_Categoricas"]] <- tablas_cat |>
    dplyr::select(Variable, Categoria, `Trans - Ponderado`, `Trans sin ponderar`, 
           `No trans ponderado`, `No trans sin ponderar`) |>
    mutate_all(~ifelse(is.na(.), "-", .))
}

# Para trans2
if (length(resultados_numericas_trans2) > 0) {
  tablas_cat <- do.call(rbind, resultados_numericas_trans2)
  tablas_finales[["trans2_Numericas_continuas"]] <- tablas_cat |>
    dplyr::select(Variable, Categoria, `Trans - Ponderado`, `Trans sin ponderar`, 
           `No trans ponderado`, `No trans sin ponderar`) |>
    mutate_all(~ifelse(is.na(.), "-", .))
}

if (length(resultados_binarias_trans2) > 0) {
  tablas_cat <- do.call(rbind, resultados_binarias_trans2)
  tablas_finales[["trans2_Binarias"]] <- tablas_cat |>
    dplyr::select(Variable, Categoria, `Trans - Ponderado`, `Trans sin ponderar`, 
           `No trans ponderado`, `No trans sin ponderar`) |>
    mutate_all(~ifelse(is.na(.), "-", .))
}

if (length(resultados_categoricas_trans2) > 0) {
  tablas_cat <- do.call(rbind, resultados_categoricas_trans2)
  tablas_finales[["trans2_Categoricas"]] <- tablas_cat |>
    dplyr::select(Variable, Categoria, `Trans - Ponderado`, `Trans sin ponderar`, 
           `No trans ponderado`, `No trans sin ponderar`) |>
    mutate_all(~ifelse(is.na(.), "-", .))
}

# Guardar resultados en Excel
rio::export(
  tablas_finales,
  "Output/T4_Transicion.xlsx"
)

