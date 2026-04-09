# Mental Health in Transgender Population - ENSSEX 2022/2023

## Descripcion del analisis

Este repositorio contiene el flujo de analisis para estudiar indicadores de salud mental en poblacion trans y compararlos con la poblacion general, usando la base ENSSEX 2022/2023.

El proyecto integra preparacion de datos, analisis descriptivo y modelos de regresion para los puntajes `GAD-2`, `PHQ-2` y `PHQ-4`, incorporando covariables sociodemograficas y ponderadores muestrales.

## Metodologia

- **Fuente de datos**: base ENSSEX 2022/2023 (`.dta`), importada y transformada en R.
- **Preparacion de datos**: limpieza, recodificacion y construccion de variables derivadas en `00_Code/1.0 Process_data.R`.
- **Construccion de desenlaces**: calculo de sintomas de ansiedad/depresion (`sm1-sm4`), puntajes `PHQ-2`, `GAD-2`, `PHQ-4` y banderas de tamizaje.
- **Submuestras**:
  - Poblacion general (`ENSSEX_trans_population.RData`).
  - Submuestra trans (`ENSSEX_trans_only.RData`).
- **Analisis descriptivo**: estadistica descriptiva no ponderada y ponderada, tablas comparativas y figuras en `00_Code/3.0 Descriptive_analysis.R`.
- **Modelamiento**:
  - Regresiones lineales para `GAD-2`, `PHQ-2`, `PHQ-4` en `00_Code/4.0 Lineal_regression.R`.
  - Modelos crudos y estandarizados, ponderados y no ponderados.
  - Errores robustos HC3, pruebas de supuestos y diagnosticos.

## Estructura del proyecto

```text
Mental_Health_Transgenders/
├── 00_Code/
│   ├── 0.1 Settings.R
│   ├── 0.2 Packages.R
│   ├── 1.0 Process_data.R
│   ├── 2.0 Expl_t1_MH.R
│   ├── 2.1 Expl_t2_SC.R
│   ├── 2.2 Expl_t3_AGR.R
│   ├── 2.3 Expl_t4_TRANS.R
│   ├── 3.0 Descriptive_analysis.R
│   ├── 3.1 Correlations_analysis.R
│   ├── 4.0 Lineal_regression.R
│   └── 4.1 Logistic_ordinal_regression.R
├── 01_Data/                  # Insumos (no versionados en este repo)
├── 02_Output/                # Resultados, tablas y figuras generadas
├── README.md
└── LICENSE
```

## Requisitos

- R (version compatible con `tidyverse`, `survey`, `srvyr`, `writexl`, `sandwich`, `lmtest`, `ggplot2`, entre otros paquetes usados en `00_Code/0.2 Packages.R`).
- Archivos de datos ENSSEX disponibles en la ruta esperada por los scripts.

## Contacto

Para consultas sobre este analisis, usar el siguiente contacto: <jdconejeros@uc.cl>
