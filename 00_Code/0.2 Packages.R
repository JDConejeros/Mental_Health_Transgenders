# Packages ---- 

# Function install/load packages
install_load <- function(packages){
  for (i in packages) {
    if (i %in% rownames(installed.packages())) {
      library(i, character.only=TRUE)
    } else {
      install.packages(i)
      library(i, character.only = TRUE)
    }
  }
}

# Apply function
install_load(c("rio", 
               "data.table",
               "tidyverse",
               "future",
               "furrr",
               "ragg",
               "writexl",
               "survival",
               "tictoc",
               "knitr",
               "tictoc",
               "future",
               "future.apply",
               "survey", 
               "srvyr",
               "ggstatsplot",
               "ggpubr",
               "patchwork",
               "statsExpressions",
               "car",
               "lmtest",
               "sandwich",
               "broom", 
               "ordinal",
               "MASS",
               "ggeffects",
               "effects",
               "psych",
               "survey",
               "psych"
               ))
