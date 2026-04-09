# 1.0 Data preparation -----------------------------------------

# Load project settings
source("00_Code/0.1 Settings.R")
source("00_Code/0.2 Packages.R")

# Load ENSSEX data
enssex <- rio::import("01_Data/BBDD_ENSSEX_2022_2023/20240516_ENSSEX_data.dta") |> as_tibble()
glimpse(enssex); names(enssex)

# Select variables used in this module
data <- enssex |>
  # Main analysis set for first-sexual-experience and trans modules
  dplyr::select("folio_encuesta", "macro_region", "region", "comuna", 
               "varstrat", # Pseudostratum
               "varunit", # Pseudocluster
               "w_personas_cal", "w_personas_cal_nr_mod_hogar", # Survey weights
               "p1", "p3", "genero_separada", "orientacion_sexual_separado",
               "edad", "fecha","fecha_nacimiento_ano", 
               "tramo_edad_4", 
               "p34", # Sexuality conversations in family
               "educ", 
               "prevision_salud",
               "pueblos",
               "p263", # Nationality
               "calidad_vida",
               "bienestar_vida_sexual", # 1-7 satisfaction with sexual life
               "i_1_p9":"i_6_p9",  # Satisfaction scales: 1-7, 9=missing
               "i_1_p11":"i_4_p11", # Mental health symptom frequency
               "i_1_p12":"i_2_p13", # Suicidal ideation/planning items
               "p138":"i_7_p150", # Trans-specific module
               "i_1_p240":"i_2_p241", # Public-space sexual violence
               "p240c":"p241c",
               "i_1_p244":"i_4_p246") |> 
  mutate(fecha_encuesta=dmy(fecha),
         agno_encuesta=year(fecha_encuesta), 
         edad_val=agno_encuesta-edad, 
         validacion_fecha_nac=if_else(edad_val==fecha_nacimiento_ano, 1, 0)) 

# Prepare analysis variables
data <- data |> 
  mutate(
    # Geographic identifiers
    macro_region = dplyr::case_when(macro_region %in% c(8, 9, 88, 99) ~ NA_real_, TRUE ~ as.numeric(macro_region)),
    macro_region=factor(macro_region, labels=c("North Macrozone",
                                               "Central Macrozone", 
                                               "South Macrozone")),
    comuna=str_to_title(comuna),
    region=factor(region, labels=c("Tarapaca", 
                                   "Antofagasta", 
                                   "Atacama", 
                                   "Coquimbo",
                                   "Valparaiso", 
                                   "O'Higgins", 
                                   "Maule", 
                                   "Biobio", 
                                   "La Araucania", 
                                   "Los Lagos", 
                                   "Aysen", 
                                   "Magallanes", 
                                   "Metropolitan", 
                                   "Los Rios", 
                                   "Arica and Parinacota", 
                                   "Nuble")),
    sex=factor(if_else(p1 %in% c(8,9,88,99), NA_real_, as.numeric(p1)), labels=c("Male", "Female")),
    gender_identity=factor(if_else(genero_separada == 7, NA_real_, as.numeric(genero_separada)),
                           labels=c("Cis man", "Transgender", "Non-binary", "Other", "Cis woman")),
    trans_from_q3 = if_else(p3 %in% c(8,9), NA_real_, if_else(p3 %in% c(3,4,5), 1, 0)),
    trans_from_gender = if_else(genero_separada == 7, NA_real_, if_else(genero_separada %in% c(2,3), 1, 0)),
    trans_from_q3 = factor(trans_from_q3, levels = c(0,1), labels = c("No", "Yes")),
    trans_from_gender = factor(trans_from_gender, levels = c(0,1), labels = c("No", "Yes")),
    sexual_orientation=factor(if_else(orientacion_sexual_separado == 9, NA_real_, as.numeric(orientacion_sexual_separado)),
                              levels = c(1,2,4,5),
                              labels=c("Heterosexual", "Homosexual/Lesbian", "Bisexual", "Other")),
    p34 = if_else(p34 %in% c(8, 9), NA_real_, as.numeric(p34)),
    sexual_talk_family = factor(p34,
                                levels = c(1,2,3),
                                labels = c("No sexual topics discussed", "Some sexual topics discussed", "All sexual topics discussed")),
    age_group_4 = factor(
      if_else(tramo_edad_4 %in% c(8, 9, 88, 99), NA_real_, as.numeric(tramo_edad_4)),
      levels = c(1, 2, 3, 4),
      labels = c("18 to 29", "30 to 44", "45 to 59", "60 and older")
    ),
    education_level = factor(if_else(educ %in% c(8,9,88,99), NA_real_, as.numeric(educ)),
                             levels = c(1,2,3,4),
                             labels=c("Primary complete/incomplete",
                                      "Secondary incomplete",
                                      "Secondary complete",
                                      "Higher education complete/incomplete")),
    health_insurance = factor(if_else(prevision_salud == 9, NA_real_, as.numeric(prevision_salud)),
                              levels = c(1,2,3),
                              labels = c("Public (FONASA)", "Private (ISAPRE)", "Other systems")),
    # Reference level = Non-indigenous (for regression contrasts)
    indigenous_status = factor(if_else(pueblos == 9, NA_real_, as.numeric(pueblos)),
                               levels = c(2, 1),
                               labels = c("Non-indigenous", "Indigenous")),
    nationality = factor(if_else(p263 == 9, NA_real_, as.numeric(p263)),
                         levels = c(1,2),
                         labels = c("Chilean", "Other")),
    quality_of_life = factor(if_else(calidad_vida == 9, NA_real_, as.numeric(calidad_vida)),
                             levels = c(1,2,3),
                             labels = c("Very poor/Poor", "Neither good nor poor", "Very good/Good")),
    # Mental health Likert items
    sm1 = if_else(i_1_p11 == 9, NA_real_, i_1_p11-1), # Little interest/pleasure in doing things
    sm2 = if_else(i_2_p11 == 9, NA_real_, i_2_p11-1), # Feeling down/depressed
    sm3 = if_else(i_3_p11 == 9, NA_real_, i_3_p11-1), # Feeling anxious/nervous
    sm4 = if_else(i_4_p11 == 9, NA_real_, i_4_p11-1), # Unable to stop/control worrying

    # PHQ-2: depression screener (range 0-6)
    phq2 = sm1 + sm2,

    # GAD-2: anxiety screener (range 0-6)
    gad2 = sm3 + sm4,

    # PHQ-4: total psychological distress screener (range 0-12)
    phq4 = sm1 + sm2 + sm3 + sm4,

    # Clinical screening flags (Lowe et al., 2010): these are not diagnoses
    phq2_flag = phq2 >= 3,
    gad2_flag = gad2 >= 3,
    phq4_flag = phq4 >= 6,

    # Suicidal ideation (binary)
    suicidal_thought_life = if_else(i_1_p12 == 9, NA_real_, 2-i_1_p12), # Lifetime
    suicidal_thought_12m = if_else(i_1_p13 == 9, NA_real_, 2-i_1_p13), # Last 12 months (filtered)
    # Suicide planning (binary)
    suicidal_plan_life = if_else(i_2_p12 == 9, NA_real_, 2-i_2_p12), # Lifetime
    suicidal_plan_12m = if_else(i_2_p13 == 9, NA_real_, 2-i_2_p13), # Last 12 months (filtered)
    # The following questions apply to trans respondents
    age_realized_trans_identity = if_else(p138 %in% c(777, 888, 999), NA_real_, p138),
    currently_in_transition = if_else(p139 %in% c(8,9), NA_real_, 2-p139),
    tiempo_transicion = p140,
    # Transition details
    transition_social = if_else(i_1_p141 == 9, NA_real_, 2-i_1_p141),
    transition_hormonal = if_else(i_2_p141 == 9, NA_real_, 2-i_2_p141),
    transition_surgery = if_else(i_3_p141 == 9, NA_real_, 2-i_3_p141),
    civil_registry_change = if_else(p142 == 9, NA_real_, if_else(p142 == 3, 0, 1)),
    hormone_source = if_else(p143_o1 %in% c(8, 9), NA_real_, p143_o1),
    intervenciones = if_else(p144 == 2, 0, NA_real_),
    # p145-p146 excluded due to small subsample and coding structure
    dysphoria_certificate = if_else(p147 == 9, NA_real_, if_else(p147 == 2, 0, 1)),
    conversion_treatment = if_else(p148_o1 == 9, NA_real_, p148_o1),
    # Aggressions (binary)
    agr1 = if_else(i_1_p149 == 9, NA_real_, 2-i_1_p149), # Physical assault
    agr2 = if_else(i_2_p149 == 9, NA_real_, 2-i_2_p149), # Sexual assault
    agr3 = if_else(i_3_p149 == 9, NA_real_, 2-i_3_p149), # Threats
    agr4 = if_else(i_4_p149 == 9, NA_real_, 2-i_4_p149), # Verbal aggression
    agr5 = if_else(i_5_p149 == 9, NA_real_, 2-i_5_p149), # Denied bathroom access
    agr6 = if_else(i_6_p149 == 9, NA_real_, 2-i_6_p149), # Expelled/excluded
    agr7 = if_else(i_7_p149 == 9, NA_real_, 2-i_7_p149), # Denied venue entry
    # Number of aggressions (1-4)
    n_agr1 = if_else(i_1_p150 == 9, NA_real_, i_1_p150),
    n_agr2 = if_else(i_2_p150 == 9, NA_real_, i_2_p150),
    n_agr3 = if_else(i_3_p150 == 9, NA_real_, i_3_p150),
    n_agr4 = if_else(i_4_p150 == 9, NA_real_, i_4_p150),
    n_agr5 = if_else(i_5_p150 == 9, NA_real_, i_5_p150),
    n_agr6 = if_else(i_6_p150 == 9, NA_real_, i_6_p150),
    n_agr7 = if_else(i_7_p150 == 9, NA_real_, i_7_p150),
    # Sexual violence (frequency)
    n_violen1 = if_else(i_1_p240 == 9, NA_real_, i_1_p240), # Unwanted sexual comments
    n_violen2 = if_else(i_2_p240 == 9, NA_real_, i_2_p240), # Unwanted physical contact
    # p241c is handled independently
    # Sexual violence (yes/no)
    violen1 = if_else(i_1_p241 == 9, NA_real_, 2-i_1_p241),
    violen2 = if_else(i_2_p241 == 9, NA_real_, 2-i_2_p241),
    violen3 = if_else(p241c == 9, NA_real_, 2-p241c), # Someone masturbated/exposed genitals without consent
    # Harassment (yes/no)
    acoso1 = if_else(i_1_p244 == 9, NA_real_, 2-i_1_p244),
    acoso2 = if_else(i_2_p244 == 9, NA_real_, 2-i_2_p244),
    acoso3 = if_else(i_3_p244 == 9, NA_real_, 2-i_3_p244),
    acoso4 = if_else(i_4_p244 == 9, NA_real_, 2-i_4_p244),
    # Harassment (frequency)
    n_acoso1 = if_else(i_1_p245 == 9, NA_real_, i_1_p245),
    n_acoso2 = if_else(i_2_p245 == 9, NA_real_, i_2_p245),
    n_acoso3 = if_else(i_3_p245 == 9, NA_real_, i_3_p245),
    n_acoso4 = if_else(i_4_p245 == 9, NA_real_, i_4_p245),
    # Harassment (place)
    l_acoso1 = if_else(i_1_p246 == 9, NA_real_, i_1_p246),
    l_acoso2 = if_else(i_2_p246 == 9, NA_real_, i_2_p246),
    l_acoso3 = if_else(i_3_p246 == 9, NA_real_, i_3_p246),
    l_acoso4 = if_else(i_4_p246 == 9, NA_real_, i_4_p246)
  ) |> 
  dplyr::select(varstrat, varunit, w_personas_cal, 
                macro_region, region, comuna,
                sex, gender_identity, trans_from_q3, trans_from_gender, 
                sexual_orientation, sexual_talk_family,
                edad, age_group_4, education_level, health_insurance, indigenous_status, nationality, quality_of_life,
                sm1:sm4, # Mental health
                phq2, gad2, phq4, phq2_flag, gad2_flag, phq4_flag,
                starts_with("suicidal_"),
                age_realized_trans_identity, currently_in_transition, tiempo_transicion,
                starts_with("transition_"), 
                civil_registry_change,
                hormone_source, intervenciones, dysphoria_certificate, conversion_treatment,
                starts_with("agr"), starts_with("n_agr"), 
                starts_with("violen"), starts_with("n_violen"),
                starts_with("acoso"), starts_with("n_acoso"), starts_with("l_acoso"),
  ) 

glimpse(data)

# Build final tables:
# 1) variables applicable to the full population
# 2) trans-specific variables, set to NA for non-trans respondents
trans_specific_vars <- c(
  "age_realized_trans_identity", "currently_in_transition", "tiempo_transicion",
  "transition_social", "transition_hormonal", "transition_surgery",
  "civil_registry_change", "hormone_source", "intervenciones",
  "dysphoria_certificate", "conversion_treatment",
  paste0("agr", 1:7), paste0("n_agr", 1:7),
  "violen1", "violen2", "violen3", "n_violen1", "n_violen2",
  "acoso1", "acoso2", "acoso3", "acoso4",
  "n_acoso1", "n_acoso2", "n_acoso3", "n_acoso4",
  "l_acoso1", "l_acoso2", "l_acoso3", "l_acoso4"
)

data_all_population <- data |>
  dplyr::select(-dplyr::all_of(trans_specific_vars))

data_trans_only <- data |>
  dplyr::select(
    varstrat, varunit, w_personas_cal,
    macro_region, region, comuna,
    sex, gender_identity, trans_from_q3, trans_from_gender,
    dplyr::all_of(trans_specific_vars)
  ) |>
  mutate(
    is_trans = trans_from_q3 == "Yes" | trans_from_gender == "Yes",
    across(dplyr::all_of(trans_specific_vars), ~ if_else(is_trans, ., NA)),
    is_trans = NULL
  )

# Save processed tables
save(data, file = "02_Output/Data/ENSSEX_full_data.RData")
save(data_all_population, file = "02_Output/Data/ENSSEX_trans_population.RData")
save(data_trans_only, file = "02_Output/Data/ENSSEX_trans_only.RData")