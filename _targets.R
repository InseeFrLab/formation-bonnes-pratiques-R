library(targets)

source("R/functions.R")

tar_option_set(
  packages = c(
    "aws.s3",
    "arrow",
    "dplyr",
    "rlang",
    "forcats",
    "MASS",
    "ggplot2",
    "gt"
  ) 
)


path_secrets <- "secrets.yaml"
path_parquet <- "individu_reg.parquet"



list(
  
  tar_target(secrets_file, path_secrets, format = "file"),
  tar_target(parquet_survey_file, path_parquet, format = "file"),
  tar_target(api_pwd, import_yaml(secrets_file)),
  
  tar_target(
    survey_sample_24,
    import_from_parquet(
      parquet_survey_file),
    format = "parquet"),
  
  # FEATURE ENGINEERING -------------------------
  
  # TRAITEMENT VALEURS MANQUANTES ==================
  tar_target(
    survey_recoded_missing,
    recode_na_survey(survey_sample_24)
  ),
  
  # TYPES EN FACTEUR ===================
  tar_target(
    survey_recoded_factors,
    recode_factors_survey(
      survey_recoded_missing)
  ),
  
  # PRODUCE AGE STATISTICS ----------------------
  tar_target(
    table_age,
    produce_table_age(survey_recoded_factors)
  ),
  
  # GRAPHIQUE PART COUPLE -----------------------  
  
  tar_target(
    super_graphique,
    graph_part_couple_by_surf(
      survey_recoded_factors
    )
  ),
  
  # MODELISATION --------------------------------

  tar_target(
    super_regression,
    create_regression(survey_recoded_factors)
  )
  
  
  
)