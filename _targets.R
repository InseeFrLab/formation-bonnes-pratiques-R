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
  tar_target(survey_sample_24,
             import_from_parquet(
               parquet_survey_file),
             format = "parquet")
)