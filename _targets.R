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
path_parquet_s3 <- "/diffusion/RP/2016/individu_reg/individu_reg.parquet"
bucket_s3 <- "donnees-insee"


list(
  tar_target(secrets_file, path_secrets, format = "file"),
  tar_target(api_pwd, import_yaml(secrets_file)),
  tar_target(survey_sample_24,
             import_parquet_raw(
               path_parquet_s3,
               bucket_s3),
             format = "parquet")
)