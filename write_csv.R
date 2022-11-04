df_parquet <- 
  aws.s3::s3read_using(
    FUN = arrow::read_parquet,
    object = "/diffusion/RP/2016/individu_reg/individu_reg.parquet",
    bucket = "donnees-insee",
    opts = list("region" = "")
  )

df_parquet <- df_parquet %>% dplyr::filter(region == "24")

arrow::write_parquet(
  df_parquet, "individu_reg.parquet"
)
