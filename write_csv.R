df_parquet <- 
  aws.s3::s3read_using(
    FUN = arrow::read_parquet,
    object = "/diffusion/RP/2016/individu_reg/individu_reg.parquet",
    bucket = "donnees-insee",
    opts = list("region" = "")
  )

df_parquet

data.table::setDT(df_parquet)

data.table::fwrite(df_parquet[region == "24"], "individu_reg.csv", sep = ";")
