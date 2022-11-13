# df_parquet <- 
#   aws.s3::s3read_using(
#     FUN = arrow::read_parquet,
#     object = "/diffusion/RP/2016/individu_reg/individu_reg.parquet",
#     bucket = "donnees-insee",
#     opts = list("region" = "")
#   )

# df_parquet

# data.table::setDT(df_parquet)

# data.table::fwrite(df_parquet[region == "24"], "individu_reg.csv", sep = ";")

df <- 
  aws.s3::s3read_using(
    FUN = readr::read_csv2,
    object = "diffusion/bonnes-pratiques-r/rp_2016_individu_reg_24.csv",
    bucket = "projet-formation",
    opts = list("region" = "")
  )

readr::write_csv2(df, "individu_reg.csv")
