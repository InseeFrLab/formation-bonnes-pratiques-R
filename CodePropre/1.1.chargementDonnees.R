# RP 2019, DEP 57 ----

# read.delim2("source/donneesRP19Dep57.csv",sep = ",",header = TRUE) -> donneesMoselle
donneesMoselle <-
  aws.s3::s3read_using(
    FUN = data.table::fread,
    # Mettre les options de FUN ici
    object = "GT_Bonnes_Pratiques/Sources/donneesRP19Dep57.csv",
    bucket = "olivierpucher",
    opts = list("region" = "")
  )

# Base communes ----

# infosCommunes <- read.delim2("source/commune_2022.csv",sep = ",",header = TRUE,encoding = "UTF-8")
infosCommunes <-
  aws.s3::s3read_using(
    FUN = data.table::fread,
    # Mettre les options de FUN ici
    object = "GT_Bonnes_Pratiques/Sources/commune_2022.csv",
    bucket = "olivierpucher",
    opts = list("region" = "")
  )
