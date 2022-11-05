library(arrow)
library(dplyr)
library(rlang)
library(forcats)
library(MASS)
library(ggplot2)
library(gt)

decennie_a_partir_annee <- function(annee) {
  #' Transforme annee en decennie
  #' 
  #' @param annee Annee en question
  #' @return La décennie en question
  #' @example 
  #' decennie_a_partir_annee(2011)
  #' decennie_a_partir_annee(2010)
  return(annee - annee %% 10)
}

stats_agregees <- function(x, statistique = "moyenne",
                           ...) {
  #' Statistiques agregees
  #' 
  #' @param statistique Statistique désirée. Les valeurs autorisées
  #'  sont 'moyenne', 'variance', 'ecart-type', 'sd', 'ecart type'
  #' @inheritParams mean
  #' 
  #' @examples
  #' stats_agregees(rnorm(10))
  #' stats_agregees(rnorm(10), "cart type")
  #' stats_agregees(rnorm(10), "ecart type")
  #' stats_agregees(rnorm(10), "variance")
  
  match.arg(statistique,
            c("moyenne",
              "variance",
              "ecart-type",
              "sd",
              "ecart type")
  )
  
  switch(statistique,
         moyenne = mean(x, ...),
         variance = var(x, ...),
         sd(x, ...)
  )
  
}



recode_as_na <- function(df, var_name, value){
  #' Recode some values as NAs
  #' 
  #' @param df `data.frame` we should start from
  #' @param var_name Variable name
  #' @param value Value that should be replaced
  #' @seealso dyplr::na_if
  
  df |>
    mutate({{ var_name }} := na_if(!!rlang::sym(var_name), value))
}


import_yaml <- function(path){
  secrets <- yaml::read_yaml(path)
  api_pwd <- secrets$api_pwd
  return(api_pwd)
}

import_from_parquet <- function(
    path,
    cols = c("region", "aemm", "aged", "anai",
             "catl", "cs1", "cs2", "cs3", "couple", "na38",
             "naf08", "pnai12", "sexe", "surf", "tp", "trans",
             "ur") 
){
  df2 <- arrow::read_parquet(
    path,
    col_select = cols
  )
  df2 <- tibble(df2)
  return(df2)
}

import_parquet_raw <- function(
    path_s3 = "/diffusion/RP/2016/individu_reg/individu_reg.parquet",
    bucket = "donnees-insee"){

  df_parquet <- 
    s3read_using(
      FUN = arrow::read_parquet,
      object = path_s3,
      bucket = bucket,
      opts = list("region" = "")
    )
  
  return(
    df_parquet %>% dplyr::filter(region == "24")
  )
  
}
