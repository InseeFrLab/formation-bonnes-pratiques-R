library(arrow)
library(dplyr)
library(rlang)
library(forcats)
library(MASS)
library(ggplot2)
library(gt)


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
    mutate(
      {{ var_name }} := na_if(
        !!rlang::sym(var_name), value
      )
    )
}

recode_na_survey <- function(data){
  data <- recode_as_na(data, "na38", "ZZ")
  data <- recode_as_na(data, "trans", "Z")
  data <- recode_as_na(data, "tp", "Z")
  data <- recode_as_na(data, "catl", "Z")
  data[endsWith(data$naf08, "ZZ"), "naf08"] <- NA
  return(data)
}

recode_factors_survey <- function(data){
  
  data <- data |>
    mutate(across(
      c(-region, -aemm, -aged, -anai),
      as.factor)
    )
  
  data <- data |>
    mutate(age = as.numeric(aged))
  
  data <- data |>
    mutate(sexe = fct_recode(sexe, Homme = "1", Femme = "2"))
  
  return(data)  
}



produce_table_age <- function(data){
  
  stats_age <- data |> 
    group_by(decennie = decennie_a_partir_annee(age)) |>
    summarise(n())
  
  table_age <- gt(stats_age) |>
    tab_header(
      title = "Distribution des âges dans notre population"
    ) |>
    fmt_number(
      columns = `n()`,
      sep_mark = " ",
      decimals = 0
    ) |>
    cols_label(
      decennie = "Tranche d'âge",
      `n()` = "Population"
    )
  
  return(table_age)
  
}

part_total <- function(df2, var_groupe = "age", var_interet = "sexe"){
  df2 |>
    group_by(!!!syms(c(var_groupe, var_interet))) |>
    summarise(share = n()) |>
    group_by(!!sym(var_groupe)) |>
    mutate(share = share / sum(share))
}

graph_part_couple_by_surf <- function(data){
  
  df3 <- part_total(data, "couple", "surf") |>
    dplyr::filter(couple != "Z")
  
  p <- ggplot(df3) +
    geom_bar(aes(x = surf, y = share,
                 fill = couple),
             stat = "identity", position = "dodge")
  
  return(p)
}

create_regression <- function(data, formula = "surf ~ cs1 + factor(ur)"){
  
  df3 <- data |>
    dplyr::select(surf, cs1, ur, couple, age) |>
    filter(surf != "Z")
  
  reg <- polr(
    as.formula(formula),
    df3 |>
      filter(
        couple == 2 &
          age > 40 &
          age < 60)
  )
}




