
library(dplyr)
library(forcats)
library(MASS)
library(yaml)
library(gt)

decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}

#' Compute aggregated statistics
#'
#' @param a A numeric vector of values.
#' @param b A string. The name of the statistic to compute.
#' @return A number.
#' @examples
#' stats_agregees(rnorm(10))
#' stats_agregees(rnorm(10), "ecart-type")
#' stats_agregees(rnorm(10), "variance")
stats_agregees <- function(a, b = "moyenne",
                           ...) {
  match.arg(b,
            c("moyenne",
              "variance",
              "ecart-type",
              "sd",
              "ecart type")
  )
  
  switch(b,
         moyenne = mean(a, ...),
         variance = var(a, ...),
         sd(a, ...)
  )
  
}

read_yaml_secret <- function(path, key) {
  return(yaml::read_yaml(path)[[key]])
}

read_from_parquet <- function(path) {
  df <- arrow::read_parquet(
    path,
    col_select  = c(
      "region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
      "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp",
      "trans", "ur"
    )
  )
  return(df)
}

produce_table_age <- function(df) {

  stats_age <- df %>%
    group_by(decennie = decennie_a_partir_annee(aged)) %>%
    summarise(n_indiv = n(),
              n_femmes = sum(sexe == "Femme"),
              n_hommes = sum(sexe == "Homme")
              )
  
  table_age <- gt(stats_age) %>%
    tab_header(
      title = "Distribution de la population par décennie"
    ) %>%
    fmt_number(
      columns = n_indiv,
      sep_mark = " ",
      decimals = 0
    ) %>%
    cols_label(
      decennie = "Tranche d'âge",
      n_indiv = "Population",
      n_femmes = "Nombre de femmes",
      n_hommes = "Nombre d'hommes"
    )
  
  return(table_age)
}
