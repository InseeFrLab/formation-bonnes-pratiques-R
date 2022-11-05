decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}
# fonction de stat agregee
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

#stats_agregees(rnorm(10))
#stats_agregees(rnorm(10), "cart type")
#stats_agregees(rnorm(10), "ecart type")
#stats_agregees(rnorm(10), "variance")

recode_as_na <- function(df, var_name, value){
  df |>
    mutate({{ var_name }} := na_if(!!rlang::sym(var_name), value))
}



