
decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}

#' Compute aggregated statistics
#'
#' @param x A numeric vector of values.
#' @param stat A string. The name of the statistic to compute.
#' @return A number.
#' @examples
#' calcul_stats_desc(rnorm(10))
#' calcul_stats_desc(rnorm(10), "ecart-type")
#' calcul_stats_desc(rnorm(10), "variance")
calcul_stats_desc <- function(x, stat = "moyenne", ...) {
  if (stat == "moyenne") {
    res <- mean(x, na.rm = TRUE, ...)
  } else if (stat == "ecart-type" || stat == "sd") {
    res <- sd(x, na.rm = TRUE, ...)
  } else if (stat == "variance") {
    res <- var(x, na.rm = TRUE, ...)
  }
  return(res)
}
