
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")


library(tidyverse)
library(dplyr)
library(forcats)


# ENVIRONNEMENT -------------------------

api_token <- yaml::read_yaml("secrets.yaml")$JETON_API

# DEFINITION FONCTIONS -------------------------

decennie_a_partir_annee <- function(annee) {
  return(annee - annee %%
           10)
}

fonction_de_stat_agregee <- function(a, b = "moyenne", ...) {
  if (b == "moyenne") {
    x <- mean(a, na.rm = TRUE, ...)
  } else if (b == "ecart-type" || b == "sd") {
    x <- sd(a, na.rm = TRUE, ...)
  } else if (b == "variance") {
    x <- var(a, na.rm = TRUE, ...)
  }
  return(x)
}

fonction_de_stat_agregee(rnorm(10))
fonction_de_stat_agregee(rnorm(10), "ecart-type")
fonction_de_stat_agregee(rnorm(10), "variance")

# IMPORT DONNEES ------------------

df <- readr::read_csv2(
  "individu_reg.csv",
  col_select = c(
    "region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
    "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp",
    "trans", "ur"
  )
)

# RETRAITEMENT DONNEES -------------------

df$sexe <- df$sexe %>%
  as.character() %>%
  fct_recode(Homme = "1", Femme = "2")


# STATISTIQUES DESCRIPTIVES --------------------

summarise(group_by(df, aged), n())


# part d'homme dans chaque cohort
df %>%
  group_by(aged, sexe) %>%
  summarise(SH_sexe = n()) %>%
  group_by(aged) %>%
  mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
  filter(sexe == 1) %>%
  ggplot() +
  geom_bar(aes(x = as.numeric(aged), y = SH_sexe), stat = "identity") +
  geom_point(aes(x = as.numeric(aged), y = SH_sexe), stat = "identity",
             color = "red") +
  coord_cartesian(c(0, 100))

# stats trans par statut
df2 <- df %>%
  group_by(couple, trans) %>%
  summarise(x = n()) %>%
  group_by(couple) %>%
  mutate(y = 100 * x / sum(x))

df %>%
  filter(sexe == "Homme") %>%
  mutate(aged = as.numeric(aged)) %>%
  pull(aged) %>%
  fonction_de_stat_agregee()

df %>%
  filter(sexe == "Femme") %>%
  mutate(aged = as.numeric(aged)) %>%
  pull(aged) %>%
  fonction_de_stat_agregee()


# GRAPHIQUES -----------

ggplot(df) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)), stat = "count")


p <- ggplot(df2) +
  geom_bar(aes(x = trans, y = y, color = couple), stat = "identity",
           position = "dodge")

ggsave("p.png", p)


# MODELISATION ---------------------


df %>%
  select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z") %>%
  MASS::polr(factor(surf) ~ cs1 + factor(ur), .)

