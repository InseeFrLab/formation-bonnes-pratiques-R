library(tidyverse)
library(dplyr)
library(forcats)

source("R/functions.R", encoding = "UTF-8")

# ENVIRONNEMENT -------------------------

api_pwd <- yaml::read_yaml("secrets.yaml")$JETON_API

stats_agregees(rnorm(10))
stats_agregees(rnorm(10), "ecart-type")
stats_agregees(rnorm(10), "variance")

# IMPORT DONNEES ------------------

df <- arrow::read_parquet(
  "individu_reg.parquet",
  col_select  = c(
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

stats_age <- df %>%
  mutate(age = as.numeric(aged)) %>%
  group_by(decennie = decennie_a_partir_annee(age)) %>%
  summarise(n())

table_age <- gt::gt(stats_age) %>%
  gt::tab_header(
    title = "Distribution des âges dans notre population"
  ) %>%
  gt::fmt_number(
    columns = `n()`,
    sep_mark = " ",
    decimals = 0
  ) %>%
  gt::cols_label(
    decennie = "Tranche d'âge",
    `n()` = "Population"
  )
# GRAPHIQUES -----------

ggplot(df) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)), stat = "count")


p <- ggplot(df2) +
  geom_bar(aes(x = trans, y = y, color = couple), stat = "identity",
           position = "dodge")

ggsave("p.png", p)


# MODELISATION ---------------------


df %>%
  dplyr::select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z") %>%
  MASS::polr(factor(surf) ~ cs1 + factor(ur), .)

