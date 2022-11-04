rm(list = ls())

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("MASS")) install.packages("MASS")


library(tidyverse)
library(dplyr)
library(forcats)
library(MASS)

decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}
# fonction de stat agregee
fonction_de_stat_agregee <- function(a, b = "moyenne",
                                     ...) {
  checkvalue <- FALSE
  for (x in c("moyenne", "variance", "ecart-type",
              "sd", "ecart type")) {
    checkvalue <- (checkvalue | b == x)
  }
  if (checkvalue == FALSE)
    stop("statistique non supportée")
  if (b == "moyenne") {
    x <- mean(a, ...)
  } else if (b == "ecart-type" || b == "sd" || b == "ecart type") {
    x <- sd(b, ...)
  } else if (a == "variance") {
    x <- var(a, ...)
  }
  return(x)
}
fonction_de_stat_agregee(rnorm(10))
fonction_de_stat_agregee(rnorm(10), "cart type")
fonction_de_stat_agregee(rnorm(10), "ecart type")
fonction_de_stat_agregee(rnorm(10), "variance")



# IMPORT DONNEES ----------------------------

df2 <- readr::read_csv2(
  "/home/onyxia/formation-bonnes-pratiques-R/individu_reg.csv",
  col_select = c("region", "aemm", "aged", "anai",
                "catl", "cs1", "cs2", "cs3", "couple", "na38",
                "naf08", "pnai12", "sexe", "surf", "tp", "trans",
                "ur"))

# FEATURE ENGINEERING -------------------------

# TRAITEMENT VALEURS MANQUANTES ==================


df2[df2$na38 == "ZZ", "na38"] <- NA
df2[df2$trans == "Z", "trans"] <- NA
df2[df2$tp == "Z", "tp"] <- NA
df2[endsWith(df2$naf08, "ZZ"), "naf08"] <- NA


# TYPES EN FACTEUR ===================

df2 <- df2 %>%
  mutate(across(
    c(-region, -aemm, -aged, -anai),
    as.factor)
    )

df2 <- df2 %>%
  mutate(age = as.numeric(aged))

df2$sexe <- df2$sexe %>%
  fct_recode(Homme = "1", Femme = "2")


# STATISTIQUES DESCRIPTIVES -------------------

# COMPTE PROFESSIONS =================

# combien de professions
print("Nombre de professions :")
print(summarise(df2, length(unique(unlist(cs3[!is.na(cs3)])))))
print("Nombre de professions :")
print(summarise(df2, length(unique(unlist(cs2[!is.na(cs2)])))))
print("Nombre de professions :")
print(summarise(df2, length(unique(unlist(cs1[!is.na(cs1)])))))


# STATISTIQUES AGE ======================

summarise(group_by(df2, age), n())


df2 %>%
  dplyr::select(age) %>%
  ggplot(.) + geom_histogram(aes(x = 5 * floor(age / 5)),
                             stat = "count")

ggplot(df2[as.numeric(df2$aged) > 50,],
       aes(x = as.numeric(aged),
           y = ..density..,
           fill = factor(decennie_a_partir_annee(as.numeric(aemm)))
       ),
       alpha = 0.2) + geom_histogram()



# part d'homme dans chaque cohorte ===================

temp <- df2 %>%
  group_by(age, sexe) %>%
  summarise(SH_sexe = n()) %>%
  group_by(age) %>%
  mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
  dplyr::filter(sexe == "Homme")

ggplot(temp) +
  geom_bar(aes(x = as.numeric(age),
               y = SH_sexe), stat = "identity") +
  geom_point(aes(x = as.numeric(age),
                 y = SH_sexe), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))


# stats surf par statut ==================

df3 <- tibble(df2 |>
                group_by(couple, surf) %>%
                summarise(x = n()) %>%
                group_by(couple) |>
                mutate(y = 100 * x / sum(x)))
ggplot(df3) %>%
  geom_bar(aes(x = surf, y = y, color = couple),
           stat = "identity", position = "dodge")

# stats trans par statut ===================

df3 <- tibble(df2 |>
                group_by(couple, trans) %>%
                summarise(x = n()) %>%
                group_by(couple) |>
                mutate(y = 100 * x / sum(x)))
ggplot(df3) + geom_bar(aes(x = trans, y = y, color = couple),
                       stat = "identity", position = "dodge")


# STATS AGREGEES -----------------------


fonction_de_stat_agregee(df %>%
                           filter(sexe == "Homme") %>%
                           mutate(aged = as.numeric(aged)) %>%
                           pull(aged), na.rm = TRUE)
fonction_de_stat_agregee(df2 %>%
                           filter(sexe == "Femme") %>%
                           mutate(aged = as.numeric(aged)) %>%
                           pull(aged), na.rm = TRUE)
fonction_de_stat_agregee(df2 %>%
                           filter(sexe == "Homme" & couple == "2") %>%
                           mutate(aged = as.numeric(aged)) %>%
                           pull(aged), na.rm = TRUE)
fonction_de_stat_agregee(df2 %>%
                           filter(sexe == "Femme" & couple == "2") %>%
                           mutate(aged = as.numeric(aged)) %>%
                           pull(aged), na.rm = TRUE)


# modelisation ----------------------------


df3 <- df2 %>%
  select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z")
df3[, 1] <- factor(df3$surf, ordered = TRUE)
df3[, "cs1"] <- factor(df3$cs1)
polr(surf ~ cs1 + factor(ur), df3 %>%
       filter(couple == "2" && as.numeric(aged > 40 &&
                                            aged < 60)))
