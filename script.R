library(arrow)
library(dplyr)
library(rlang)
library(forcats)
library(MASS)
library(ggplot2)

source("R/functions.R", encoding = "UTF-8")

secrets <- yaml::read_yaml("secrets.yaml")
api_pwd <- secrets$api_pwd


# IMPORT DONNEES ----------------------------

df2 <- arrow::read_parquet(
  "individu_reg.parquet",
  col_select = c("region", "aemm", "aged", "anai",
                 "catl", "cs1", "cs2", "cs3", "couple", "na38",
                 "naf08", "pnai12", "sexe", "surf", "tp", "trans",
                 "ur") 
  )
df2 <- tibble(df2)

# FEATURE ENGINEERING -------------------------

# TRAITEMENT VALEURS MANQUANTES ==================

df2 <- recode_as_na(df2, "na38", "ZZ")
df2 <- recode_as_na(df2, "trans", "Z")
df2 <- recode_as_na(df2, "tp", "Z")
df2[endsWith(df2$naf08, "ZZ"), "naf08"] <- NA


# TYPES EN FACTEUR ===================

df2 <- df2 |>
  mutate(across(
    c(-region, -aemm, -aged, -anai),
    as.factor)
  )

df2 <- df2 |>
  mutate(age = as.numeric(aged))

df2 <- df2 |>
  mutate(sexe = fct_recode(sexe, Homme = "1", Femme = "2"))


# STATISTIQUES DESCRIPTIVES -------------------

# COMPTE PROFESSIONS =================

# combien de professions
print("Nombre de professions :")
sapply(df2 %>% dplyr::select(starts_with("cs")),
       function(x) n_distinct(x))


print(
  summarise(group_by(df2, aged), n())
)


# STATISTIQUES AGE ======================

summarise(group_by(df2, age), n())


df2 |>
  dplyr::select(age) |>
  ggplot() + geom_histogram(aes(x = 5 * floor(age / 5)),
                             stat = "count")

ggplot(df2[as.numeric(df2$aged) > 50,],
       aes(x = as.numeric(aged),
           y = ..density..,
           fill = factor(decennie_a_partir_annee(as.numeric(aemm)))
       ),
       alpha = 0.2) + geom_histogram()



# part d'homme dans chaque cohorte ===================

part_total <- function(df2, var_groupe = "age", var_interet = "sexe"){
  df2 |>
    group_by(!!!syms(c(var_groupe, var_interet))) |>
    summarise(share = n()) |>
    group_by(!!sym(var_groupe)) |>
    mutate(share = share / sum(share))
}

temp <- part_total(df2) |> filter(sexe == "Homme")

ggplot(temp) +
  geom_bar(aes(x = as.numeric(age),
               y = share), stat = "identity") +
  geom_point(aes(x = as.numeric(age),
                 y = share), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))


# stats surf par statut ==================

df3 <- part_total(df2, "couple", "surf")


p <- ggplot(df3) +
  geom_bar(aes(x = trans, y = y,
               color = couple),
           stat = "identity", position = "dodge")

dir.create("./output")

ggsave(p, "./output/surf_par_statut.png")


# stats trans par statut ===================

df3 <- part_total(df2, "couple", "trans")

ggplot(df3) + geom_bar(aes(x = trans, y = share, color = couple),
                       stat = "identity", position = "dodge")


# STATS AGREGEES =================

df3_homme <- df2 |>
  filter(sexe == "Homme")
df3_femme <- df2 |>
  filter(sexe != "Homme")
df3_homme_couple <- df3_homme |> filter(couple == 2)
df3_femme_couple <- df3_femme |> filter(couple == 2)

lapply(
  list(df3_homme$age,
       df3_femme$age,
       df3_homme_couple$age,
       df3_femme_couple$age), stats_agregees, na.rm = TRUE
)


# MODELISATION ----------------------------


df3 <- df2 |>
  dplyr::select(surf, cs1, ur, couple, age) |>
  filter(surf != "Z")

polr(surf ~ cs1 + factor(ur),
     df3 |>
       filter(
         couple == 2 &
           age > 40 &
           age < 60)
)
