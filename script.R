library(targets)

source("R/functions.R", encoding = "UTF-8")


tar_load(api_pwd)

# IMPORT DONNEES ----------------------------

tar_load(df2)


# FEATURE ENGINEERING -------------------------

tar_load(survey_recoded_factors)


# STATISTIQUES DESCRIPTIVES -------------------

# COMPTE PROFESSIONS =================

# combien de professions
print("Nombre de professions :")
sapply(survey_recoded_factors %>% dplyr::select(starts_with("cs")),
       function(x) n_distinct(x))


# STATISTIQUES AGE ======================

summarise(group_by(survey_recoded_factors, age), n())

tar_load(table_age)

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

temp <- part_total(df2) |> filter(sexe == "Homme")

ggplot(temp) +
  geom_bar(aes(x = as.numeric(age),
               y = share), stat = "identity") +
  geom_point(aes(x = as.numeric(age),
                 y = share), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))


# stats surf par statut ==================


tar_load(super_graphique)



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

tar_load(super_regression)
