
library(dplyr)
library(forcats)
library(MASS)
library(yaml)
library(gt)

source("R/functions.R", encoding = "UTF-8")



# ENVIRONNEMENT ----------------------------

api_token <- yaml::read_yaml("R/secrets.yaml")$JETON_API




# IMPORT DONNEES ----------------------------

# j'importe les données avec read_csv2 parce que c'est un csv avec des ;
# et que read_csv attend comme separateur des ,
df <- arrow::read_parquet(
  "individu_reg.parquet",
  col_select  = c(
    "region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
    "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp",
    "trans", "ur"
  )
)

# RETRAITEMENT DES DONNEES -------------------------

## TRAITEMENT VALEURS MANQUANTES ==================

df <- df %>%
  mutate(na38 = na_if(na38, "ZZ"),
         trans = na_if(trans, "Z"),
         tp = na_if(tp, "Z"),
         naf08 = na_if(naf08, "ZZZZZ"),
         aemm = na_if(aemm, "ZZZZ"))

## TYPES EN FACTEUR ===================

df$sexe <- df$sexe %>%
  as.character() %>%
  fct_recode(Homme = "1", Femme = "2")

df <- df %>%
  mutate(aged = as.numeric(aged))

df <- df %>%
  mutate(across(
    c(-region, -aemm, -aged, -anai),
    as.factor)
  )



# STATISTIQUES DESCRIPTIVES -------------------

## COMPTE PROFESSIONS =================

print("Nombre de professions :")
print(summarise(df, length(unique(unlist(cs3[!is.na(cs3)])))))
print("Nombre de professions :''")
print(summarise(df, length(unique(unlist(cs2[!is.na(cs2)])))))
print("Nombre de professions :")
print(summarise(df, length(unique(unlist(cs1[!is.na(cs1)])))))

## STATISTIQUES AGE ======================

df %>%
  group_by(aged) %>%
  summarise(n()) %>%
  ggplot(aes(x=aged, y=`n()`)) +
  geom_bar(stat = "identity")

df %>%
  filter(aged > 50) %>%
ggplot(aes(
  x = aged,
  y = ..density.., fill = factor(decennie_a_partir_annee(as.numeric(aemm)))
), alpha = 0.2) +
  geom_histogram() # position = "dodge") + scale_fill_viridis_d()

stats_age <- df %>%
  group_by(decennie = decennie_a_partir_annee(aged)) %>%
  summarise(n())

table_age <- gt(stats_age) %>%
  tab_header(
    title = "Distribution des âges dans notre population"
  ) %>%
  fmt_number(
    columns = `n()`,
    sep_mark = " ",
    decimals = 0
  ) %>%
  cols_label(
    decennie = "Tranche d'âge",
    `n()` = "Population"
  )

## Part d'homme dans chaque cohorte ===========
ggplot(df %>%
         group_by(aged, sexe) %>%
         summarise(SH_sexe = n()) %>%
         group_by(aged) %>%
         mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
         filter(sexe == 1)) +
  geom_bar(aes(x = aged, y = SH_sexe), stat = "identity") +
  geom_point(aes(x = aged, y = SH_sexe),
             stat = "identity",
             color = "red"
  ) +
  coord_cartesian(c(0, 100))

# stats surf par statut ==================

df3 <- tibble(df %>%
                group_by(couple, surf) %>%
                summarise(x = n()) %>%
                group_by(couple) %>%
                mutate(y = 100 * x / sum(x)))
ggplot(df3) +
  geom_bar(aes(x = surf, y = y, color = couple),
           stat = "identity",
           position = "dodge"
  )

# stats trans par statut ===================

df3 <- tibble(df %>%
                group_by(couple, trans) %>%
                summarise(x = n()) %>%
                group_by(couple) %>%
                mutate(y = 100 * x / sum(x)))
p <- ggplot(df3) +
  geom_bar(aes(x = trans, y = y, color = couple),
           stat = "identity",
           position = "dodge"
  )

ggsave("p.png", p)





# STATS AGREGEES =================

stats_agregees(df %>%
                           filter(sexe == "Homme") %>%
                           mutate(aged = aged) %>%
                           pull(aged))
stats_agregees(df %>%
                           filter(sexe == "Femme") %>%
                           mutate(aged = aged) %>%
                           pull(aged))
stats_agregees(df %>%
                           filter(sexe == "Homme" & couple == "2") %>%
                           mutate(aged = aged) %>%
                           pull(aged))
stats_agregees(df %>%
                           filter(sexe == "Femme" & couple == "2") %>%
                           mutate(aged = aged) %>%
                           pull(aged))

# MODELISATION =================

df3 <- df %>%
  dplyr::select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z")
df3[, 1] <- factor(df3$surf, ordered = TRUE)
df3[, "cs1"] <- factor(df3$cs1)
df3 %>%
  filter(couple == "2" & aged > 40 & aged < 60)
polr(surf ~ cs1 + factor(ur), df3)
