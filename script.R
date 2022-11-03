library(tidyverse)
library(dplyr)

# j'importe les données avec read_csv2 parce que c'est un csv avec des ; et que read_csv attend comme separateur des , 
df <- readr::read_csv2(
  "/home/onyxia/formation-bonnes-pratiques-R/individu_reg.csv",
  col_names = c("region", "aemm", "aged", "anai","catl","cs1", "cs2", "cs3", "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp", "trans", "ur"))

# y a un truc qui va pas avec l'import, je corrige
colnames(df) <- df[1,]
df <- df[2:nrow(df),]

df2 <- df |>
  select(c("region", "dept", "aemm", "aged", "anai","catl","cs1", "cs2", "cs3", "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp", "trans", "ur"))
print(df2, 20)

# combien de professions
print("Nombre de professions :")
print(summarise(df2,length(unique(unlist(cs3[!is.na(cs1)])))))
print("Nombre de professions :'')
"print(summarise(df2,length(unique(unlist(cs3[!is.na(cs2)])))))
oprint("Nombre de professions :")
print(summarise(df2,length(unique(unlist(cs3[!is.na(cs3)])))))

summarise(group_by(df2, aged), n())

df2 %>% select(aged) %>% ggplot(.) + geom_histogram(aes(x = 5*floor(as.numeric(aged)/5)), stat = "count")




# part d'homme dans chaque cohort
ggplot(df %>% group_by(as.numeric(aged, sexe)) %>% summarise(SH_sexe = n()) %>% group_by(aged) %>% summarise(SH_sexe = SH_sexe/sum(SH_sexe))) %>% filter(sexe==1) + geom_bar(aes(x = as.numeric(aged), y = SH_sexe), stat="identity") + geom_point(aes(x = as.numeric(aged), y = SH_sexe), stat="identity", color = "red") + coord_cartesian(c(0,100))
# correction (qu'il faudra retirer)
# ggplot(
#   df2 %>% group_by(aged, sexe) %>% summarise(SH_sexe = n()) %>% group_by(aged) %>% mutate(SH_sexe = SH_sexe/sum(SH_sexe)) %>% filter(sexe==1)
# ) + geom_bar(aes(x = as.numeric(aged), y = SH_sexe), stat="identity") + geom_point(aes(x = as.numeric(aged), y = SH_sexe), stat="identity", color = "red") + coord_cartesian(c(0,100))


# stats surf par statut
df3 = tibble(df2 |> group_by(couple, surf) %>% summarise(x = n()) %>% group_by(couple) |> mutate(y = 100*x/sum(x))
)
ggplot(df3) %>%
  geom_bar(aes(x = surf, y = y, color = couple), stat = "identity", position = "dodge")

# stats trans par statut
df3 = tibble(df2 |> group_by(couple, trans) %>% summarise(x = n()) %>% group_by(couple) |> mutate(y = 100*x/sum(x))
)
ggplot(df3) +
  geom_bar(aes(x = trans, y = y, color = couple), stat = "identity", position = "dodge")


# recode valeurs manquantes
#valeursManquantes <- data.frame(colonne = c(""), NBRE = c(NA))
#for (i in 1:length(colnames(df2))){
#  x = df2[,i]
#  j=0
#  t <-0
#  for (j in 1:nrow(x)){
#    if (is.na(pull(x[j,])) == T) t <- t+1
#  }
#  data.frame(
#    
#  )
#}
df2[df2$na38 == "ZZ","na38"] <- NA
df2[df2$na38 == "Z","trans"] <- NA
df2[df2$tp == "Z","tp"] <- NA
df2[endsWith(df2$naf08, "Z"), "naf08"] <- NA
