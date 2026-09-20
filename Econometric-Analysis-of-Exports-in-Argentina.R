
library(readxl)


APP_9_variables <- read_excel(file.choose())



df <- APP_9_variables
View (df)
df$export <- as.numeric(as.character(df$export))
str(df$export)
summary(df$export)
df <- df[!is.na(df$export), ]

df$qualification_ <- ifelse(df$qualification < 33, "Low",
                            ifelse(df$qualification >= 34 & df$qualification <= 66, "medium","high"))

getwd()
df$secteur_8groupes <- NA

# 1 Industrie lourde
df$secteur_8groupes[df$secteur_industrie %in% c(
  "Basic metals",
  "Non metallic mineral products",
  "Chemicals"
)] <- "Industrie_lourde"

# 2 Machines & équipements
df$secteur_8groupes[df$secteur_industrie %in% c(
  "Machinery and equipment (29-30)",
  "Transport machines (34-35)",
  "Electronics (31-32)",
  "Precision instruments"
)] <- "Machines_equipements"

# 3 Métal & transformation
df$secteur_8groupes[df$secteur_industrie %in% c(
  "Fabricated metal products"
)] <- "Transformation_metal"

# 4 Biens intermédiaires
df$secteur_8groupes[df$secteur_industrie %in% c(
  "Plastics & rubber",
  "Paper",
  "Wood"
)] <- "Biens_intermediaires"

# 5 Biens consommation
df$secteur_8groupes[df$secteur_industrie %in% c(
  "Furniture",
  "Leather",
  "Textiles",
  "Garments"
)] <- "Biens_consommation"

# 6 Agro
df$secteur_8groupes[df$secteur_industrie %in% c(
  "Food"
)] <- "Agroalimentaire"

# 7 Commerce
df$secteur_8groupes[df$secteur_industrie %in% c(
  "Retail",
  "Wholesale",
  "Services of motor vehicles"
)] <- "Commerce"

# 8 Services
df$secteur_8groupes[df$secteur_industrie %in% c(
  "IT",
  "Hotel and restaurants: section H",
  "Publishing, printing, and Recorded media",
  "Transport  Section I: (60-64)",
  "Recycling",
  "Construction Section F:"
)] <- "Services"

df$secteur_8groupes <- as.factor(df$secteur_8groupes)

df$Exportdummy <- ifelse(df$export > 0,1,0)
df$certif_international_dummy <- ifelse(df$certif_international =="Yes",1, 0)
df$site_web_dummy <- ifelse(df$site_web == "Yes",1, 0)
df$cadre_femme_dummy <-ifelse(df$cadre_femme == "Yes",1,0)

df$depense_RetD <- as.numeric(df$depense_RetD)
df$a_depense_RetD <- ifelse(is.na(df$depense_RetD), 0, 1)

model <- glm(Exportdummy~ taille + region + qualification_ + secteur_8groupes+ certif_international_dummy + site_web_dummy+ cadre_femme_dummy + a_depense_RetD,data=df, family = binomial(link = probit))
summary(model)

install.packages("margins")


library(margins)

# Calcul des effets marginaux
mfx <- margins(model)

# Résumé avec moyenne des effets marginaux
summary(mfx)

library(dplyr)

# Récupérer un tableau propre
mfx_table <- summary(mfx) %>%
  as.data.frame() %>%
  rename(
    Variable = factor,
    Effet_marginal = AME,
    Std_error = SE,
    z_value = z,
    p_value = p
  )

library(margins)

# Calcul des effets marginaux
mfx <- margins(model)

# Créer un tableau résumé des effets marginaux
mfx_df <- summary(mfx)

# Afficher
mfx_table

# Récupérer les coefficients du modèle
coef_table <- summary(model)$coefficients
coef_df <- as.data.frame(coef_table)
coef_df$Variable <- rownames(coef_df)

coef_df$Effet_marginal <- mfx_summary$AME[match(coef_df$Variable, mfx_summary$factor)]

coef_df <- coef_df[, c("Variable", "Estimate", "Std. Error", "z value", "Pr(>|z|)", "Effet_marginal")]

# Afficher le tableau final
coef_df

library(margins)

#  Calcul du probit 
model <- glm(Exportdummy ~ ..., data=df, family = binomial(link = "probit"))

# Calcul des effets marginaux
mfx <- margins(model)
mfx_df <- summary(mfx)

# Récupérer les coefficients du modèle
coef_df <- as.data.frame(summary(model)$coefficients)
coef_df$Variable <- rownames(coef_df)

# Ajouter la colonne des effets marginaux
coef_df$Effet_marginal <- mfx_df$AME[match(coef_df$Variable, mfx_df$factor)]



colnames(coef_df)[1:4] <- c("Estimate", "Std_Error", "z_value", "p_value")

r
final_table <- coef_df[, c("Variable", "Estimate", "Std_Error", "z_value", "p_value", "Effet_marginal")]
final_table


#######fin effets marginaux ######

# Charger le package nécessaire
library(dplyr)


# 1. PRÉPARATION DES DONNÉES ET ESTIMATION DU MODÈLE



vars_modele <- c("Exportdummy", "taille", "region", "qualification_", 
                 "secteur_8groupes", "certif_international_dummy", 
                 "site_web_dummy", "cadre_femme_dummy", "a_depense_RetD")


df_propre <- df[complete.cases(df[, vars_modele]), ]


model_propre <- glm(Exportdummy ~ taille + region + qualification_ + secteur_8groupes + 
                      certif_international_dummy + site_web_dummy + cadre_femme_dummy + 
                      a_depense_RetD, 
                    data = df_propre, 
                    family = binomial(link = "probit"))


df_propre$probabilite_export <- predict(model_propre, type = "response")


# 2. CRÉATION DES GROUPES EXTRÊMEs



groupe_fort <- df_propre %>% filter(probabilite_export > 0.70)
groupe_faible <- df_propre %>% filter(probabilite_export < 0.20)


# 3. FONCTIONS D'AFFICHAGE 



stat_binaire <- function(data, variable_nom, titre_propre) {
  oui_pct <- mean(data[[variable_nom]] == 1, na.rm = TRUE) * 100
  non_pct <- 100 - oui_pct
  cat(sprintf("** %s **\n  Oui : %.2f %%\n  Non : %.2f %%\n\n", titre_propre, oui_pct, non_pct))
}


stat_quali <- function(data, variable_nom, titre_propre) {
  cat(sprintf("** %s **\n", titre_propre))
  tab <- prop.table(table(data[[variable_nom]])) * 100
  print(round(tab, 2))
  cat("\n")
}



cat("  PROFIL À FORTE PROBABILITÉ D'EXPORTER (p > 65%)\n")
cat("  (Basé sur", nrow(groupe_fort), "entreprises)\n")


cat("--- Variables Indicatrices ---\n")
stat_binaire(groupe_fort, "site_web_dummy", "Site Web")
stat_binaire(groupe_fort, "certif_international_dummy", "Certification internationale")
stat_binaire(groupe_fort, "a_depense_RetD", "Dépenses en R&D")
stat_binaire(groupe_fort, "cadre_femme_dummy", "Cadre dirigeant femme")

cat("--- Variables Qualitatives ---\n")
stat_quali(groupe_fort, "qualification_", "Qualification de la main d'œuvre")

cat("** Secteur d'activité (Top 3) **\n")
top_secteurs_fort <- sort(round(prop.table(table(groupe_fort$secteur_8groupes)) * 100, 2), decreasing = TRUE)
print(head(top_secteurs_fort, 3))
cat("\n")

cat("  PROFIL À FAIBLE PROBABILITÉ D'EXPORTER (p < 35%)\n")
cat("  (Basé sur", nrow(groupe_faible), "entreprises)\n")

cat("--- Variables Indicatrices ---\n")
stat_binaire(groupe_faible, "site_web_dummy", "Site Web")
stat_binaire(groupe_faible, "certif_international_dummy", "Certification internationale")
stat_binaire(groupe_faible, "a_depense_RetD", "Dépenses en R&D")
stat_binaire(groupe_faible, "cadre_femme_dummy", "Cadre dirigeant femme")

cat("--- Variables Qualitatives ---\n")
stat_quali(groupe_faible, "qualification_", "Qualification de la main d'œuvre")

cat("** Secteur d'activité (Top 3) **\n")
top_secteurs_faible <- sort(round(prop.table(table(groupe_faible$secteur_8groupes)) * 100, 2), decreasing = TRUE)
print(head(top_secteurs_faible, 3))
cat("\n")

