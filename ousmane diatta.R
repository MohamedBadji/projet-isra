library(readxl)
library(ggplot2)
library(dplyr)
library(corrplot)
library(randomForest)







donnees_od_1 <- read_excel("donnees_od_1.xlsx")
View(donnees_od_1)
attach(donnees_od_1)
str(donnees_od_1)
summary(donnees_od_1)

                ###########################################
                #                                         #
                #            VISUALISATION                #
                #                                         #
                ###########################################


#Pour identifier les valeurs extrêmes et la dispersion de la biomasse.
x11()
#par(mfrow=c(2,2))
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE)) 
ggplot(donnees_od_1, aes(y = biomass)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot de la biomasse", y = "Biomasse")

# Relations entre la biomasse et les autres variables
ggplot(donnees_od_1, aes(x = soil_moisture, y = biomass)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relation entre soil_moisture et Biomasse", x = "soil_moisture", y = "Biomasse")

ggplot(donnees_od_1, aes(x = soil_temperature, y = biomass)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relation entre la température du sol et la Biomasse", x = "soil_temperature", y = "Biomasse")

ggplot(donnees_od_1, aes(x = rain, y = biomass)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relation entre rain et Biomasse", x = "rain", y = "Biomasse")

ggplot(donnees_od_1, aes(x = rain_breaks, y = biomass)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relation entre rain_breaks et Biomasse", x = "rain_breaks", y = "Biomasse")

ggplot(donnees_od_1, aes(x = rain_days, y = biomass)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relation entre rain_days et Biomasse", x = "rain_days", y = "Biomasse")

dev.off()

# Corrélation 2 à 2
cor(donnees_od_1, use = "complete.obs")

# Calculer la corrélation entre chaque variable du dataframe et la biomasse.
correlations <- cor(donnees_od_1[, -which(names(donnees_od_1) == "biomass")],
                    donnees_od_1$biomass, use = "complete.obs"); correlations

# Visualiser les corrélations

library(corrplot)
cor_matrix <- cor(donnees_od_1)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, number.cex = 0.7)

#Dans un corrplot, les couleurs représentent la force et la direction de la corrélation :
#   Bleu → Corrélation positive
#   Rouge → Corrélation négative
#⚪ Blanc → Pas de corrélation.







               ################################################
               #                                              #
               #                 MODELISATION                 #
               #                                              #
               ################################################




# Modèle de régression linéaire
modele_od <- lm(biomass ~ soil_moisture + soil_temperature + rain + rain_breaks
                + rain_days)
summary(modele_od)

library(FactoMineR)
library(factoextra)

amc_result <- PCA(donnees_od_1, graph = TRUE)



# Ajuster un modèle Random Forest pour estimer l'importance des variables
rf_model <- randomForest(biomass ~ ., data = donnees_od_1, importance = TRUE, ntree = 500)

# Afficher l'importance des variables
importance(rf_model)

# Visualiser l'importance des variables
varImpPlot(rf_model)

#Soil temperature (température du sol) est la variable la plus influente pour 
#les prédictions, selon les deux métriques.
#Rain breaks (interruption de pluie) joue également un rôle important.
#Soil moisture (humidité du sol) et rain_days (jours de pluie) ont un impact modéré.
#Rain (pluie totale) semble avoir une importance plus faible.

#Régression multiple
#La régression multiple permet de voir comment plusieurs variables 
#expliquent la biomasse.

# Charger les packages nécessaires
#install.packages(c("car"))
library(car)

# Ajuster le modèle de régression multiple
modele_lm <- lm(biomass ~ ., data = donnees_od_1)

# Résumé du modèle
summary(modele_lm)

# Vérifier la colinéarité entre variables (VIF - Variance Inflation Factor)
vif(modele_lm)

#Interprétation
#Les coefficients indiquent l'effet de chaque variable sur la biomasse.
#Le R² montre la proportion de la variance expliquée.
#Le VIF permet de détecter la colinéarité (variables qui signifient la meme chose)
#entre variables (problème si VIF > 5).
#soil_moisture : 1.064 → Très faible colinéarité.
#soil_temperature : 2.113 → Légère colinéarité, sans impact majeur.
#rain : 2.269 → Légère colinéarité.
#rain_breaks : 3.319 → Colinéarité modérée.
#rain_days : 3.103 → Colinéarité modérée.

#XGBoost est un modèle puissant pour identifier les variables les plus importantes.

#install.packages("xgboost")
library(xgboost)

# Préparation des données
X <- as.matrix(donnees_od_1 %>% select(-biomass))  # Supprimer la colonne cible
y <- donnees_od_1$biomass


# Convertir en format XGBoost
dtrain <- xgb.DMatrix(data = X, label = y)

# Ajuster le modèle XGBoost
xgb_model <- xgboost(data = dtrain, objective = "reg:squarederror", nrounds = 100, max_depth = 6)

# Importance des variables
importance_matrix <- xgb.importance(feature_names = colnames(X), model = xgb_model)
print(importance_matrix)

# Visualisation
xgb.plot.importance(importance_matrix)

#Les variables les plus importantes seront classées en fonction
#de leur impact sur la biomasse.


#Gain : Contribution moyenne d’une variable à la réduction de l’erreur 
#lorsqu’elle est utilisée dans un arbre. Plus cette valeur est grande, 
#plus la variable est importante pour la prédiction.
#Cover : Proportion des observations couvertes par une variable lorsqu’elle 
#est utilisée dans le modèle.
#Frequency : Fréquence d’apparition d’une variable dans les arbres de décision du modèle.
#Analyse des résultats

#Soil_temperature (Température du sol):
#Gain : 0.879 C’est la variable la plus importante pour le modèle en termes 
#de contribution à la réduction de l’erreur.
#Cover : 0.356 Elle couvre une part significative des données.
#Frequency : 0.266 Elle est utilisée environ 26,6 % du temps dans les arbres du modèle.

#Soil_moisture (Humidité du sol):
#Gain : 0.0013 Faible contribution au gain, mais...
#Cover : 0.489 Elle est utilisée très souvent dans le modèle.
#Frequency : 0.580 Elle apparaît dans environ 58 % des arbres, ce qui 
#indique qu’elle est souvent utilisée pour la séparation des données.

#Rain (Pluie totale):
#Gain : 0.0887 Moins important que soil_temperature mais encore significatif.
#Cover : 0.099 Elle couvre une petite partie des données.
#Frequency : 0.097 Peu souvent utilisée dans les arbres (seulement 9,7 % du temps).

#Rain_breaks (Interruptions de pluie):
#Gain : 0.0307 Faible importance.
#Cover : 0.033 Elle couvre très peu d’observations.
#Frequency : 0.035 Elle est rarement utilisée dans les arbres.

#Rain_days (Nombre de jours de pluie):
#Gain : 0.00001 Contribution quasi nulle au modèle.
#Cover : 0.020 Peu de couverture.
#Frequency : 0.020 Rarement utilisée (2 % des arbres).