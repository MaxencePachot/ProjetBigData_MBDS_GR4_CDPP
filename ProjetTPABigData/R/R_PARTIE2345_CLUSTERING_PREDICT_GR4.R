# Installez les packages si vous ne les avez pas (il faut la dernière version de vctr 0.5.2)
# install.packages("ggplot2")
# install.packages("rattle")
# install.packages("dplyr")
# install.packages("vctrs")
# install.packages("cluster")
# install.packages("randomForest")
library(cluster)
library(ggplot2)

#################################################################################################
#################################################################################################
############## ANALYSE AVEC LES CSV DIRECTELENT EXPORTÉS DEPUIS LES TABLES HIVE #################
#################################################################################################
#################################################################################################

# Créer une table vide avec les noms de colonne spécifiés
Catalogue <- data.frame(id = character(),
                        marque = character(),
                        nom = character(),
                        puissance = character(),
                        longueur = character(),
                        nbPlaces = numeric(),
                        nbPortes = numeric(),
                        couleur = character(),
                        occasion = character(),
                        prix = numeric(),
                        stringsAsFactors = TRUE)

# Importer le fichier CSV
CData <- read.csv("C:/vagrant-projects/OracleDatabase/21.3.0/ProjetTPABigData/R/Catalogue.csv", sep = ",", dec = ".",stringsAsFactors = TRUE)

# Ajouter les données à la table
colnames(CData) <- c("id","marque", "nom", "puissance", "longueur", "nbPlaces", "nbPortes", "couleur", "occasion", "prix")
Catalogue <- rbind(Catalogue, CData)

# Supprimer les colonnes id et immatriculation
Catalogue <- subset(Catalogue, select = -c(id))

# Afficher les premières lignes
head(Catalogue, 20)

# Afficher les informations sur les variables
str(Catalogue)

#####################################################################################
##################### PARTIE 1 SUR CATALOGUE : ANALYSE DES DONNÉES ##################
#####################################################################################

# Résumé des statistiques descriptives pour chaque variable numérique
summary(Catalogue)

summary(Catalogue[,c("nbPlaces", "nbPortes", "prix")])

# Histogrammes des variables numériques
hist(Catalogue$nbPlaces)
hist(Catalogue$nbPortes)
hist(Catalogue$prix)

# conversion de la colonne "longueur" en format numérique
Catalogue$longueur <- as.numeric(Catalogue$longueur)

# histogramme de la longueur
hist(Catalogue$longueur, breaks = seq(0, 4, 1), 
     main = "Distribution de la longueur des véhicules", xlab = "Longueur")

# Boîtes à moustaches des variables numériques
boxplot(Catalogue$prix)

# Boîte à moustaches de la puissance
boxplot(Catalogue$puissance, horizontal = TRUE, main = "Distribution de la puissance", xlab = "Puissance (ch)")

# Nuages de points pour les relations entre les variables numériques
plot(Catalogue$nbPlaces, Catalogue$prix)
plot(Catalogue$nbPortes, Catalogue$prix)

# Fréquences des marques
table(Catalogue$marque)

# Histogramme des fréquences des marques
barplot(table(Catalogue$marque), las = 2, cex.names = 0.7, main = "Fréquences des marques", ylab = "Fréquence", xlab = "Marque")

# Installer le package ggplot2 s'il n'est pas déjà installé
# install.packages("ggplot2")

# graphique affichant le prix des voitures par marque avec la moyenne en losange par marque
ggplot(Catalogue, aes(x = reorder(marque, prix), y = prix, fill = marque)) +
  geom_boxplot() +
  stat_summary(
    aes(group = marque),
    fun = "mean",
    geom = "point",
    shape = 23,
    size = 3,
    color = "black"
  ) +
  labs(title = "Prix des voitures par marque", x = "Marque", y = "Prix (en euros)")

#####################################################################################
############### PARTIE 2 CLUSTERING sur CATALOGUE AVEC KMEANS CHOISI#################
#####################################################################################

# Sélection des variables pertinentes
data <- Catalogue[, c("longueur", "puissance", "prix", "nbPortes")]

# Normalisation des variables
data_norm <- scale(data)

# Clustering k-means en 4 (plusieurs tests on été effectués et 4 était selon nous la meilleure décision)
km <- kmeans(data_norm, centers = 4)

# Sauvegarde du modèle
saveRDS(km, "model.rds")

# Attribution des clusters aux observations du catalogue
Catalogue$cluster <- km$cluster

# Calcul des statistiques descriptives pour chaque cluster
cluster_stats <- aggregate(
  Catalogue[, c("longueur", "puissance", "prix", "nbPlaces", "nbPortes")],
  by = list(cluster = Catalogue$cluster),
  FUN = function(x) c(
    mean = mean(x), sd = sd(x), min = min(x), max = max(x),
    median = median(x), q1 = quantile(x, probs = 0.25), q3 = quantile(x, probs = 0.75)
  )
)

# Affichage des statistiques pour chaque cluster
print(cluster_stats)

# Affichage graphique
# Relation dans chaque cluster : Puissance / Prix
ggplot(Catalogue, aes(x = puissance, y = prix, color = factor(cluster))) +
  geom_point() +
  facet_wrap(~factor(cluster)) +
  labs(title = "Voitures par cluster", x = "puissance", y = "prix", color = "Cluster")

# Relation dans chaque cluster : nbPortes / nbPlaces
ggplot(Catalogue, aes(x = nbPortes, y = nbPlaces, color = factor(cluster))) +
  geom_point() +
  facet_wrap(~factor(cluster)) +
  labs(title = "Voitures par cluster", x = "nbPortes", y = "nbPlaces", color = "Cluster")

#####################################################################################
####################### PARTIE 2 avec hclust NON CHOISI #############################
#####################################################################################

# On a essayé plusieurs techniques de clustering, notamment HCLUST, celle-ci fonctionnait 
# plutôt bien mais lors du predict sur immatriculations nous avons pu constater que hclust n'était pas 
# fonctionnel avec cette fonction donc nous sommes partis sur kmeans
# qui au final ne fonctionnait pas non plus avec kmeans
# Lors de la predict, toutes les valeurs étaient mises dans un seul cluster

# # Sélection des variables pertinentes
# data <- Catalogue[, c("longueur", "puissance", "prix", "nbPlaces", "nbPortes")]
# 
# sapply(data, class)
# 
# # Normalisation des variables
# data_norm <- scale(data)
# 
# # Calcul de la matrice de similarité
# dist_matrix <- dist(data_norm)
# 
# # Clustering hiérarchique agglomératif
# hc <- hclust(dist_matrix, method = "ward.D")
# 
# # Sauvegarde du modèle
# saveRDS(hc, "model_clusters.rds")
# 
# # Visualisation de la dendrogramme
# plot(hc, main = "Dendrogramme")
# 
# # Utilisation de la coupure à 5 clusters
# Catalogue$cluster <- cutree(hc, k = 5)
# 
# # Calcul des statistiques descriptives pour chaque cluster
# cluster_stats <- aggregate(
#   Catalogue[, c("longueur", "puissance", "prix", "nbPlaces", "nbPortes")],
#   by = list(cluster = Catalogue$cluster),
#   FUN = function(x) c(
#     mean = mean(x), sd = sd(x), min = min(x), max = max(x),
#     median = median(x), q1 = quantile(x, probs = 0.25), q3 = quantile(x, probs = 0.75)
#   )
# )
# 
# # Affichage des statistiques pour chaque cluster
# print(cluster_stats)

#####################################################################################
######################### AFFICHAGE DE CHAQUE CLUSTER ###############################
#####################################################################################

#####################################################################################
################################## CLUSTER 1 ########################################
#####################################################################################

# Sélection des voitures du cluster 1
cluster1_cars <- Catalogue[Catalogue$cluster == 1, ]

names(cluster1_cars)

cluster1_table <- as.data.frame(cluster1_cars[c("marque", "nom", "prix", "puissance", "nbPortes", "nbPlaces")])
print(cluster1_table)

library(ggplot2)
ggplot(cluster1_table, aes(x = prix, y = puissance)) + 
  geom_point() +
  labs(title = "Relation entre la puissance et le prix pour les voitures du cluster 1",
       x = "Prix",
       y = "Puissance")

summary(cluster1_table$puissance)
summary(cluster1_table$prix)

#####################################################################################
################################## CLUSTER 2 ########################################
#####################################################################################

# Sélection des voitures du cluster 2
cluster2_cars <- Catalogue[Catalogue$cluster == 2, ]

# Création de la table
cluster2_table <- as.data.frame(cluster2_cars[c("marque", "nom", "prix", "puissance", "nbPortes", "nbPlaces")])

# Affichage de la table
print(cluster2_table)

# Création du graphique
ggplot(cluster2_table, aes(x = prix, y = puissance)) + 
  geom_point() +
  labs(title = "Relation entre la puissance et le prix pour les voitures du cluster 2",
       x = "Prix",
       y = "Puissance")

# Résumé des variables
summary(cluster2_table$puissance)
summary(cluster2_table$prix)

#####################################################################################
################################## CLUSTER 3 ########################################
#####################################################################################

# Sélection des voitures du cluster 3
cluster3_cars <- Catalogue[Catalogue$cluster == 3, ]

# Création de la table
cluster3_table <- as.data.frame(cluster3_cars[c("marque", "nom", "prix", "puissance", "nbPortes", "nbPlaces")])

# Affichage de la table
print(cluster3_table)

# Création du graphique
ggplot(cluster3_table, aes(x = prix, y = puissance)) + 
  geom_point() +
  labs(title = "Relation entre la puissance et le prix pour les voitures du cluster 3",
       x = "Prix",
       y = "Puissance")

# Résumé des variables
summary(cluster3_table$puissance)
summary(cluster3_table$prix)

#####################################################################################
################################## CLUSTER 4 ########################################
#####################################################################################

# Sélection des voitures du cluster 4
cluster4_cars <- Catalogue[Catalogue$cluster == 4, ]

# Création de la table
cluster4_table <- as.data.frame(cluster4_cars[c("marque", "nom", "prix", "puissance", "nbPortes", "nbPlaces")])

# Affichage de la table
print(cluster4_table)

# Création du graphique
ggplot(cluster4_table, aes(x = prix, y = puissance)) + 
  geom_point() +
  labs(title = "Relation entre la puissance et le prix pour les voitures du cluster 4",
       x = "Prix",
       y = "Puissance")

# Résumé des variables
summary(cluster4_table$puissance)
summary(cluster4_table$prix)

#####################################################################################
################ Création d'une table Catalogue avec les classes ###################
#####################################################################################

# Création d'une nouvelle colonne "Classe" dans Catalogue
CatalogueClasses <- Catalogue
CatalogueClasses$Classe <- NA

head(CatalogueClasses)

# Attribution des classes en fonction des clusters
# Les clusters changeant toujours, il est impossible de définir et d'attitrer des noms aux clusters
# Ex : Le cluster 1 représente les luxes, si je relance le script, il pourrait représenter les citadines

# CatalogueClasses$Classe[Catalogue$cluster == 1] <- "Routière/Familliale"
# CatalogueClasses$Classe[Catalogue$cluster == 2] <- "Citadine Plus"
# CatalogueClasses$Classe[Catalogue$cluster == 3] <- "Citadine"
# CatalogueClasses$Classe[Catalogue$cluster == 4] <- "Luxe"

# table(CatalogueClasses$Classe)
# 
# library(ggplot2)
# 
# ggplot(CatalogueClasses, aes(x = prix, y = puissance, color = factor(Classe))) + 
#   geom_point() +
#   labs(title = "Nuage de points pour chaque voiture selon sa Classe",
#        x = "Prix",
#        y = "puissance",
#        color = "Classe")
# 
# # Résumé statistique pour chaque critère en fonction de la classe
# stats_classes <- aggregate(CatalogueClasses[, -c(1:2)], by=list(CatalogueClasses$Classe), FUN=mean)
# 
# # Affiche les stats selon les classes
# stats_classes

#####################################################################################
###### 3 du PDF : Application des classes / clusters à Immatriculations.csv #########
#####################################################################################

Immatriculations <- data.frame(immatriculation = character(),
                               marque = character(),
                               nom = character(),
                               puissance = character(),
                               longueur = character(),
                               nbPlaces = numeric(),
                               nbPortes = numeric(),
                               couleur = character(),
                               occasion = character(),
                               prix = numeric(),
                               stringsAsFactors = TRUE)

# Importer le fichier CSV
ImmData <- read.csv("C:/vagrant-projects/OracleDatabase/21.3.0/ProjetTPABigData/R/Immatriculations_ext.csv", header = TRUE, sep = ",", dec = ".",stringsAsFactors = TRUE)

# Ajouter les données à la table
colnames(ImmData) <- c("immatriculation", "marque", "nom", "puissance", "longueur", "nbPlaces", "nbPortes", "couleur", "occasion", "prix")

Immatriculations <- rbind(Immatriculations, ImmData)

# Afficher les premières lignes de la table
head(Immatriculations,20)

summary(Immatriculations)

# conversion de la colonne "longueur" en format numérique
Immatriculations$longueur <- as.numeric(Immatriculations$longueur)

# Afficher les premières lignes de la table
head(Immatriculations,20)

#####################################################################################
################# PARTIE 1 SUR IMMATRICULATIONS : ANALYSE DES DONNÉES ###############
#####################################################################################

# Résumé des statistiques descriptives pour chaque variable numérique
summary(Immatriculations)

summary(Immatriculations[,c("nbPlaces", "nbPortes", "prix")])

# Histogrammes des variables numériques
hist(Immatriculations$nbPlaces)
hist(Immatriculations$nbPortes)
hist(Immatriculations$prix)

# conversion de la colonne "longueur" en format numérique
Immatriculations$longueur <- as.numeric(Immatriculations$longueur)

# histogramme de la longueur
hist(Immatriculations$longueur, breaks = seq(0, 4, 1), 
     main = "Distribution de la longueur des véhicules", xlab = "Longueur")

# Boîtes à moustaches des variables numériques
boxplot(Immatriculations$prix)

# Boîte à moustaches de la puissance
boxplot(Immatriculations$puissance, horizontal = TRUE, main = "Distribution de la puissance", xlab = "Puissance (ch)")

# Nuages de points pour les relations entre les variables numériques
plot(Immatriculations$nbPlaces, Immatriculations$prix)
plot(Immatriculations$nbPortes, Immatriculations$prix)

# Fréquences des marques
table(Catalogue$marque)

# Histogramme des fréquences des marques
barplot(table(Immatriculations$marque), las = 2, cex.names = 0.7, main = "Fréquences des marques", ylab = "Fréquence", xlab = "Marque")

# Installer le package ggplot2 s'il n'est pas déjà installé
# install.packages("ggplot2")

# graphique affichant le prix des voitures par marque avec la moyenne en losange par marque
ggplot(Immatriculations, aes(x = reorder(marque, prix), y = prix, fill = marque)) +
  geom_boxplot() +
  stat_summary(
    aes(group = marque),
    fun = "mean",
    geom = "point",
    shape = 23,
    size = 3,
    color = "black"
  ) +
  labs(title = "Prix des voitures par marque", x = "Marque", y = "Prix (en euros)")


# ######################################################
# ##################### PREDICT ########################
# ######################################################
# 
# # CETTE PARTIE visait à utiliser le clustering fait sur catalogue mais après de nombreuses
# # tentatives, cela ne fonctionne pas
# 
# # Nous allons donc faire LE CLUSTERING DIRECTEMENT SUR IMMATRICULATIONS
# 
# # On récupère uniquement les variables utiles
# new_data <- Immatriculations[, c("longueur", "puissance", "prix", "nbPortes")]
# 
# # Récupérer le modèle
# model <- readRDS("model.rds")
# 
# library(rattle)
# 
# # Prédire les classes pour immatriculations en utilisant le modèle
# # Ajouter la colonne de clusters prédits à la table d'immatriculations
# Immatriculations$cluster <- predict(model, new_data)
# 
# # Création d'une nouvelle colonne "Classe" dans Catalogue
# ImmatriculationsClasses <- Immatriculations
# ImmatriculationsClasses$Classe <- NA
# 
# # Afficher les premières lignes de la table
# head(ImmatriculationsClasses,50)
# 
# # Affichage du nombre de voitures par classe
# table(ImmatriculationsClasses$Classe)
# 
# summary(ImmatriculationsClasses)
# 
# summary(ImmatriculationsClasses[ImmatriculationsClasses$cluster == 4,])

######################################################
##################### KMEANS #########################
######################################################

# On refait donc un clustering sur IMMATRICULATIONS

new_data_kmeans <- Immatriculations[, c("longueur", "puissance", "prix", "nbPortes")]

# Vérifier s'il y a des valeurs manquantes dans la colonne "puissance" de la table "Immatriculations"
verifNA <- function(data){
  cols_na <- names(which(colSums(is.na(data)) > 0))
  if(length(cols_na) == 0){
    message("Il n'y a pas de valeurs manquantes dans la table.")
  } else {
    message("La table contient des valeurs manquantes dans les colonnes suivantes : ", paste(cols_na, collapse = ", "))
  }
}

verifNA(Immatriculations)

summary(new_data_kmeans)

# Normalisation des variables
new_data_norm <- scale(new_data_kmeans)

summary(new_data_norm)
# Clustering k-means
kmImm <- kmeans(new_data_norm, centers = 4)

# Attribution des clusters aux observations du catalogue
Immatriculations$cluster <- kmImm$cluster

# Calcul des statistiques descriptives pour chaque cluster
cluster_stats_Imm <- aggregate(
  Immatriculations[, c("longueur", "puissance", "prix", "nbPlaces", "nbPortes")],
  by = list(cluster = Immatriculations$cluster),
  FUN = function(x) c(
    mean = mean(x), sd = sd(x), min = min(x), max = max(x),
    median = median(x), q1 = quantile(x, probs = 0.25), q3 = quantile(x, probs = 0.75)
  )
)

# Affichage du nombre de voiture dans chaque cluster  
table(Immatriculations$cluster)

# Affichage des statistiques pour chaque cluster
aggregate(Immatriculations[, -c(1:2)], by=list(Immatriculations$cluster), FUN=mean)

# Afficher les premières lignes de la table
head(Immatriculations,20)

# Encore une fois ici, on ne peut pas définir de nom aux cluster sans intervention humaine du fait 
# que chaque cluster change à chaque lancer du clustering
# Immatriculations$cluster <- ifelse(Immatriculations$cluster == 1, "Citadine", 
#                                    ifelse(Immatriculations$cluster == 2, "Citadine Plus", 
#                                           ifelse(Immatriculations$cluster == 3, "Routière familliale", 
#                                                  ifelse(Immatriculations$cluster == 4, "Luxe", NA))))


######################################################
#################### PARTIE 4 ########################
######################################################

# Fusionner clients (4 et 12) et immatriculations + la colonne cluster

# Charger les fichiers CSV
clients_12_ext <- read.csv("C:/vagrant-projects/OracleDatabase/21.3.0/ProjetTPABigData/R/clients_12_ext.csv", header = FALSE)
clients_4_ext <- read.csv("C:/vagrant-projects/OracleDatabase/21.3.0/ProjetTPABigData/R/clients_4_ext.csv", header = FALSE)

# Nommer les colonnes
colnames(clients_12_ext) <- c("id", "age", "sexe", "taux", "situationFamiliale", "nbEnfantsAcharge", "SecondeVoiture", "immatriculation")
colnames(clients_4_ext) <- c("id", "age", "sexe", "taux", "situationFamiliale", "nbEnfantsAcharge", "SecondeVoiture", "immatriculation")

# Combinaison des deux tables
clients_combined <- rbind(clients_12_ext, clients_4_ext)

table(clients_combined$situationFamiliale)
table(clients_combined$sexe)

summary(clients_combined)

library(dplyr)

ImmatClients <- inner_join(clients_combined, Immatriculations, by = "immatriculation", multiple = "all")

head(ImmatClients)

# Affichage du nombre de voiture dans chaque cluster  
table(ImmatClients$cluster)

######################################################
#################### PARTIE 5 ########################
######################################################

# SEPARER ENSEMBLE D'APPRENTISSAGE ET ENSEMBLE DE TEST

# Fixer la graine aléatoire
set.seed(123)

# Créer un vecteur d'indices aléatoires pour l'échantillonnage
indices <- sample(1:nrow(ImmatClients), size = nrow(ImmatClients), replace = FALSE)

indices
# Calculer le nombre de lignes pour l'ensemble d'apprentissage
n_train <- round(nrow(ImmatClients) * 0.7)

n_train
# Sélectionner les lignes pour l'ensemble d'apprentissage et de test
ImmatClients_EA <- ImmatClients[indices[1:n_train], ]

ImmatClients_ET <- ImmatClients[indices[(n_train+1):nrow(ImmatClients)], ]

# Supprimer les colonnes id et immatriculation
ImmatClients_EA <- subset(ImmatClients_EA, select = -c(id, immatriculation))
ImmatClients_ET <- subset(ImmatClients_ET, select = -c(id, immatriculation))

head(ImmatClients_EA)
head(ImmatClients_ET)

######################
# ARBRES DE DECISION #
######################

library(rpart)

library(C50)

library(rpart.plot)

ImmatClients_EA$cluster <- factor(ImmatClients_EA$cluster)

class(ImmatClients_EA$cluster)

summary(ImmatClients_EA)

colnames(ImmatClients_EA) 

# Construction de l'arbre de decision
tree1 <- rpart(cluster~age+sexe+taux+situationFamiliale+nbEnfantsAcharge+SecondeVoiture, ImmatClients_EA)
tree2 <- C5.0(cluster~age+sexe+taux+situationFamiliale+nbEnfantsAcharge+SecondeVoiture, ImmatClients_EA)

prp(tree1, extra=0, box.col=c("tomato", "cyan","green","orange")[tree1$frame$yval])
plot(tree2, type="simple")

# TEST 

test_tree1 <- predict(tree1, ImmatClients_ET, type="class")
print(test_tree1)
table(test_tree1) 

test_tree2 <- predict(tree2, ImmatClients_ET, type="class")
print(test_tree2)
table(test_tree2) 

#Matrice de Confusion 

#Haut droite : Faux Positif : FP
#Bas droite : Vrai Positif : VP
#Haut gauche : Vrai Negatif : VN
#Bas gauche : Faux Negatif : FN

mc_tree1 <- table(ImmatClients_ET$cluster, test_tree1) 
mc_tree1

# test_tree1
#      1     2     3     4
# 1  5096     5  3032     2
# 2     0  2409  2065   798
# 3   330   137 10828   183
# 4     0     1  1819  2885

mc_tree2 <- table(ImmatClients_ET$cluster, test_tree2) 
mc_tree2

#     1    2    3    4
# 1 8115    3   15    2
# 2    1 2351 2143  777
# 3 3424    5 8045    4
# 4    5    1 1896 2803

#################
# RANDOM FOREST #
#################


library(randomForest)

# Approche 1 : Random Forest
# Tester plusieurs paramétrages de Random Forest
n_trees <- c(50, 100, 200)
mtry <- c(2, 4, 6)
predictors <- c("age", "sexe", "taux", "situationFamiliale", "nbEnfantsAcharge", "SecondeVoiture","cluster")
results_rf <- list()
for (n in n_trees) {
  for (m in mtry) {
    rf_model <- randomForest(cluster ~age + sexe + taux + situationFamiliale + nbEnfantsAcharge + SecondeVoiture, data = ImmatClients_EA[, predictors], ntree = n, mtry = m)
    predictions <- predict(rf_model, newdata = ImmatClients_ET[, predictors])
    accuracy <- sum(predictions == ImmatClients_ET$cluster) / nrow(ImmatClients_ET)
    results_rf[[paste("n=", n, ", mtry=", m)]] <- accuracy
  }
}
# Afficher les résultats de Random Forest
print(results_rf)

# > print(results_rf)
# $`n= 50 , mtry= 2`
# [1] 0.7220347
# 
# $`n= 50 , mtry= 4`
# [1] 0.6946263
# 
# $`n= 50 , mtry= 6`
# [1] 0.6757578
# 
# $`n= 100 , mtry= 2`
# [1] 0.722541
# 
# $`n= 100 , mtry= 4`
# [1] 0.6932087
# 
# $`n= 100 , mtry= 6`
# [1] 0.6756228
# 
# $`n= 200 , mtry= 2`
# [1] 0.7239924
# 
# $`n= 200 , mtry= 4`
# [1] 0.6945926
# 
# $`n= 200 , mtry= 6`
# [1] 0.6763316

##################################################
################# PARTIE 5 #######################
##################################################

# Prédire des classes sur le fichier marketing

# Charger les données
Marketing <- read.csv("C:/vagrant-projects/OracleDatabase/21.3.0/ProjetTPABigData/R/Marketing_ext.csv")

# Nommer les colonnes
colnames(Marketing) <- c("id","age", "sexe", "taux", "situationFamiliale", "nbEnfantsAcharge", "SecondeVoiture")

# Supprimer les colonnes id et immatriculation
Marketing <- subset(Marketing, select = -c(id))

# Prédire les classes avec Random Forest en utilisant le plus précis (n = 200 et mtry = 2)
rf_model <- randomForest(cluster ~ age + sexe + taux + situationFamiliale + nbEnfantsAcharge + SecondeVoiture, data = ImmatClients_EA, ntree = 200, mtry = 2)
Marketing$cluster <- predict(rf_model, newdata = Marketing)

# Vérifier le nombre d'observations dans chaque classe
table(Marketing$cluster)

# Sauvegarder la nouvelle table avec la colonne 'cluster'
write.csv(Marketing, file = "C:/vagrant-projects/OracleDatabase/21.3.0/ProjetTPABigData/R/Marketing_cluster.csv", quote = FALSE)

# On revoit les clusters
# Affichage des statistiques pour chaque cluster
aggregate(Immatriculations[, -c(1:2)], by=list(Immatriculations$cluster), FUN=mean)

