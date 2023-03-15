#install.packages("ggplot2")
library(ggplot2)

#####################################################################################
############## PARTIE 1 SUR CLIENTS_4 et CLIENTS_12 : ANALYSE DES DONNÉES ###########
#####################################################################################

# Import des csv
clients_12_ext <- read.csv("C:/vagrant-projects/OracleDatabase/21.3.0/ProjetTPABigData/R/clients_12_ext.csv", header = FALSE)
clients_4_ext <- read.csv("C:/vagrant-projects/OracleDatabase/21.3.0/ProjetTPABigData/R/clients_4_ext.csv", header = FALSE)

# Nommer les colonnes
colnames(clients_12_ext) <- c("id", "age", "sexe", "taux", "situationFamiliale", "nbEnfantsAcharge", "SecondeVoiture", "immatriculation")
colnames(clients_4_ext) <- c("id", "age", "sexe", "taux", "situationFamiliale", "nbEnfantsAcharge", "SecondeVoiture", "immatriculation")

# Combinaison des deux tables
clients_combined <- rbind(clients_12_ext, clients_4_ext)

# Résumé des statistiques descriptives pour chaque variable numérique
summary(clients_combined)

str(clients_combined)

View(clients_combined)

# Afficher le nombre de clients_combined par sexe
table(clients_combined$sexe)

# Tracer un histogramme de la distribution des âges des clients_combined
hist(clients_combined$age)

# Tracer un graphique en boîte des âges des clients_combined pour chaque situation familiale
boxplot(clients_combined$age ~ clients_combined$situationFamiliale)

# On test la proportion de l'âge des clients_combined
qplot(age, data=clients_combined, xlab="Age", ylab="Nb de clients")
# On test le taux des clients_combined 
# Taux selon le sexe ainsi que l'affichage du nb de clients
qplot(taux, data = subset(clients_combined, sexe %in% c("m", "f")), fill = sexe)

# Intervalles d'âge des clients en boîtes à moustache
clients_combined$age_group <- cut(clients_combined$age, breaks = seq(0, 100, by = 10))
ggplot(na.omit(clients_combined), aes(x=age_group, y=taux)) + geom_boxplot() + labs(x="Tranches d'âge", y="Taux")
