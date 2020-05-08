#' ____________________________________________________________________________
# 
# FIGURE 1 : Comparaison id n°1 et id n°2 
# 
# 
# 
# Nettoyage
rm(list = ls())
graphics.off()
# 
# 
# Chemin vers le dossier où les figures sont enregistrées : 
path_to_your_figures <- './figure/'
# 
# 
# 
# Lecture des données : 
simulated_ESM <- read.csv("./data/simulated_NegA_group.csv", stringsAsFactors = F)
# 
# 
# On récupère les données de l'id n°1
id1_hr <- simulated_ESM[simulated_ESM$id == 1, 2]
id1_val  <- simulated_ESM[simulated_ESM$id == 1, 3]
# 
# On récupère les données de l'id n°2
id2_hr <- simulated_ESM[simulated_ESM$id == 2, 2]
id2_val  <- simulated_ESM[simulated_ESM$id == 2, 3]
# 
# 
# Ils ont bien les mêmes moyennes et sd : 
mean(id1_val) == mean(id2_val) #> TRUE
sd(id1_val) == sd(id2_val) #> TRUE
means <- mean(id1_val)
sds   <- sd(id1_val)
# 
# 
# 
# Pour visualiser les données : 
source('./function/plot_segments.R')
plot_segments(simulated_ESM, id = 1, save_at = "./figure/")
# 
# 