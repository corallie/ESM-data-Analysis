#' ____________________________________________________________________________
# 
# FIGURE 3 : Cycles 
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
simulated_ESM    <- read.csv("./data/simulated_NegA_group.csv", stringsAsFactors = F)
simulated_segs   <- read.csv("./data/segments_cut.csv", stringsAsFactors = F)
simulated_cycles <- read.csv("./data/cycles.csv", stringsAsFactors = F)
# 
# 
# 
# 
# ID : one by one ====
# On choisit un id : 
i_id <- 1 
# 
# 
# On récupère les données de l'id : 
id_vals   <- simulated_ESM[simulated_ESM$id == i_id, ]
id_segs   <- simulated_segs[simulated_segs$id == i_id, ]
id_cycles <- simulated_cycles[simulated_cycles$id == i_id, ]
# 
# 
# On calcule la moyenne de l'id : 
id_mean <- mean(id_vals$NegA)
# 
# 
# On regarde que les cycles "hauts" : 
id_cycles <- id_cycles[id_cycles$type == 1, ]
# > entre les pics max et la valeur moyenne de l'id 
# 
# 
# 
# Pour visualiser les données : 
source('./function/plot_cycles.R')
plot_cycles(data = simulated_ESM, segments_cut = simulated_segs, simulated_cycles, id = 1)
# 
# 
# 
# 