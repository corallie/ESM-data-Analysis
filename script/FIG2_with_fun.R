#' ____________________________________________________________________________
# 
# FIGURE 2 : Phases 
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
simulated_segs   <- read.csv("./data/segments.csv", stringsAsFactors = F)
simulated_phases <- read.csv("./data/phases.csv", stringsAsFactors = F)
# 
# 
# 
# 
# 
# ID : one by one ====
# On choisit un id : 
i_id <- 10 
# 
# 
# On récupère les données de l'id : 
id_vals   <- simulated_ESM[simulated_ESM$id == i_id, ]
id_segs   <- simulated_segs[simulated_segs$id == i_id, ]
id_phases <- simulated_phases[simulated_phases$id == i_id, ]
# 
# 
# 
# Pour visualiser les données : 
source('./function/plot_phases.R')
plot_phases(data = data, segments = segments, phases = phases, id = 1)
# 
# 
# 
# 