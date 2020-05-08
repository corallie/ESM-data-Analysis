#' ____________________________________________________________________________
# 
# TABLE 2 : Comparaison phases id n°1 et id n°2 
# 
# 
# 
# Nettoyage : 
rm(list = ls())
graphics.off()
# 
# 
# 
# Lecture des données : 
simulated_ESM <- read.csv("./data/simulated_NegA_group.csv", stringsAsFactors = F)
simulated_ESM_phases <- read.csv("./data/phases.csv", stringsAsFactors = F)
# 
# 
# On récupère les données de l'id n°1 : 
id1_data <- simulated_ESM[simulated_ESM$id == 1, ]
id1_phases <- simulated_ESM_phases[simulated_ESM_phases$id == 1, ]
# 
# 
# On récupère les données de l'id n°2 : 
id2_data <- simulated_ESM[simulated_ESM$id == 2, ]
id2_phases <- simulated_ESM_phases[simulated_ESM_phases$id == 2, ]
# 
# 
# On les regroupe ensemble : 
id1_id2_phases <- rbind(id1_phases, id2_phases)
# 
# 
# On sauvegarde dans un fichier : 
write.csv(id1_id2_phases, 
          row.names = T, 
          file = paste0('./figure/TAB2A_phases.csv'))
# 
# 
# Caractéristiques des phases ascendantes :
id1_id2_phases_asc   <- id1_id2_phases[id1_id2_phases$pente_e > 0, ]
phases_asc_aire_tot  <- tapply(id1_id2_phases_asc$aire_2min, id1_id2_phases_asc$id, sum)
phases_asc_aire_moy  <- tapply(id1_id2_phases_asc$aire_2min, id1_id2_phases_asc$id, mean)
phases_asc_duree_tot <- tapply(id1_id2_phases_asc$diff_t, id1_id2_phases_asc$id, sum)
phases_asc_duree_moy <- tapply(id1_id2_phases_asc$diff_t, id1_id2_phases_asc$id, mean)
# 
# 
# Caractéristiques des phases descendantes :
id1_id2_phases_desc   <- id1_id2_phases[id1_id2_phases$pente_e < 0, ]
phases_desc_aire_tot  <- tapply(id1_id2_phases_desc$aire_2min, id1_id2_phases_desc$id, sum)
phases_desc_aire_moy  <- tapply(id1_id2_phases_desc$aire_2min, id1_id2_phases_desc$id, mean)
phases_desc_duree_tot <- tapply(id1_id2_phases_desc$diff_t, id1_id2_phases_desc$id, sum)
phases_desc_duree_moy <- tapply(id1_id2_phases_desc$diff_t, id1_id2_phases_asc$id, mean)
# 
# 
# Caractéristiques des phases descendantes :
id1_id2_phases_plat   <- id1_id2_phases[id1_id2_phases$pente_e == 0, ]
phases_plat_aire_tot  <- tapply(id1_id2_phases_plat$aire_2min, id1_id2_phases_plat$id, sum)
phases_plat_aire_moy  <- tapply(id1_id2_phases_plat$aire_2min, id1_id2_phases_plat$id, mean)
phases_plat_duree_tot <- tapply(id1_id2_phases_plat$diff_t, id1_id2_phases_plat$id, sum)
phases_plat_duree_moy <- tapply(id1_id2_phases_plat$diff_t, id1_id2_phases_plat$id, mean)
# 
# 
# 
# On regroupe le tout : 
phases_caract <- rbind(phases_asc_aire_tot, phases_asc_aire_moy, phases_asc_duree_tot, phases_asc_duree_moy, 
                      phases_desc_aire_tot, phases_desc_aire_moy, phases_desc_duree_tot, phases_desc_duree_moy, 
                      phases_plat_aire_tot, phases_plat_aire_moy, phases_plat_duree_tot, phases_plat_duree_moy)
# 
# 
# On sauvegarde dans un fichier : 
write.csv(phases_caract, 
          row.names = T, 
          file = paste0('./figure/TAB2_phase_caract.csv'))
# 
# 
# 