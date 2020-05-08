#' ____________________________________________________________________________
# 
# TABLE 3 : Comparaison cycles id n°1 et id n°2 
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
simulated_ESM_cycles <- read.csv("./data/cycles.csv", stringsAsFactors = F)
# 
# 
# On récupère les données de l'id n°1 : 
id1_data <- simulated_ESM[simulated_ESM$id == 1, ]
id1_cycles <- simulated_ESM_cycles[simulated_ESM_cycles$id == 1, ]
# 
# 
# On récupère les données de l'id n°2 : 
id2_data <- simulated_ESM[simulated_ESM$id == 2, ]
id2_cycles <- simulated_ESM_cycles[simulated_ESM_cycles$id == 2, ]
# 
# 
# On les regroupe ensemble : 
id1_id2_cycles <- rbind(id1_cycles, id2_cycles)
# 
# 
# On filtre les cycles "hauts"
id1_id2_cycles   <- id1_id2_cycles[id1_id2_cycles$type == 1, ]
# 
# 
# On sauvegarde dans un fichier : 
write.csv(id1_id2_cycles, 
          row.names = T, 
          file = paste0('./figure/TAB3A_cycles.csv'))
# 
# 
# Caractéristiques des cycles :
cycles_nb        <- tapply(id1_id2_cycles$cycle, id1_id2_cycles$id, length)
cycles_aire_tot  <- tapply(id1_id2_cycles$aire_2mean, id1_id2_cycles$id, sum)
cycles_duree_tot <- tapply(id1_id2_cycles$duree, id1_id2_cycles$id, sum)
cycles_ymax_mean <- tapply(id1_id2_cycles$y_max, id1_id2_cycles$id, mean)
cycles_ymax_max  <- tapply(id1_id2_cycles$y_max, id1_id2_cycles$id, max)
# 
# 
# 
# 
# On regroupe le tout : 
cycles_caract <- rbind(cycles_nb,cycles_aire_tot, cycles_duree_tot, cycles_ymax_mean, cycles_ymax_max)
# 
# 
# On sauvegarde dans un fichier : 
write.csv(cycles_caract, 
          row.names = T, 
          file = paste0('./figure/TAB3_cycle_caract.csv'))
# 
# 
# 