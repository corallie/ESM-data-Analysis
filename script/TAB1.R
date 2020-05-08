#' ____________________________________________________________________________
# 
# TABLE 1 : Comparaison segments id n°1 et id n°2 
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
simulated_ESM_seg <- read.csv("./data/segments.csv", stringsAsFactors = F)
# 
# 
# On récupère les données de l'id n°1 : 
id1_data <- simulated_ESM[simulated_ESM$id == 1, ]
id1_segs <- simulated_ESM_seg[simulated_ESM_seg$id == 1, ]
# 
# 
# On récupère les données de l'id n°2 : 
id2_data <- simulated_ESM[simulated_ESM$id == 2, ]
id2_segs <- simulated_ESM_seg[simulated_ESM_seg$id == 2, ]
# 
# 
# On les regroupe ensemble : 
id1_id2_segs <- rbind(id1_segs, id2_segs)
# 
# 
# On sauvegarde dans un fichier : 
write.csv(id1_id2_segs, 
          row.names = T, 
          file = paste0('./figure/TAB1A_segments.csv'))
# 
# 
# Caractéristiques de toutes les pentes :
pente_mean <- tapply(id1_id2_segs$pente, id1_id2_segs$id, mean)
pente_max  <- tapply(id1_id2_segs$pente, id1_id2_segs$id, max)
pente_min  <- tapply(id1_id2_segs$pente, id1_id2_segs$id, min)
# 
# 
# Caractéristiques des pentes ascendantes :
id1_id2_segs_asc <- id1_id2_segs[id1_id2_segs$pente > 0, ]
pente_mean_asc   <- tapply(id1_id2_segs_asc$pente, id1_id2_segs_asc$id, mean)
pente_max_asc    <- tapply(id1_id2_segs_asc$pente, id1_id2_segs_asc$id, max)
pente_min_asc    <- tapply(id1_id2_segs_asc$pente, id1_id2_segs_asc$id, min)
# 
# 
# Caractéristiques des pentes descendantes :
id1_id2_segs_desc <- id1_id2_segs[id1_id2_segs$pente < 0, ]
pente_mean_desc   <- tapply(id1_id2_segs_desc$pente, id1_id2_segs_desc$id, mean)
pente_max_desc    <- tapply(id1_id2_segs_desc$pente, id1_id2_segs_desc$id, max)
pente_min_desc    <- tapply(id1_id2_segs_desc$pente, id1_id2_segs_desc$id, min)
# 
# 
# On regroupe le tout : 
pente_caract <- rbind(pente_mean, pente_max, pente_min, 
                      pente_mean_asc, pente_max_asc, pente_min_asc, 
                      pente_mean_desc, pente_max_desc, pente_min_desc)
# 
# 
# On sauvegarde dans un fichier : 
write.csv(pente_caract, 
          row.names = T, 
          file = paste0('./figure/TAB1_pente_caract.csv'))
# 
# 
# 