#' Title
#'
#' @param phases 
#' @param save_at 
#'
#' @return
#' @export
#' 
#' @author Coralie Vennin, \email{coralie.vennin@@gmail.com}
#' @author Pauline Mialhe, \email{pauline.mialhe@@univ-reunion.fr}
#'
#' @examples
table_phases <- function(phases, save_at = NULL) {
  # 
  # 
  # Caractéristiques des phases ascendantes :
  phases_asc          <- phases[phases$slope_e > 0, ]
  phases_asc_auc_tot  <- tapply(phases_asc$auc_to_min, phases_asc$id, sum)
  phases_asc_auc_mean <- tapply(phases_asc$auc_to_min, phases_asc$id, mean)
  phases_asc_dur_tot  <- tapply(phases_asc$diff_t, phases_asc$id, sum)
  phases_asc_dur_mean <- tapply(phases_asc$diff_t, phases_asc$id, mean)
  phases_asc_slope_e  <- tapply(phases_asc$slope_e, phases_asc$id, mean)
  phases_asc_nb       <- length(phases_asc)
  # 
  # 
  # Caractéristiques des phases descendantes :
  phases_desc          <- phases[phases$slope_e < 0, ]
  phases_desc_auc_tot  <- tapply(phases_desc$auc_to_min, phases_desc$id, sum)
  phases_desc_auc_mean <- tapply(phases_desc$auc_to_min, phases_desc$id, mean)
  phases_desc_dur_tot  <- tapply(phases_desc$diff_t, phases_desc$id, sum)
  phases_desc_dur_mean <- tapply(phases_desc$diff_t, phases_asc$id, mean)
  phases_desc_slope_e  <- tapply(phases_desc$slope_e, phases_asc$id, mean)
  phases_desc_nb       <- length(phases_desc)
  # 
  # 
  # Caractéristiques des phases plates :
  phases_plat          <- phases[phases$slope_e == 0, ]
  phases_plat_auc_tot  <- tapply(phases_plat$auc_to_min, phases_plat$id, sum)
  phases_plat_auc_mean <- tapply(phases_plat$auc_to_min, phases_plat$id, mean)
  phases_plat_dur_tot  <- tapply(phases_plat$diff_t, phases_plat$id, sum)
  phases_plat_dur_mean <- tapply(phases_plat$diff_t, phases_plat$id, mean)
  phases_plat_nb       <- length(phases_plat)
  # 
  # 
  # On regroupe le tout : 
  phases_features <- rbind(phases_asc_auc_tot, phases_asc_auc_mean, phases_asc_dur_tot, phases_asc_dur_mean, phases_asc_slope_e, phases_asc_nb, 
                           phases_desc_auc_tot, phases_desc_auc_mean, phases_desc_dur_tot, phases_desc_dur_mean, phases_desc_slope_e, phases_desc_nb, 
                           phases_plat_auc_tot, phases_plat_auc_mean, phases_plat_dur_tot, phases_plat_dur_mean, phases_plat_nb)
  # 
  # 
  # On rajoute id dans le nom de colonne pour que ça soit bien explicite : 
  colnames(phases_features) <- paste0("id_", colnames(phases_features))
  # 
  # 
  # Function output : 
  return(phases_features)
  # 
  # 
}