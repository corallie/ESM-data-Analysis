#' Phases summary by id
#'
#' @param phases a data.frame object of the phases of the ESM data.
#' @param save_at the path where to save the table (path + file name.csv)
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
  # Features of ascending phases
  phases_asc          <- phases[phases$slope_e > 0, ]
  phases_asc_auc_tot  <- tapply(phases_asc$auc_to_min, phases_asc$id, sum)
  phases_asc_auc_mean <- tapply(phases_asc$auc_to_min, phases_asc$id, mean)
  phases_asc_dur_tot  <- tapply(phases_asc$diff_t, phases_asc$id, sum)
  phases_asc_dur_mean <- tapply(phases_asc$diff_t, phases_asc$id, mean)
  phases_asc_slope_e  <- tapply(phases_asc$slope_e, phases_asc$id, mean)
  phases_asc_nb       <- length(phases_asc)
  # 
  # 
  # Features of descending phases
  phases_desc          <- phases[phases$slope_e < 0, ]
  phases_desc_auc_tot  <- tapply(phases_desc$auc_to_min, phases_desc$id, sum)
  phases_desc_auc_mean <- tapply(phases_desc$auc_to_min, phases_desc$id, mean)
  phases_desc_dur_tot  <- tapply(phases_desc$diff_t, phases_desc$id, sum)
  phases_desc_dur_mean <- tapply(phases_desc$diff_t, phases_asc$id, mean)
  phases_desc_slope_e  <- tapply(phases_desc$slope_e, phases_asc$id, mean)
  phases_desc_nb       <- length(phases_desc)
  # 
  # 
  # Features of flat phases
  phases_plat          <- phases[phases$slope_e == 0, ]
  phases_plat_auc_tot  <- tapply(phases_plat$auc_to_min, phases_plat$id, sum)
  phases_plat_auc_mean <- tapply(phases_plat$auc_to_min, phases_plat$id, mean)
  phases_plat_dur_tot  <- tapply(phases_plat$diff_t, phases_plat$id, sum)
  phases_plat_dur_mean <- tapply(phases_plat$diff_t, phases_plat$id, mean)
  phases_plat_nb       <- length(phases_plat)
  # 
  # 
  # Group all features together
  phases_features <- rbind(phases_asc_auc_tot, phases_asc_auc_mean,
                           phases_asc_dur_tot, phases_asc_dur_mean,
                           phases_asc_slope_e, phases_asc_nb, 
                           phases_desc_auc_tot, phases_desc_auc_mean, 
                           phases_desc_dur_tot, phases_desc_dur_mean, 
                           phases_desc_slope_e, phases_desc_nb, 
                           phases_plat_auc_tot, phases_plat_auc_mean,
                           phases_plat_dur_tot, phases_plat_dur_mean, 
                           phases_plat_nb)
  # 
  # 
  # Add id in colnames
  colnames(phases_features) <- paste0("id_", colnames(phases_features))
  # 
  # 
  # Function output : 
  return(phases_features)
  # 
  # 
}