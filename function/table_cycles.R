#' Title
#'
#' @param phases 
#' @param save_at 
#'
#' @return
#' @export
#'
#' @examples
table_cycles <- function(cycles, save_at = NULL) {
  # 
  # 
  # Caractéristiques des cycles :
  cycles_nb        <- tapply(cycles$cycle, cycles$id, length)
  cycles_auc_tot   <- tapply(cycles$auc_to_mean, cycles$id, sum)
  cycles_dur_tot   <- tapply(cycles$duration, cycles$id, sum)
  cycles_ymax_mean <- tapply(cycles$y_max, cycles$id, mean)
  cycles_ymax_max  <- tapply(cycles$y_max, cycles$id, max)
  # 
  # 
  # On regroupe le tout : 
  cycles_features <- rbind(cycles_nb, cycles_auc_tot, cycles_dur_tot, cycles_ymax_mean, cycles_ymax_max)
  # 
  # 
  # On rajoute id dans le nom de colonne pour que ça soit bien explicite : 
  colnames(cycles_features) <- paste0("id_", colnames(cycles_features))
  # 
  # 
  # On sauvegarde dans un fichier : 
  if (!is.null(save_at))
    write.csv(cycles_features, 
              row.names = T, 
              file = paste0('./figure/TABLE__CYCLES__summary.csv'))
  # 
  # 
  if (is.null(save_at))
    return(cycles_features)
  # 
  # 
}