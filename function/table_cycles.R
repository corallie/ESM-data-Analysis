#' Cycles summary by id
#'
#' @param cycles a data.frame object of the cycles of the ESM data. 
#' @param save_at the path where to save the table (path + file name.csv)
#'
#' @return
#' @export
#' 
#' @author Coralie Vennin, \email{coralie.vennin@@gmail.com}
#' @author Pauline Mialhe, \email{pauline.mialhe@@univ-reunion.fr}
#'
#' @examples
table_cycles <- function(cycles, save_at = NULL) {
  # 
  # 
  # Features of cycles
  cycles_nb        <- tapply(cycles$cycle, cycles$id, length)
  cycles_auc_tot   <- tapply(cycles$auc_to_mean, cycles$id, sum)
  cycles_dur_tot   <- tapply(cycles$duration, cycles$id, sum)
  cycles_ymax_mean <- tapply(cycles$y_max, cycles$id, mean)
  cycles_ymax_max  <- tapply(cycles$y_max, cycles$id, max)
  # 
  # 
  # Group all features together
  cycles_features <- rbind(cycles_nb, cycles_auc_tot, cycles_dur_tot, cycles_ymax_mean, cycles_ymax_max)
  # 
  # 
  # Add id in colnames
  colnames(cycles_features) <- paste0("id_", colnames(cycles_features))
  # 
  # 
  # Function output : 
  return(cycles_features)
  # 
  # 
}