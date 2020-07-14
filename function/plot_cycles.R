#' Title
#'
#' @param data 
#' @param id 
#' @param save_at 
#' @param do_legend 
#'
#' @return
#' @export
#' 
#' @author Coralie Vennin, \email{coralie.vennin@@gmail.com}
#' @author Pauline Mialhe, \email{pauline.mialhe@@univ-reunion.fr}
#'
#' @examples
plot_cycles <- function(data, id, save_at = NULL, do_legend = TRUE) {
  # 
  # 
  # On vérifie qu'il n'y a qu'un id sélectionné : 
  if (length(id) != 1) 
    stop('Select only one id')
  # 
  # 
  # A améliorer : 
  # -> si début est dans un cycle, ça ne fonctionne pas
  # 
  # 
  # On vérifie les colnames dans data : 
  if (!all(c('id', 'hr', 'LSNAI') %in% colnames(data))) 
    stop('data should have columns : id, hr and LSNAI')
  # 
  # 
  # On sélectionne les colonnes utiles : 
  data <- data[ , c('id', 'hr', 'LSNAI')]
  # 
  # 
  # On sélectionne les données pour l'id sélectionné : 
  id_data   <- data[data$id == id, ]
  id_segs   <- do_segments(data = id_data, cut_at_mean = TRUE)
  id_cycles <- do_cycles(data = id_data)
  # 
  # 
  # On calcule la moyenne de l'id sélectionné : 
  id_mean <- mean(id_data$LSNAI)
  # 
  # 
  # Le cas échéant, on enregistre le graphique dans le pdf : 
  if (!is.null(save_at)) pdf(paste0(save_at, '/FIGURE__CYCLES__id_', id, '.pdf'), 7, 4)
  # 
  # 
  # Initialisation des paramètres du graphique : 
  par(las = 1, mar = c(4, 5, 2, 3))
  # 
  # 
  # Initialisation du graphique : 
  plot(x = range(id_data$hr), 
       y = range(id_data$LSNAI), 
       xlab = 'Hours since the start of ESM', 
       ylab = 'Level of simultated\nnegative affect intensity', 
       yaxt = 'n',
       type = 'n', 
       lty = 2, 
       ylim = c(0.5, 10)) 
  # 
  # 
  # Rajout des lignes pointillées horizontales : 
  abline(h = c(1:10), col = 'grey30', lty = 3, lwd = .75)
  # 
  # 
  # Rajout des valeurs de l'axe vertical :
  axis(2, at = 1:10)
  # 
  # 
  # Rajout des cycles : 
  for (irow in 1:nrow(id_cycles)) {
    # 
    i_cycle <- id_cycles[irow, ]
    i_segs  <- id_segs[id_segs$t1 >= min(i_cycle$t1) & 
                         id_segs$t2 <= max(i_cycle$tn), ]
    # 
    for (iirow in 1:nrow(i_segs)) {
      ii_segs <- i_segs[iirow, ]
      polygon(x = c(ii_segs$t1, ii_segs$t2, ii_segs$t2, ii_segs$t1), 
              y = c(ii_segs$y1, ii_segs$y2, id_mean, id_mean), 
              col = 'darkred', border = "transparent")
    }
  }
  # 
  # 
  # Rajout de la ligne en rouge de la moyenne : 
  abline(h = id_mean, col = "red", lwd = 1.25)
  # 
  # 
  # Rajout de la valeur de la moyenne à droite :
  axis(4, at = id_mean, 
       labels = round(id_mean, digits = 1), 
       col.axis = 'red', col.ticks = 'red',
       lwd.ticks = 1.5, font = 2)
  # 
  # 
  # Rajout des points de mesures du suivi : 
  points(x = id_data$hr, 
         y = id_data$LSNAI, 
         type = 'o', 
         col = ifelse(id == 2,'black','black'), 
         pch = ifelse(id == 2,4,1), 
         lty = 1) 
  # 
  # 
  # Affichage de la légende : 
  if (do_legend)
    legend('topright', legend = paste0('id n°', id, '   '), 
           pch = 1, lty = 1, 
           bg = 'white', col = 'black')
  # 
  # 
  # Le cas échéant, on finalise la sauvegarde du graphique 
  if (!is.null(save_at)) dev.off() 
  # 
  # 
}