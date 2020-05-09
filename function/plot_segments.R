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
#' @examples
plot_segments <- function(data, id, save_at = NULL, do_legend = TRUE) {
  # 
  # 
  # On vérifie qu'il n'y a qu'un id sélectionné : 
  if (length(id) != 1) 
    stop('Select only one id')
  # 
  # 
  # On vérifie les colnames dans data : 
  if (!all(c('id', 'hr', 'NegA') %in% colnames(data))) 
    stop('data should have columns : id, hr and NegA')
  # 
  # 
  # On sélectionne les données pour l'id sélectionné : 
  id_data <- data[data$id == id, ]
  # 
  # 
  # On calcule la moyenne et l'écar-type de l'id sélectionné : 
  means  <- mean(id_data$NegA)
  sds    <- sd(id_data$NegA)
  # 
  # 
  # Le cas échéant, on enregistre le graphique dans le pdf : 
  if (!is.null(save_at)) pdf(paste0(save_at, '/FIGURE__SEGMENTS__id_', id, '.pdf'), 7, 4)
  # 
  # 
  # Initialisation des paramètres du graphique : 
  par(las = 1, mar = c(4, 4, 2, 4))
  # 
  # 
  # Initialisation du graphique : 
  plot(x = id_data$hr, 
       y = id_data$NegA, 
       xlab = 'Hours since the start of ESM', 
       ylab = 'Intensity of negative affect', 
       yaxt = 'n', 
       col = 'black', 
       type = 'o', ylim = c(0.5, 10)) 
  # 
  # 
  # Rajout des lignes pointillées horizontales : 
  abline(h = c(1:10), col = 'grey30', lty = 3, lwd = .75)
  # 
  # 
  # Rajout de la ligne en rouge de la moyenne : 
  abline(h = means, col = 'red', lty = 1, lwd = 1.5) 
  # 
  # 
  # Rajout des lignes en bleu de l'écart-type : 
  abline(h = means+sds, col = 'blue', lty = 1) #mean + sd
  abline(h = means-sds, col = 'blue', lty = 1) #mean - sd
  # 
  # 
  # Rajout des valeurs de l'écart-type +/- la moyenne : 
  axis(4, at = c(means - sds, means + sds), 
       col.axis = 'blue', col.ticks = 'blue', 
       labels = round(c(means - sds, means + sds), digits = 1))
  # 
  # 
  # Rajout des valeurs de l'écart-type : 
  axis(4, at = c(means - sds/2, means + sds/2), 
       labels = paste0("(", paste0(c("-", "+"), round(sds, digits = 1)), ")"), 
       col.axis = 'blue', col.ticks = 'transparent', font = 3)
  # 
  # 
  # Rajout de la valeur de la moyenne : 
  axis(4, at = means, 
       labels = round(means, digits = 1), 
       col.axis = 'red', col.ticks = 'red',
       lwd.ticks = 1.5, font = 2)
  # 
  # 
  # Rajout des valeurs de l'axe vertical :
  axis(2, at = 1:10)
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