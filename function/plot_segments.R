plot_segments <- function(data, id, save_at = NULL) {
  # 
  # 
  # On vérifie qu'il n'y a qu'un id sélectionné : 
  if (length(id) != 1) 
    stop('Choisir un seul id')
  # 
  # 
  # On vérifie les colnames dans data : 
  if (!all(c('id', 'hr', 'NegA') %in% colnames(data))) 
    stop('data pas dans le bon format')
  # 
  # 
  # On sélectionne les colonnes utiles : 
  id_data <- data[data$id == id, ]
  # 
  # 
  id_hr  <- id_data$hr
  id_val <- id_data$NegA
  means  <- mean(id_val)
  sds    <- sd(id_val)
  # 
  # 
  if (!is.null(save_at)) pdf(paste0(save_at, '/SEGMENTS__id_', id, '.pdf'), 7, 4)
  # 
  # Visualisation : 
  par(las = 1, mar = c(4, 4, 2, 4))
  plot(x = id_hr, 
       y = id_val, 
       xlab = 'Hours since the start of ESM', 
       ylab = 'Intensity of negative affect', 
       yaxt = 'n', 
       col = 'black', 
       type = 'o', ylim = c(0.5, 8)) #du suivi
  abline(h = c(1:8), col = 'grey30', lty = 3, lwd = .75)
  abline(h = means, col = 'red', lty = 1, lwd = 1.5) #de la moyenne
  abline(h = means+sds, col = 'blue', lty = 1) #de la moyenne + sd
  abline(h = means-sds, col = 'blue', lty = 1) #de la moyenne - sd
  axis(4, at = c(means - sds, means + sds), 
       col.axis = 'blue', col.ticks = 'blue', #labels = NA), 
       labels = round(c(means - sds, means + sds), digits = 1))
  axis(4, at = c(means - sds/2, means + sds/2), 
       labels = paste0("(", paste0(c("-", "+"), round(sds, digits = 1)), ")"), 
       col.axis = 'blue', col.ticks = 'transparent', font = 3)
  axis(4, at = means, 
       labels = round(means, digits = 1), 
       col.axis = 'red', col.ticks = 'red',
       lwd.ticks = 1.5, font = 2)
  axis(2, at = 1:8)
  legend('topright', legend = paste0('id n°', id, '   '), 
         pch = 1, lty = 1, 
         bg = 'white', col = 'black')
  # 
  if (!is.null(save_at)) dev.off() 
  # 
}