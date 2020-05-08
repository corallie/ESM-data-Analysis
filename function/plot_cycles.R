plot_cycles <- function(data, segments_cut, cycles, id, type = +1, save_at = NULL) {
  # 
  # 
  # On vérifie qu'il n'y a qu'un id sélectionné : 
  if (length(id) != 1) 
    stop('Choisir un seul id')
  # 
  # 
  # A améliorer : 
  # -> si début est dans un cycle, ça ne fonctionne pas
  # 
  # 
  # 
  # On vérifie les colnames dans data : 
  if (!all(c('id', 'hr', 'NegA') %in% colnames(data))) 
    stop('data pas dans le bon format')
  # 
  # 
  # 
  # On sélectionne les colonnes utiles : 
  data <- data[ , c('id', 'hr', 'NegA')]
  # 
  # 
  # 
  # On vérifie les colnames dans segments_cut : 
  if (!all(c('id', 
             't1', 't2', 'diff_t', 
             'y1', 'y2', 'diff_y', 
             'pente', 'ord_origine', 
             'aire_2mean', 'aire_2min', 
             'cycle') %in% colnames(segments_cut))) 
    stop('segments_cut pas dans le bon format')
  # 
  # 
  # 
  # On vérifie les colnames dans cycles : 
  if (!all(c('id', 
             't_debut', 't_fin', 'duree', 
             'y_min', 'y_max', 'amplitude', 
             'pente_debut', 'pente_debutM', 'pente_fin', 'pente_finM', 
             'aire_2mean', 'aire_2min', 
             'cycle_nb') %in% colnames(cycles))) {
    stop('cycles pas dans le bon format')
  }
  # 
  # 
  # 
  id_data   <- data[data$id == id, ]
  id_segs   <- segments_cut[segments_cut$id == id, ]
  id_cycles <- cycles[cycles$id == id, ]
  # 
  # 
  # Moyenne de l'individu id : 
  id_mean <- mean(id_data$NegA)
  # 
  # 
  if (!is.null(save_at)) pdf(paste0(save_at, '/CYCLES__id_', id, '.pdf'), 7, 4)
  # 
  # Visualisation : 
  # 
  par(las = 1, mar = c(4, 4, 2, 3))
  plot(x = range(id_data$hr), 
       y = range(id_data$NegA), 
       xlab = 'Hours since the start of ESM', 
       ylab = 'Intensity of negative affect', 
       yaxt = 'n',
       type = 'n', 
       lty = 2, 
       ylim = c(0.5, 8)) #du suivi
  abline(h = c(1:8), col = 'grey30', lty = 3, lwd = .75)
  axis(2, at = 1:8)
  # 
  # 
  for (irow in 1:nrow(id_cycles)) {
    # 
    i_cycle <- id_cycles[irow, ]
    i_segs  <- id_segs[id_segs$t1 >= min(i_cycle$t_debut) & id_segs$t2 <= max(i_cycle$t_fin), ]
    # 
    for (iirow in 1:nrow(i_segs)) {
      ii_segs <- i_segs[iirow, ]
      polygon(x = c(ii_segs$t1, ii_segs$t2, ii_segs$t2, ii_segs$t1), 
              y = c(ii_segs$y1, ii_segs$y2, id_mean, id_mean), 
              col = 'darkred', border = "transparent")
    }
  }
  # 
  abline(h = id_mean, col = "red", lwd = 1.25)
  axis(4, at = id_mean, 
       labels = round(id_mean, digits = 1), 
       col.axis = 'red', col.ticks = 'red',
       lwd.ticks = 1.5, font = 2)
  # 
  # 
  points(x = id_data$hr, 
         y = id_data$NegA, 
         type = 'o', 
         col = ifelse(id == 2,'black','black'), 
         pch = ifelse(id == 2,4,1), 
         lty = 1) #du suivi
  # 
  # 
  if (!is.null(save_at)) dev.off() 
  # 
}
