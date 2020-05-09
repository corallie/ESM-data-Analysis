#' Title
#'
#' @param data 
#' @param segments 
#' @param phases 
#' @param id 
#' @param save_at 
#' @param do_legend 
#'
#' @return
#' @export
#'
#' @examples
plot_phases <- function(data, segments, phases, id, save_at = NULL, do_legend = TRUE) {
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
  # On sélectionne les colonnes utiles : 
  data <- data[ , c('id', 'hr', 'NegA')]
  # 
  # 
  # On vérifie les colnames dans segments : 
  if (!all(c('id', 
             't1', 't2', 'diff_t', 
             'y1', 'y2', 'diff_y', 
             'slope', 'intercept', 
             'auc_to_mean', 'auc_to_min', 
             'mean_inter', 'segment_nb') %in% colnames(segments))) 
    stop('segments in wrong format')
  # 
  # 
  # On vérifie les colnames dans phases : 
  if (!all(c('id', 
             't1', 'tn', 'diff_t', 
             'y1', 'yn', 'diff_y', 
             'slope_e', 'slope_mean', 'slope_sd', 'slope_max', 'slope_min', 
             'auc_to_mean', 'auc_to_min', 
             'cat', 'phase_nb') %in% colnames(phases)))
    stop('phases in wrong format') 
  # 
  # 
  # On sélectionne les données pour l'id sélectionné : 
  id_data   <- data[data$id == id, ]
  id_segs   <- segments[segments$id == id, ]
  id_phases <- phases[phases$id == id, ]
  # 
  # 
  # On calcule la moyenne de l'id sélectionné : 
  id_mean <- mean(id_data$NegA)
  # 
  # 
  # Le cas échéant, on enregistre le graphique dans le pdf : 
  if (!is.null(save_at)) 
    pdf(paste0(save_at, '/FIGURE__PHASES__id_', id, '.pdf'), 7, 4)
  # 
  # 
  # Initialisation des paramètres du graphique : 
  par(las = 1, mar = c(4, 4, 2, 4))
  # 
  # 
  # Initialisation du graphique : 
  plot(x = range(id_data$hr), 
       y = range(id_data$NegA), 
       xlab = 'Hours since the start of ESM', 
       ylab = 'Intensity of negative affect', 
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
  # Rajout des phases asc. : 
  phases_up <- id_phases[id_phases$cat == 1, ]
  for (irow in 1:nrow(phases_up)) {
    # 
    i_phases_up <- phases_up[irow, ]
    i_seg_up    <- id_segs[id_segs$t1 >= min(i_phases_up$t1) & id_segs$t2 <= max(i_phases_up$tn), ]
    # 
    for (iirow in 1:nrow(i_seg_up)) {
      ii_seg_up <- i_seg_up[iirow, ]
      polygon(x = c(ii_seg_up$t1, ii_seg_up$t2, ii_seg_up$t2, ii_seg_up$t1), 
              y = c(ii_seg_up$y1, ii_seg_up$y2, rep(min(id_data$NegA), 2)), 
              col = 'red', border = "transparent")
    }
  }
  # 
  # 
  # Rajout des phases desc. : 
  phases_down <- id_phases[id_phases$cat == -1, ]
  for (irow in 1:nrow(phases_down)) {
    # 
    i_phases_down <- phases_down[irow, ]
    i_seg_down    <- id_segs[id_segs$t1 >= min(i_phases_down$t1) & id_segs$t2 <= max(i_phases_down$tn), ]
    # 
    for (iirow in 1:nrow(i_seg_down)) {
      ii_seg_down <- i_seg_down[iirow, ]
      polygon(x = c(ii_seg_down$t1, ii_seg_down$t2, ii_seg_down$t2, ii_seg_down$t1), 
              y = c(ii_seg_down$y1, ii_seg_down$y2, rep(min(id_data$NegA), 2)), 
              col = 'blue', border = "transparent")
    }
  }
  # 
  # 
  # Rajout de la ligne en rouge de la moyenne : 
  abline(h = id_mean, col = "red", lwd = 1.25)
  # 
  # 
  # Rajout des points de mesures du suivi : 
  points(x = id_data$hr, 
         y = id_data$NegA, 
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