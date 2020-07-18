#' Plot phases by id
#'
#' @param data a data.frame object of the ESM data described by at least three columns : id, hr and LSNAI. See do_hr to compute hr (ellapsed hours since the monitoring beginning of each id).
#' @param id the id number to plot
#' @param save_at the path where to save the figure (path + file name.pdf)
#' @param do_legend boolean to show / hide the legend
#'
#' @return
#' @export
#' 
#' @author Coralie Vennin, \email{coralie.vennin@@gmail.com}
#' @author Pauline Mialhe, \email{pauline.mialhe@@univ-reunion.fr}
#'
#' @examples
plot_phases <- function(data, id, save_at = NULL, do_legend = TRUE) {
  # 
  # 
  # Check if not more than one id is selected
  if (length(id) != 1) 
    stop('Select only one id')
  # 
  # 
  # Check data colnames
  if (!all(c('id', 'hr', 'LSNAI') %in% colnames(data))) 
    stop('data should have columns : id, hr and LSNAI')
  # 
  # 
  # Select/reorder data 
  data <- data[ , c('id', 'hr', 'LSNAI')]
  # 
  # 
  # Select/compute data of the selected id
  id_data   <- data[data$id == id, ]
  id_segs   <- do_segments(data = id_data, cut_at_mean = FALSE)
  id_phases <- do_phases(data = id_data)
  # 
  # 
  # Compute mean of the selected id
  id_mean <- mean(id_data$LSNAI)
  # 
  # 
  # If so, save figure using pdf
  if (!is.null(save_at)) 
    pdf(file = paste0(save_at, '/FIGURE__PHASES__id_', id, '.pdf'), width = 7, height = 4)
  # 
  # 
  # Initialisation of the graphical parameters
  par(las = 1, mar = c(4, 5, 2, 4))
  # 
  # 
  # Initialisation of the plot
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
  # Add horizontal dotted lines
  abline(h = c(1:10), col = 'grey30', lty = 3, lwd = .75)
  # 
  # 
  # Add vertical axes
  axis(2, at = 1:10)
  # 
  # 
  # Plot ascending phases
  phases_up <- id_phases[id_phases$cat == 1, ]
  for (irow in 1:nrow(phases_up)) {
    # 
    i_phases_up <- phases_up[irow, ]
    i_seg_up    <- id_segs[id_segs$t1 >= min(i_phases_up$t1) & id_segs$t2 <= max(i_phases_up$tn), ]
    # 
    for (iirow in 1:nrow(i_seg_up)) {
      ii_seg_up <- i_seg_up[iirow, ]
      polygon(x = c(ii_seg_up$t1, ii_seg_up$t2, ii_seg_up$t2, ii_seg_up$t1), 
              y = c(ii_seg_up$y1, ii_seg_up$y2, rep(min(id_data$LSNAI), 2)), 
              col = 'red', border = "transparent")
    }
  }
  # 
  # 
  # Plot descending phases
  phases_down <- id_phases[id_phases$cat == -1, ]
  for (irow in 1:nrow(phases_down)) {
    # 
    i_phases_down <- phases_down[irow, ]
    i_seg_down    <- id_segs[id_segs$t1 >= min(i_phases_down$t1) & id_segs$t2 <= max(i_phases_down$tn), ]
    # 
    for (iirow in 1:nrow(i_seg_down)) {
      ii_seg_down <- i_seg_down[iirow, ]
      polygon(x = c(ii_seg_down$t1, ii_seg_down$t2, ii_seg_down$t2, ii_seg_down$t1), 
              y = c(ii_seg_down$y1, ii_seg_down$y2, rep(min(id_data$LSNAI), 2)), 
              col = 'blue', border = "transparent")
    }
  }
  # 
  # 
  # Locate measures with dots
  points(x = id_data$hr, 
         y = id_data$LSNAI, 
         type = 'o', 
         col = ifelse(id == 2,'black','black'), 
         pch = ifelse(id == 2,4,1), 
         lty = 1) 
  # 
  # 
  # If so, add legend
  if (do_legend)
    legend('topright', legend = paste0('id n°', id, '   '), 
           pch = 1, lty = 1, 
           bg = 'white', col = 'black')
  # 
  # 
  # If so, finalise pdf save
  if (!is.null(save_at)) dev.off() 
  # 
  # 
}