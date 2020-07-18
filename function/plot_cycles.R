#' Plot cycles by id
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
plot_cycles <- function(data, id, save_at = NULL, do_legend = TRUE) {
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
  # Select/Reorder columns
  data <- data[ , c('id', 'hr', 'LSNAI')]
  # 
  # 
  # Select/compute data of the selected id
  id_data   <- data[data$id == id, ]
  id_segs   <- do_segments(data = id_data, cut_at_mean = TRUE)
  id_cycles <- do_cycles(data = id_data)
  # 
  # 
  # Compute mean of the selected id
  id_mean <- mean(id_data$LSNAI)
  # 
  # 
  # If so, save figure using pdf
  if (!is.null(save_at)) pdf(paste0(save_at, '/FIGURE__CYCLES__id_', id, '.pdf'), 7, 4)
  # 
  # 
  # Initialisation of the graphical parameters
  par(las = 1, mar = c(4, 5, 2, 3))
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
  # Plot cycles
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
  # Add horizontal red line at the id's average
  abline(h = id_mean, col = "red", lwd = 1.25)
  # 
  # 
  # Add value : id's average
  axis(4, at = id_mean, 
       labels = round(id_mean, digits = 1), 
       col.axis = 'red', col.ticks = 'red',
       lwd.ticks = 1.5, font = 2)
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