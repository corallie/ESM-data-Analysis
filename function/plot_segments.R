#' Plot segments by id
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
plot_segments <- function(data, id, save_at = NULL, do_legend = TRUE) {
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
  # Select data of the selected id
  id_data <- data[data$id == id, ]
  # 
  # 
  # Compute mean and standard-deviation of the selected id
  id_mean  <- mean(id_data$LSNAI)
  id_sd    <- sd(id_data$LSNAI)
  # 
  # 
  # If so, save figure using pdf
  if (!is.null(save_at)) 
    pdf(file = paste0(save_at, '/FIGURE__SEGMENTS__id_', id, '.pdf'), width = 7, height = 4)
  # 
  # 
  # Initialisation of the graphical parameters
  par(las = 1, mar = c(4, 5, 2, 4))
  # 
  # 
  # Initialisation of the plot
  plot(x = id_data$hr, 
       y = id_data$LSNAI, 
       xlab = 'Hours since the start of ESM', 
       ylab = 'Level of simultated\nnegative affect intensity', 
       yaxt = 'n', 
       col = 'black', 
       type = 'o', 
       ylim = c(0.5, 10)) 
  # 
  # 
  # Add horizontal dotted lines
  abline(h = c(1:10), col = 'grey30', lty = 3, lwd = .75)
  # 
  # 
  # Add horizontal red line at the id's average
  abline(h = id_mean, col = 'red', lty = 1, lwd = 1.5) 
  # 
  # 
  # Add blue lines at mean +/- standard-deviation
  abline(h = id_mean+id_sd, col = 'blue', lty = 1) #mean + sd
  abline(h = id_mean-id_sd, col = 'blue', lty = 1) #mean - sd
  # 
  # 
  # Add value : mean +/- standard-deviation
  axis(4, at = c(id_mean - id_sd, id_mean + id_sd), 
       col.axis = 'blue', col.ticks = 'blue', 
       labels = round(c(id_mean - id_sd, id_mean + id_sd), digits = 1))
  # 
  # 
  # Add value : standard-deviation 
  axis(4, at = c(id_mean - id_sd/2, id_mean + id_sd/2), 
       labels = paste0("(", paste0(c("-", "+"), round(id_sd, digits = 1)), ")"), 
       col.axis = 'blue', col.ticks = 'transparent', font = 3)
  # 
  # 
  # Add value : id's average
  axis(4, at = id_mean, 
       labels = round(id_mean, digits = 1), 
       col.axis = 'red', col.ticks = 'red',
       lwd.ticks = 1.5, font = 2)
  # 
  # 
  # Add vertical axes
  axis(2, at = 1:10)
  # 
  # 
  # If so, add legend
  if (do_legend)
    legend('topright', 
           legend = paste0('id n°', id, '   '), 
           pch = 1, lty = 1, 
           bg = 'white', col = 'black')
  # 
  # 
  # If so, finalise pdf save
  if (!is.null(save_at)) dev.off() 
  # 
  # 
}