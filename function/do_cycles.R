#' Compute cycles by id
#'
#' @param data a data.frame object of the ESM data described by at least three columns : id, hr and LSNAI. See do_hr to compute hr (ellapsed hours since the monitoring beginning of each id).
#'
#' @return a data.frame object of the ESM data's cycles and their features by id.
#' @export
#' 
#' @author Coralie Vennin, \email{coralie.vennin@@gmail.com}
#' @author Pauline Mialhe, \email{pauline.mialhe@@univ-reunion.fr}
#'
#' @examples
do_cycles <- function(data) {
  # 
  # 
  # Check data colnames
  if (!all(c('id', 'hr', 'LSNAI') %in% colnames(data))) 
    stop('data should have columns : id, hr and LSNAI')
  # 
  # 
  # Select/Reorder colnames
  data <- data[ , c('id', 'hr', 'LSNAI')]
  # 
  # 
  # Cycles need segments with cut at mean
  segments_cut <- do_segments(data = data, cut_at_mean = TRUE)
  # 
  # 
  # Id list
  list_id <- unique(segments_cut$id)
  # 
  # 
  # 
  # Initialisation of the function output containing cycles features of all id
  group_cycles <- NULL
  # 
  # 
  # For each id,
  for (id in list_id) {
    # 
    # 
    # 
    # Get id data
    id_segments_cut <- segments_cut[segments_cut$id == id, c('hr', 'LSNAI')]
    # 
    # 
    # Number of cycle
    nb_cycle <- max(id_segments_cut$ind)
    #   
    # 
    # 
    # Initialisation of the output containing cycles features of id
    id_cycles <- NULL
    # 
    # 
    # For each cycle, 
    for (icycle in 1:nb_cycle) {
      #
      # Get segments features of the cycle
      id_icycle_segs <- id_segments_cut[id_segments_cut$ind == icycle, ]
      # 
      # 
      # Number of segments in the cycle
      seg_number <- nrow(id_icycle_segs)
      # 
      # 
      # Formate data for the cycle
      id_data <- data.frame(t = c(id_icycle_segs$t1, id_icycle_segs$t2[seg_number]), 
                            y = c(id_icycle_segs$y1, id_icycle_segs$y2[seg_number])) 
      # 
      # 
      # Cycle feature
      t1          <- min(id_data$t) # its beginning 
      tn          <- max(id_data$t) # its ending
      duration    <- tn - t1 # its duration
      y_min       <- min(id_data$y) # its LSNAI minimum 
      y_max       <- max(id_data$y) # its LSNAI maximum 
      amplitude   <- y_max - y_min # its LSNAI amplitude
      auc_to_mean <- sum(id_icycle_segs$auc_to_mean) #its area under the curve to the id's average
      # 
      #
      # Add type to extract cycles over the mean (not under)
      if (auc_to_mean > 0) { 
        type   <- 1
      }
      #
      if (auc_to_mean < 0) {
        type   <- -1
      }
      #
      # Group cycle features together 
      id_icycle <- c(id, type, 
                     t1, tn, duration, 
                     y_min, y_max, amplitude,
                     auc_to_mean)
      #
      # Add id_icycle to the id output 
      id_cycles <- rbind.data.frame(id_cycles, id_icycle)
      #
    }
    # 
    # 
    # Rename colnames
    colnames(id_cycles) <- c("id", "type", 
                             "t1", "tn", "duration", 
                             "y_min", "y_max", "amplitude", 
                             "auc_to_mean")
    # Remove rownames
    rownames(id_cycles) <- NULL #reset numbering
    # 
    # 
    # Extract cycles and remove type column
    id_cycles <- id_cycles[id_cycles$type == 1, colnames(id_cycles) != "type"]
    # 
    # 
    # Remove rownames
    rownames(id_cycles) <- NULL
    # 
    # 
    # Add column with cycle numbering
    id_cycles$cycle_nb <- 1:nrow(id_cycles)
    # 
    #
    # Formate cycles output (group_cycles)
    group_cycles <- rbind.data.frame(group_cycles, id_cycles)
  }
  # 
  # 
  # Function output : 
  return(group_cycles)
  # 
  # 
}
