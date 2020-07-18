#' Compute segments by id
#'
#' @param data a data.frame object of the ESM data described by at least three columns : id, hr and LSNAI. See do_hr to compute hr (ellapsed hours since the monitoring beginning of each id).
#' @param cut_at_mean a boolean set to FALSE. If TRUE, segments goint through the id LSNAI mean are cut in two, creating a fictif point at the id mean. 
#'
#' @return a data.frame object of the ESM data's segments and their features by id.
#' @export
#' 
#' @author Coralie Vennin, \email{coralie.vennin@@gmail.com}
#' @author Pauline Mialhe, \email{pauline.mialhe@@univ-reunion.fr}
#'
#' @examples
do_segments <- function(data, cut_at_mean = FALSE) {
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
  # Id list
  list_id <- unique(data$id)
  # 
  # 
  # Initialisation of the function output containing segments features of all id
  group_segments <- NULL
  # 
  # 
  # For each id,
  for (id in list_id) {
    # 
    # 
    # Get id data
    id_data <- data[data$id == id, c('hr', 'LSNAI')]
    # 
    # 
    # Compute the id's average 
    id_mean <- mean(id_data$LSNAI)
    # standard-deviation
    id_sd <- sd(id_data$LSNAI)
    # and minimum value
    id_min <- min(id_data$LSNAI)
    # 
    # 
    # Number of segments
    nb_segment <- nrow(id_data)-1
    # 
    # 
    # Initialisation of the id output containing its segments features 
    id_segments <- NULL
    # 
    # 
    # Initialisation of the variable which allows to cycles computation
    ind <- 1
    # (not working/usefull for no_cut_segments = TRUE)
    # 
    # 
    # For each segment, 
    for (i_segment in 1:nb_segment) {
      # 
      # First and last ellapsed hours 
      t1 <- id_data$hr[i_segment]
      t2 <- id_data$hr[i_segment+1]
      # and duration between them
      diff_t <- t2 - t1
      # 
      # First and last LSNAI
      y1 <- id_data$LSNAI[i_segment]
      y2 <- id_data$LSNAI[i_segment+1]
      # and amplitude between them
      diff_y <- y2 - y1
      # 
      # 
      # Slope is 
      slope <- diff_y / diff_t
      # and intercept is
      intercept <- (t2*y1 - t1*y2) / diff_t
      # 
      # 
      # For the calculation of the cycles, we have to cut segments in two if they cross the line of the id's average. 
      # So we need to know about the current segment: 
      # Does the segment cross the line of the individual's average? 
      boolean_cross_mean <- (y2 > id_mean) - (y1 > id_mean)
      # > boolean_cross_mean = 0 : it does not cross the average
      # > boolean_cross_mean != 0 : it does
      # 
      # 
      # If we do not cut at mean 
      if (!cut_at_mean | 
          # or if segments does not cross the id's average 
          boolean_cross_mean == 0) { 
        # 
        # 
        # Compute area under the curve until the mean
        auc_to_mean <- pracma::trapz(x = c(t1, t2), 
                                     y = c(y1 - id_mean, y2 - id_mean))
        # 
        # 
        # Compute area under the curve until the id min
        auc_to_min <- pracma::trapz(x = c(t1, t2), 
                                    y = c(y1 - id_min, y2 - id_min))
        # 
        # 
        # Same cycle, ind stay the same
        ind <- ind
        # 
        # 
        # Group segment features together 
        id_segment <- c(id, t1, t2, diff_t, y1, y2, diff_y, slope, intercept, auc_to_mean, auc_to_min, ind, boolean_cross_mean)
        # 
        # 
        # Add segment features into id output (id_segments)
        id_segments <- rbind(id_segments, id_segment)
        # 
        # 
      } else 
        # else if we do cut at mean 
        if (cut_at_mean &
            # and if segments cross the id's average 
            boolean_cross_mean != 0) { 
          # 
          # 
          # We cut the segment in two, at the point : 
          y <- id_mean #y = the id's average
          t <- (y - intercept) / slope #t = ellapsed hour where it crosses
          # 
          # 
          # Compute differences for the first segment 
          diff_t_avmoy <- t - t1 #duration
          diff_y_avmoy <- id_mean - y1 #LSNAI
          # et the second segmend
          diff_t_apmoy <- t2 - t #duration
          diff_y_apmoy <- y2 - id_mean #LSNAI
          # 
          # Compute area under curve and id mean 
          auc_to_mean_avmoy <- pracma::trapz(x = c(t1, t),
                                             y = c(y1 - id_mean, 0))
          auc_to_mean_apmoy <- pracma::trapz(x = c(t, t2),
                                             y = c(0, y2 - id_mean))
          # 
          # 
          # Compute area under curve and id min 
          auc_to_min_avmoy <- pracma::trapz(x = c(t1, t),
                                            y = c(y1 - id_min, 0))
          auc_to_min_apmoy <- pracma::trapz(x = c(t, t2),
                                            y = c(0, y2 - id_min))
          # 
          # 
          # 
          # Group segments features together 
          # 1) first segment (same cycle) : 
          id_segment_avmoy <- c(id, t1, t, diff_t_avmoy, y1, y, diff_y_avmoy, slope, intercept, auc_to_mean_avmoy, auc_to_min_avmoy, ind, boolean_cross_mean)
          # 2) second segments (new cycle) : 
          ind <- ind + 1
          id_segment_apmoy <- c(id, t, t2, diff_t_apmoy, y, y2, diff_y_apmoy, slope, intercept, auc_to_mean_apmoy, auc_to_min_apmoy, ind, boolean_cross_mean)
          # 
          # 
          # Add segment features into id output (id_segments)
          id_segments <- rbind(id_segments, id_segment_avmoy, id_segment_apmoy)
          # 
          # 
        } 
    }
    # 
    # 
    # Conversion to data.frame
    id_segments <- as.data.frame(id_segments)
    rownames(id_segments) <- NULL
    # 
    # 
    # Add segments numbering
    id_segments$segment_no <- 1:nrow(id_segments)
    # 
    # 
    # Add segment features of the id into output (group_segments)
    group_segments <- rbind.data.frame(group_segments, id_segments)
    # 
    # 
  }
  # 
  # 
  # Set colnames
  colnames(group_segments) <- c("id", "t1", "t2", "diff_t", "y1", "y2", "diff_y", 
                                "slope", "intercept", "auc_to_mean", "auc_to_min", 
                                "ind", "mean_inter", "segment_nb")
  # [ CAREFUL : auc_to_mean     = area under the curve until the mean (ligne 42)
  #               et auc_2min   = area under the curve until the id min (ligne 45)
  #               et mean_inter = variable that locates created segments ]
  # and remove rownames
  rownames(group_segments) <- NULL # = reset the numbering
  # 
  # 
  # 
  # We delete the ind column if we do not cut the segments with the average
  if (!cut_at_mean) group_segments[ , colnames(group_segments) == "ind"] <- NULL
  # 
  # We delete the column segment_nb if we cut with the segments with the average
  if (cut_at_mean) group_segments[ , colnames(group_segments) == "segment_nb"] <- NULL
  # and we delete the column mean_inter 
  if (cut_at_mean) group_segments[ , colnames(group_segments) == "mean_inter"] <- NULL
  # 
  # 
  # Function output : 
  return(group_segments)
  # 
  # 
}
# 
# 
# 
# 
#' ____________________________________________________________________________
