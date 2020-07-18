#' Compute phases by id
#'
#' @param data a data.frame object of the ESM data described by at least three columns : id, hr and LSNAI. See do_hr to compute hr (ellapsed hours since the monitoring beginning of each id).
#'
#' @return a data.frame object of the ESM data's phases and their features by id.
#' @export
#' 
#' @author Coralie Vennin, \email{coralie.vennin@@gmail.com}
#' @author Pauline Mialhe, \email{pauline.mialhe@@univ-reunion.fr}
#'
#' @examples
do_phases <- function(data) {
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
  # For each id,
  group_phases_list <- lapply(list_id, function(id) {
    # 
    # 
    # 
    # Initialisation of the variable where there will be phases features of the id
    id_phases <- NULL
    # 
    # 
    # 
    # Get id data
    id_data     <- data[data$id == id, ] #measures
    id_segments <- do_segments(id_data, cut_at_mean = FALSE) #segments without cut at mean
    id_min      <- min(id_data$LSNAI) #min value of the id
    # 
    # 
    # Remove rownames
    rownames(id_segments) <- NULL # = reset the numbering
    # 
    # 
    # 
    # FOR FLAT PHASES : ====
    # 
    # Boolean to locate the flat phases
    boolean_flat_at_1 <- id_segments$slope == 0 & id_segments$intercept == id_min
    # 
    # Features of sequences of flat phases 
    rle_flat_at_1 <- rle(boolean_flat_at_1*1)
    # 
    # 
    # Get index of the last value of the sequence
    seqs_ind_last <- cumsum(rle_flat_at_1$lengths)[rle_flat_at_1$values == 1]
    # and its length
    seqs_lenghts  <- rle_flat_at_1$lengths[rle_flat_at_1$values == 1]
    # 
    # 
    # Extract the segments of flat phases
    list_of_flat <- lapply(1:length(seqs_ind_last), function(i) {
      i_last <- seqs_ind_last[i]
      i_lenght <- seqs_lenghts[i]
      if (i_lenght == 1) return(id_segments[i_last, ])
      return(id_segments[c(i_last-i_lenght+1):i_last, ])
    })
    # 
    # 
    # Compute feature of each flat phases
    df_of_flat_features <- do.call(rbind.data.frame, lapply(1:length(list_of_flat), function(ind_i_flat) {
      # 
      # Get data of the phase
      i_flat <- list_of_flat[[ind_i_flat]]
      # 
      # and its features
      i_t_deb  <- min(i_flat$t1)    #first time
      i_t_fin  <- max(i_flat$t2)    #last time
      i_diff_t <- i_t_fin - i_t_deb #duration
      i_y_deb  <- i_flat$y1[1]      #first LSNAI
      i_y_fin  <- rev(i_flat$y2)[1] #last LSNAI
      i_diff_y <- i_y_fin-i_y_deb   #amplitude
      #
      # 
      # Formate features together and add some
      i_df_phase <- data.frame(id          = id, 
                               t1          = i_t_deb,
                               tn          = i_t_fin,
                               diff_t      = i_diff_t,
                               y1          = i_y_deb,
                               yn          = i_y_fin,
                               diff_y      = i_diff_y,
                               slope_e     = i_diff_y/i_diff_t, #mean slope
                               auc_to_min  = sum(i_flat$auc_to_min), #area under the curve to the min
                               cat         = 0) #category of phase (0 for flat)
      # Output
      return(i_df_phase)
      # 
    }))
    # 
    # 
    # 
    # FOR ASCENDING PHASES : ====
    # 
    # Boolean to locate the ascending phases
    boolean_asc <- c((id_segments$slope >= 0 & id_segments$intercept != id_min) | 
                       (id_segments$slope > 0 & id_segments$intercept == id_min & id_segments$t1 == 0))
    # 
    # Features of sequences of ascending phases 
    rle_asc <- rle(boolean_asc*1)
    # 
    # 
    # Get index of the last value of the sequence
    seqs_ind_last <- cumsum(rle_asc$lengths)[rle_asc$values == 1]
    # and its length
    seqs_lenghts <- rle_asc$lengths[rle_asc$values == 1]
    # 
    # 
    # Extract the segments of ascending phases
    list_of_asc <- lapply(1:length(seqs_ind_last), function(i) {
      i_last <- seqs_ind_last[i]
      i_lenght <- seqs_lenghts[i]
      if (i_lenght == 1) {
        if (id_segments$slope[i_last] == 0) return(NULL) 
        return(id_segments[i_last, ])
      } else {
        if (all(id_segments$slope[c(i_last-i_lenght+1):i_last] == 0)) return(NULL)
        return(id_segments[c(i_last-i_lenght+1):i_last, ])
      }
    })
    # 
    # 
    # Compute feature of ascending phases
    df_of_asc_features <- do.call(rbind.data.frame, lapply(1:length(list_of_asc), function(ind_i_asc) {
      # 
      # Get data of the phase
      i_asc <- list_of_asc[[ind_i_asc]]
      # 
      # NULL cases
      if(is.null(i_asc)) return(NULL)
      # 
      # and its features
      i_t_deb  <- min(i_asc$t1)     #first time
      i_t_fin  <- max(i_asc$t2)     #last time
      i_diff_t <- i_t_fin - i_t_deb #duration
      i_y_deb  <- i_asc$y1[1]       #first LSNAI
      i_y_fin  <- rev(i_asc$y2)[1]  #last LSNAI
      i_diff_y <- i_y_fin-i_y_deb   #amplitude
      # 
      # Formate features together and add some
      i_df_phase <- data.frame(id          = id, 
                               t1          = i_t_deb,
                               tn          = i_t_fin,
                               diff_t      = i_diff_t,
                               y1          = i_y_deb,
                               yn          = i_y_fin,
                               diff_y      = i_diff_y,
                               slope_e     = i_diff_y/i_diff_t, #mean slope
                               auc_to_min  = sum(i_asc$auc_to_min), #area under the curve to the min
                               cat         = 1) #category of phase (1 for ascending)
    }))
    # 
    # 
    # 
    # FOR DESCENDING PHASES : ====
    # 
    # Boolean to locate the descending phases
    boolean_desc <- c((id_segments$slope <= 0 & id_segments$intercept != id_min) | 
                        (id_segments$slope < 0 & id_segments$intercept == id_min & id_segments$t1 == 0))
    # 
    # Features of sequences of descending phases 
    rle_desc <- rle(boolean_desc*1)
    # 
    # 
    # Get index of the last value of the sequence
    seqs_ind_last <- cumsum(rle_desc$lengths)[rle_desc$values == 1]
    # and its length
    seqs_lenghts <- rle_desc$lengths[rle_desc$values == 1]
    # 
    # 
    # Extract the segments of descending phases
    list_of_desc <- lapply(1:length(seqs_ind_last), function(i) {
      i_last <- seqs_ind_last[i]
      i_lenght <- seqs_lenghts[i]
      if (i_lenght == 1) {
        if (id_segments$slope[i_last] == 0) return(NULL)
        return(id_segments[i_last, ])
      } else {
        if (all(id_segments$slope[c(i_last-i_lenght+1):i_last] == 0)) return(NULL)
        return(id_segments[c(i_last-i_lenght+1):i_last, ])
      }
    })
    # 
    # 
    # Compute feature of descending phases
    df_of_desc_features <- do.call(rbind.data.frame, lapply(1:length(list_of_desc), function(ind_i_desc) {
      # 
      # Get data of the phase
      i_desc <- list_of_desc[[ind_i_desc]]
      # 
      # NULL cases
      if(is.null(i_desc)) return(NULL)
      # 
      # 
      # Remove descending segments that are in ascending phases (= flat segments at the end of an ascending phase could not be in the beginning of a descending phase)
      list_seg_asc <- unlist(lapply(list_of_asc, function(i) if(!is.null(i)) i$segment_nb))
      test_if_not_in_asc <- i_desc$segment_nb %in% list_seg_asc
      if (any(test_if_not_in_asc)) i_desc <- i_desc[!test_if_not_in_asc, ]
      # 
      # Get features of the phase
      i_t_deb  <- min(i_desc$t1)    #first time
      i_t_fin  <- max(i_desc$t2)    #last time
      i_diff_t <- i_t_fin - i_t_deb #duration
      i_y_deb  <- i_desc$y1[1]      #first LSNAI
      i_y_fin  <- rev(i_desc$y2)[1] #last LSNAI
      i_diff_y <- i_y_fin-i_y_deb   #amplitude
      #
      # Formate features together and add some
      i_df_phase <- data.frame(id          = id, 
                               t1          = i_t_deb,
                               tn          = i_t_fin,
                               diff_t      = i_diff_t,
                               y1          = i_y_deb,
                               yn          = i_y_fin,
                               diff_y      = i_diff_y,
                               slope_e     = i_diff_y/i_diff_t, #mean slope
                               auc_to_min  = sum(i_desc$auc_to_min), #area under the curve to the min
                               cat         = -1) #category of phase (-1 for descending)
    }))
    # 
    # Group phases features together 
    id_phases <- rbind(df_of_flat_features, df_of_asc_features, df_of_desc_features)
    rownames(id_phases) <- NULL
    # 
    # 
    # Reorder time
    id_phases <- id_phases[order(id_phases$t1), ]
    rownames(id_phases) <- NULL
    # 
    # 
    # Add phase numbering
    id_phases$phase_nb <- 1:nrow(id_phases)
    # 
    # 
    # id output
    return(id_phases)
    # 
  })
  # 
  # 
  # Formate phases features of all ids
  group_phases <- do.call(rbind.data.frame, group_phases_list)
  rownames(group_phases) <- NULL 
  # 
  # 
  # Function output : 
  return(group_phases)
  # 
  # 
}
# 
# 
# 