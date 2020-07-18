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
  # Init function output containing segments features of all id
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
    # Init id output containing its segments features 
    id_segments <- NULL
    # 
    # 
    # Init variable which allows to cycles computation
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
      # For the calculation of the cycles, we have to cut segments in half if they cross the line of the id's average. 
      # So we need to know about the current segment: 
      # Does the segment cross the line of the individual's average? 
      boolean_cross_mean <- (y2 > id_mean) - (y1 > id_mean)
      # > boolean_cross_mean = 0 : it does not cross the average
      # > boolean_cross_mean != 0 : it does
      # 
      # 
      # If segments 
      if (!cut_at_mean | boolean_cross_mean == 0) { 
        # 
        # 
        # On calcule l'aire entre la courbe et la moyenne de l'individu :
        aire_slc <- pracma::trapz(x = c(t1, t2), 
                                  y = c(y1 - id_mean, y2 - id_mean))
        # 
        # 
        # On calcule l'aire entre la courbe et le niveau le plus bas de l'individu :
        aire_ymin <- pracma::trapz(x = c(t1, t2), 
                                   y = c(y1 - id_min, y2 - id_min))
        # 
        # 
        # On reste dans le même cycle :
        ind <- ind
        # 
        # 
        # On regroupe les informations du segment en cours : 
        id_segment <- c(id, t1, t2, diff_t, y1, y2, diff_y, slope, intercept, aire_slc, aire_ymin, ind, boolean_cross_mean)
        # 
        # 
        # On les sauvegarde les caractéristiques du segment en cours dans la variable id_segments : 
        id_segments <- rbind(id_segments, id_segment)
        # 
        # 
      } else if (cut_at_mean & boolean_cross_mean != 0) { #Si le segment traverse la moyenne
        # 
        # 
        # On va devoir couper le segment, le point de coupe est en : 
        y <- id_mean #y = la moyenne
        t <- (y - intercept) / slope #t = hr pour lequel on passe par la moyenne
        # 
        # 
        # On calcule les différences avant la moyenne :  
        diff_t_avmoy <- t - t1
        diff_y_avmoy <- id_mean - y1
        # et après la moyenne !
        diff_t_apmoy <- t2 - t
        diff_y_apmoy <- y2 - id_mean
        # 
        # mais aussi les aires entre la courbe du suivi et la moyenne : 
        aire_slc_avmoy <- pracma::trapz(x = c(t1, t),
                                        y = c(y1 - id_mean, 0))
        aire_slc_apmoy <- pracma::trapz(x = c(t, t2),
                                        y = c(0, y2 - id_mean))
        # 
        # 
        # et les aires entre la courbe du suivi et la valeur minimale : 
        aire_ymin_avmoy <- pracma::trapz(x = c(t1, t),
                                         y = c(y1 - id_min, 0))
        aire_ymin_apmoy <- pracma::trapz(x = c(t, t2),
                                         y = c(0, y2 - id_min))
        # 
        # 
        # 
        # On regroupe les informations du segment en cours, soit : 
        # 1) la partie avant la moyenne (cycle le même que le précédent) : 
        id_segment_avmoy <- c(id, t1, t, diff_t_avmoy, y1, y, diff_y_avmoy, slope, intercept, aire_slc_avmoy, aire_ymin_avmoy, ind, boolean_cross_mean)
        # 2) la partie après la moyenne (on entre dans un nouveau cycle) : 
        ind <- ind + 1
        id_segment_apmoy <- c(id, t, t2, diff_t_apmoy, y, y2, diff_y_apmoy, slope, intercept, aire_slc_apmoy, aire_ymin_apmoy, ind, boolean_cross_mean)
        # 
        # 
        # On les sauvegarde les caractéristiques des deux parties du segment en cours dans la variable id_segments : 
        id_segments <- rbind(id_segments, id_segment_avmoy, id_segment_apmoy)
        # 
        # 
      } 
      # 
      # 
    }
    # On convertit en data.frame
    id_segments <- as.data.frame(id_segments)
    rownames(id_segments) <- NULL
    # 
    # 
    # On rajoute la numérotation des segments : 
    id_segments$segment_no <- 1:nrow(id_segments)
    # 
    # 
    # On rajoute les caractéristiques des segments de l'id en cours dans la variable contenant tous les id : 
    group_segments <- rbind.data.frame(group_segments, id_segments)
    # 
    # 
  }
  # 
  # 
  # On renomme les colonnes : 
  colnames(group_segments) <- c("id", "t1", "t2", "diff_t", "y1", "y2", "diff_y", 
                                "slope", "intercept", "auc_to_mean", "auc_to_min", 
                                "ind", "mean_inter", "segment_nb")
  # [ Attention : auc_to_mean     = aire avec pour base la moyenne de l'id (ligne 42)
  #               et auc_2min   = aire avec pour base la valeur minimal de l'id (ligne 45)
  #               et mean_inter = variable qui permet de localiser les segments créés ]
  # et on supprime les noms des lignes :
  rownames(group_segments) <- NULL #ça remet la numérotation
  # 
  # 
  # 
  # On supprime la colonne ind si on ne coupe pas les segments avec la moyenne : 
  if (!cut_at_mean) group_segments[ , colnames(group_segments) == "ind"] <- NULL
  # 
  # On supprime la colonne segment_no si on coupe avec les segments avec la moyenne : 
  if (cut_at_mean) group_segments[ , colnames(group_segments) == "segment_nb"] <- NULL
  # et on supprime la colonne mean_inter aussi : 
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
