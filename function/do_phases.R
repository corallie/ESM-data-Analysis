#' Title
#'
#' @param data 
#' @param segments 
#'
#' @return
#' @export
#' 
#' @author Coralie Vennin, \email{??}
#' @author Pauline Mialhe, \email{pauline.mialhe@@univ-reunion.fr}
#'
#' @examples
do_phases <- function(data, segments) {
  # 
  # 
  # On vérifie les colnames dans data : 
  if (!all(c('id', 'hr', 'NegA') %in% colnames(data))) 
    stop('data pas dans le bon format')
  # 
  # 
  # On sélectionne les colonnes utiles : 
  data <- data[ , c('id', 'hr', 'NegA')]
  # 
  # 
  # 
  # On vérifie les colnames dans segments : 
  if (!all(c('id', 
             't1', 't2', 'diff_t', 
             'y1', 'y2', 'diff_y', 
             'pente', 'ord_origine', 
             'aire_2mean', 'aire_2min', 
             'mean_inter', 'segment_nb') %in% colnames(segments))) 
    stop('segments pas dans le bon format')
  # 
  # 
  # 
  # La liste des id :
  list_id <- unique(segments$id)
  # 
  # 
  # 
  # On initialise un objet qui contiendra les informations des phases du group :
  group_phases <- NULL
  # 
  # 
  # 
  # id=8 #test lapply ====
  group_phases_list <- lapply(list_id, function(id) {
    # 
    # 
    # 
    # On initialise un objet qui contiendra les informations des phases de l'id :
    id_phases <- NULL
    # 
    # 
    # 
    # On récupère les données de l'id en cours : 
    id_data     <- data[data$id == id, ]
    id_segments <- segments[segments$id == id, ]
    id_min      <- min(id_data$NegA)
    # -1: pour supprimer la colonne contenant la valeur de l'id, on ne garde que 
    # les colonnes minr et NegA
    # 
    # 
    # On réinitialise les noms de lignes pour qu'elles soient numérotées de 1 à nrow(id_segments)
    rownames(id_segments) <- NULL
    # 
    # 
    # 
    # POUR LES PLATS : ====
    # 
    boolean_plat_at_1 <- c(id_segments$pente == 0 & id_segments$ord_origine == id_min)*1
    rle_plat_at_1 <- rle(boolean_plat_at_1)
    # 
    # 
    seqs_ind_fins <- cumsum(rle_plat_at_1$lengths)[rle_plat_at_1$values == 1]
    seqs_lenghts <- rle_plat_at_1$lengths[rle_plat_at_1$values == 1]
    # 
    # 
    # 
    # i <- 3 # test lapply
    list_of_plat <- lapply(1:length(seqs_ind_fins), function(i) {
      i_deb <- seqs_ind_fins[i]
      i_lgh <- seqs_lenghts[i]
      if (i_lgh == 1) return(id_segments[i_deb, ])
      return(id_segments[c(i_deb-i_lgh+1):i_deb, ])
    })
    # 
    # 
    # 
    df_of_plat_caract <- do.call(rbind.data.frame, lapply(1:length(list_of_plat), function(ind_iplat) {
      # 
      iplat <- list_of_plat[[ind_iplat]]
      # 
      i_t_deb  <- min(iplat$t1)
      i_t_fin  <- max(iplat$t2)
      i_diff_t <- i_t_fin - i_t_deb
      i_y_deb  <- iplat$y1[1]
      i_y_fin  <- rev(iplat$y2)[1]
      i_diff_y <- i_y_fin-i_y_deb
      #
      i_df_phase <- data.frame(id         = id, 
                               t1         = i_t_deb,
                               tn         = i_t_fin,
                               diff_t     = i_diff_t,
                               y1         = i_y_deb,
                               yn         = i_y_fin,
                               diff_y     = i_diff_y,
                               pente_e    = i_diff_y/i_diff_t,
                               pente_mean = mean(iplat$pente),
                               pente_sd   = sd(iplat$pente),
                               pente_max  = max(iplat$pente),
                               pente_min  = min(iplat$pente),
                               aire_2mean = sum(iplat$aire_2mean),
                               aire_2min  = sum(iplat$aire_2min),
                               cat        = 0)
    }))
    # 
    # 
    # 
    # POUR LES ASC. : ====
    # 
    boolean_asc <- c((id_segments$pente >= 0 & id_segments$ord_origine != id_min) | 
                       (id_segments$pente > 0 & id_segments$ord_origine == id_min & id_segments$t1 == 0))*1
    rle_asc <- rle(boolean_asc)
    # 
    # 
    seqs_ind_fins <- cumsum(rle_asc$lengths)[rle_asc$values == 1]
    seqs_lenghts <- rle_asc$lengths[rle_asc$values == 1]
    # 
    # 
    # 
    # i <- 5 # test lapply
    list_of_asc <- lapply(1:length(seqs_ind_fins), function(i) {
      i_deb <- seqs_ind_fins[i]
      i_lgh <- seqs_lenghts[i]
      if (i_lgh == 1) {
        if (id_segments$pente[i_deb] == 0) return(NULL)
        return(id_segments[i_deb, ])
      } else {
        if (all(id_segments$pente[c(i_deb-i_lgh+1):i_deb] == 0)) return(NULL)
        return(id_segments[c(i_deb-i_lgh+1):i_deb, ])
      }
    })
    # 
    # 
    # 
    df_of_asc_caract <- do.call(rbind.data.frame, lapply(1:length(list_of_asc), function(ind_iasc) {
      # 
      iasc <- list_of_asc[[ind_iasc]]
      # 
      if(is.null(iasc)) return(NULL)
      # 
      i_t_deb  <- min(iasc$t1)
      i_t_fin  <- max(iasc$t2)
      i_diff_t <- i_t_fin - i_t_deb
      i_y_deb  <- iasc$y1[1]
      i_y_fin  <- rev(iasc$y2)[1]
      i_diff_y <- i_y_fin-i_y_deb
      #
      i_df_phase <- data.frame(id         = id, 
                               t1         = i_t_deb,
                               tn         = i_t_fin,
                               diff_t     = i_diff_t,
                               y1         = i_y_deb,
                               yn         = i_y_fin,
                               diff_y     = i_diff_y,
                               pente_e    = i_diff_y/i_diff_t,
                               pente_mean = mean(iasc$pente),
                               pente_sd   = sd(iasc$pente),
                               pente_max  = max(iasc$pente),
                               pente_min  = min(iasc$pente),
                               aire_2mean = sum(iasc$aire_2mean),
                               aire_2min  = sum(iasc$aire_2min),
                               cat        = 1)
    }))
    # 
    # 
    # 
    # POUR LES DESC. : ====
    # 
    boolean_desc <- c((id_segments$pente <= 0 & id_segments$ord_origine != id_min) | 
                        (id_segments$pente < 0 & id_segments$ord_origine == id_min & id_segments$t1 == 0))*1
    
    rle_desc <- rle(boolean_desc)
    # 
    # 
    seqs_ind_fins <- cumsum(rle_desc$lengths)[rle_desc$values == 1]
    seqs_lenghts <- rle_desc$lengths[rle_desc$values == 1]
    # 
    # 
    # 
    # i <- 3 # test lapply
    list_of_desc <- lapply(1:length(seqs_ind_fins), function(i) {
      i_deb <- seqs_ind_fins[i]
      i_lgh <- seqs_lenghts[i]
      if (i_lgh == 1) {
        if (id_segments$pente[i_deb] == 0) return(NULL)
        return(id_segments[i_deb, ])
      } else {
        if (all(id_segments$pente[c(i_deb-i_lgh+1):i_deb] == 0)) return(NULL)
        return(id_segments[c(i_deb-i_lgh+1):i_deb, ])
      }
    })
    # 
    # 
    # 
    df_of_desc_caract <- do.call(rbind.data.frame, lapply(1:length(list_of_desc), function(ind_idesc) {
      # 
      idesc <- list_of_desc[[ind_idesc]]
      # 
      if(is.null(idesc)) return(NULL)
      # 
      # 
      list_seg_asc <- unlist(lapply(list_of_asc, function(i) if(!is.null(i)) i$segment_nb))
      test_if_not_in_asc <- idesc$segment_nb %in% list_seg_asc
      if (any(test_if_not_in_asc)) idesc <- idesc[!test_if_not_in_asc, ]
      # 
      # 
      i_t_deb  <- min(idesc$t1)
      i_t_fin  <- max(idesc$t2)
      i_diff_t <- i_t_fin - i_t_deb
      i_y_deb  <- idesc$y1[1]
      i_y_fin  <- rev(idesc$y2)[1]
      i_diff_y <- i_y_fin-i_y_deb
      #
      i_df_phase <- data.frame(id         = id, 
                               t1         = i_t_deb,
                               tn         = i_t_fin,
                               diff_t     = i_diff_t,
                               y1         = i_y_deb,
                               yn         = i_y_fin,
                               diff_y     = i_diff_y,
                               pente_e    = i_diff_y/i_diff_t,
                               pente_mean = mean(idesc$pente),
                               pente_sd   = sd(idesc$pente),
                               pente_max  = max(idesc$pente),
                               pente_min  = min(idesc$pente),
                               aire_2mean = sum(idesc$aire_2mean),
                               aire_2min  = sum(idesc$aire_2min),
                               cat        = -1)
    }))
    # 
    # On regroupe tout dans le même objet : 
    id_phases <- rbind(df_of_plat_caract, df_of_asc_caract, df_of_desc_caract)
    rownames(id_phases) <- NULL
    # 
    # 
    # On réordonne le temps : 
    id_phases <- id_phases[order(id_phases$t1), ]
    rownames(id_phases) <- NULL
    # 
    # 
    # On rajoute la numérotation des phases : 
    id_phases$phase_nb <- 1:nrow(id_phases)
    # 
    # 
    return(id_phases)
    # 
  })
  # 
  # 
  # On regroupe le tout : 
  group_phases <- do.call(rbind.data.frame, group_phases_list)
  rownames(group_phases) <- NULL
  # 
  # 
  return(group_phases)
  # 
}
# 
# 
# 
# 
# 