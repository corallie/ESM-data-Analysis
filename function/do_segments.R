#' Création des segments par id
#'
#' @param data 
#' @param cut_at_mean 
#'
#' @return
#' @export
#' 
#' @author Coralie Vennin, \email{??}
#' @author Pauline Mialhe, \email{pauline.mialhe@@univ-reunion.fr}
#'
#' @examples
do_segments <- function(data, cut_at_mean = T) {
  # 
  # 
  # On vérifie les colnames dans data : 
  if (!all(c('id', 'hr', 'NegA') %in% colnames(data))) 
    stop('data should have columns : id, hr and NegA')
  # 
  # 
  # On sélectionne les colonnes : 
  data <- data[ , c('id', 'hr', 'NegA')]
  # 
  # 
  # La liste des id :
  list_id <- unique(data$id)
  # 
  # 
  # On initialise un objet qui contiendra les informations des segments du group :
  group_segments <- NULL
  # 
  # 
  # id = 1 #test for
  for (id in list_id) {# Pour chacun des id,
    # 
    # 
    # On récupère les données de l'id en cours : 
    id_data     <- data[data$id == id, -1]
    # -1: pour supprimer la colonne contenant la valeur de l'id, on ne garde que 
    # les colonnes hr et NegA
    # 
    # 
    # On calcule sa moyenne et son écart-type : 
    id_mean <- mean(id_data$NegA)
    id_sd <- sd(id_data$NegA)
    # mais aussi, la valeur minimale :
    id_min <- min(id_data$NegA)
    # 
    # 
    # On calcule le nombre de segment : 
    nb_segment <- nrow(id_data)-1
    # 
    # 
    # On initialise un objet qui contiendra les informations des segments de l'id
    id_segments <- NULL
    # 
    # 
    # On initialise une variable qui nous permettra de faire les calculs des cycles : 
    ind <- 1
    # (ne fonctionne quand dans le cas no_cut_segments = TRUE)
    # 
    # 
    # 
    for (i_segment in 1:nb_segment) {# Pour chacun des segments, 
      # 
      # Le début et la fin du segment : 
      t1 <- id_data[i_segment, 1]
      t2 <- id_data[i_segment+1, 1]
      # et la différence de temps écoulé : 
      diff_t <- t2 - t1
      # 
      # Les valeurs du suivi associées : 
      y1 <- id_data[i_segment, 2]
      y2 <- id_data[i_segment+1, 2]
      # et la différence des deux valeurs : 
      diff_y <- y2 - y1
      # 
      # 
      # La slope est : 
      slope <- diff_y / diff_t
      # et l'ordonnée à l'origine est :
      intercept <- (t2*y1 - t1*y2) / diff_t
      # 
      # 
      # Pour le calcul des cycles, on va devoir découper les segments en deux 
      # s'ils traversent la droite de la moyenne de l'individu. 
      # Donc on doit savoir pour le segment en cours : 
      # Est-ce que le segment traverse la droite de la moyenne de l'individu ? 
      intersection_avec_moyenne <- (y2 > id_mean) - (y1 > id_mean)
      # > intersection_avec_moyenne = 0 si le segment ne traverse pas la moyenne
      # > intersection_avec_moyenne != 0 si le segment traverse la moyenne
      # 
      # 
      # 
      if (!cut_at_mean | intersection_avec_moyenne == 0) { #Si le segment ne traverse pas la moyenne
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
        id_segment <- c(id, t1, t2, diff_t, y1, y2, diff_y, slope, intercept, aire_slc, aire_ymin, ind, intersection_avec_moyenne)
        # 
        # 
        # On les sauvegarde les caractéristiques du segment en cours dans la variable id_segments : 
        id_segments <- rbind(id_segments, id_segment)
        # 
        # 
      } else if (cut_at_mean & intersection_avec_moyenne != 0) { #Si le segment traverse la moyenne
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
        id_segment_avmoy <- c(id, t1, t, diff_t_avmoy, y1, y, diff_y_avmoy, slope, intercept, aire_slc_avmoy, aire_ymin_avmoy, ind, intersection_avec_moyenne)
        # 2) la partie après la moyenne (on entre dans un nouveau cycle) : 
        ind <- ind + 1
        id_segment_apmoy <- c(id, t, t2, diff_t_apmoy, y, y2, diff_y_apmoy, slope, intercept, aire_slc_apmoy, aire_ymin_apmoy, ind, intersection_avec_moyenne)
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
  # 
  return(group_segments)
  # 
}
# 
# 
# 
# 
#' ____________________________________________________________________________