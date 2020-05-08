#' Title
#'
#' @param segments_cut 
#' @param type 
#'
#' @return
#' @export
#' 
#' @author Coralie Vennin, \email{??}
#' @author Pauline Mialhe, \email{pauline.mialhe@@univ-reunion.fr}
#'
#' @examples
do_cycles <- function(segments_cut) {
  # 
  # 
  # On vérifie les colnames dans segments_cut : 
  if (!all(c('id', 
             't1', 't2', 'diff_t', 
             'y1', 'y2', 'diff_y', 
             'pente', 'ord_origine', 
             'aire_2mean', 'aire_2min', 
             'cycle') %in% colnames(segments_cut)))
    stop('segments_cut pas dans le bon format')
  # 
  # 
  # 
  # La liste des id :
  list_id <- unique(segments_cut$id)
  # 
  # 
  # 
  # On initialise un objet qui contiendra les informations des cycles du group :
  group_cycles <- NULL
  # 
  # 
  # 
  # id=1 #test for ====
  for (id in list_id) {
    # 
    # 
    # 
    # On récupère les données de l'id en cours : 
    id_segments_cut <- segments_cut[segments_cut$id == id, -1]
    # -1: pour supprimer la colonne contenant la valeur de l'id, on ne garde que 
    # les colonnes minr et NegA
    # 
    # 
    # On comptabilise le nombre de cycle : 
    nb_cycle <- max(id_segments_cut$cycle)
    #   
    # 
    # 
    # On initialise un objet qui contiendra les informations des cycles de l'id en cours :
    id_cycles <- NULL
    # 
    # 
    # 
    # icycle=1 # test for ====
    for (icycle in 1:nb_cycle) {
      #
      # On récupère les caractéristiques des segments pour le cycle en cours : 
      id_icycle_segs <- id_segments_cut[id_segments_cut$cycle == icycle, ]
      # 
      # 
      # Le nombre segment dans le cycle est : 
      seg_number <- nrow(id_icycle_segs)
      # 
      # 
      # On reconstruit les données de l'id pour le cycle en cours : 
      id_data <- data.frame(t = c(id_icycle_segs$t1, id_icycle_segs$t2[seg_number]), # les minutes des mesures 
                            y = c(id_icycle_segs$y1, id_icycle_segs$y2[seg_number])) # les valeurs des mesures 
      # 
      # 
      # Toutes les variables que l'on veut :
      t_debut   <- min(id_data$t) # le temps de début de cycle
      t_fin     <- max(id_data$t) # le temps de fin de cycle
      duree     <- t_fin - t_debut # la durée du cycle
      y_min     <- min(id_data$y) # le minimum du cycle
      y_max     <- max(id_data$y) # le maximum du cycle
      amplitude <- y_max - y_min # l'amplitude du cycle
      #
      #
      # PM : ligne suivante à revoir ?! 
      # Print if erreur sur signe dans id_icycle_segs$aire
      u_signes <- unique(sign(id_icycle_segs$aire_2mean))
      if (length(u_signes) > 1 & all(u_signes != 0)) {
        print(paste("ID", id, ":", "cycle n°", icycle))
      }
      # 
      # 
      #
      aire_2mean  <- sum(id_icycle_segs$aire_2mean)
      aire_2min   <- sum(id_icycle_segs$aire_2min)
      pente_debut <- id_icycle_segs$pente[1]
      pente_fin   <- id_icycle_segs$pente[nrow(id_icycle_segs)]
      # 
      #
      # Si l'aire est positive :
      if (aire_2mean > 0) {
        type   <- 1
        t_ypic <- id_data$t[id_data$y == y_max]
        
        # S'il y a un pic max pour le cycle :
        if (length(t_ypic) == 1) {
          pente_debutM <- (y_max - id_data$y[1])/(t_ypic - id_icycle_segs$t1[1])
          pente_finM   <- (id_data$y[nrow(id_data)] - y_max)/(id_icycle_segs$t2[nrow(id_icycle_segs)] - t_ypic)
        }
        #
        # S'il y a plusieurs pic max pour le cycle :
        if (length(t_ypic) != 1) {
          pente_debutM <- (y_max - id_data$y[1])/(t_ypic[1] - id_icycle_segs$t1[1])
          pente_finM   <- (id_data$y[nrow(id_data)] - y_max)/(id_icycle_segs$t2[nrow(id_icycle_segs)] - t_ypic[length(t_ypic)])
        }
      }
      #
      # Si l'aire est négative :
      if (aire_2mean < 0) {
        type   <- -1
        t_ypic <- id_data$t[id_data$y == y_min]
        
        # S'il y a un pic min pour le cycle :
        if (length(t_ypic) == 1) {
          pente_debutM <- (y_min - id_data$y[1])/(t_ypic - id_data$t[1])
          pente_finM   <- (id_data$y[nrow(id_data)] - y_min)/(id_data$t[nrow(id_data)] - t_ypic)
        }
        
        # S'il y a plusieurs pic min pour le cycle :
        if (length(t_ypic) != 1) {
          pente_debutM <- (y_min - id_data$y[1])/(t_ypic[1] - id_data$t[1])
          pente_finM   <- (id_data$y[nrow(id_data)] - y_min)/(id_data$t[nrow(id_data)] - t_ypic[length(t_ypic)])
        }
      }
      #
      id_icycle <- c(id, type, t_debut, t_fin, duree, y_min, y_max, amplitude, pente_debut, pente_debutM, pente_fin, pente_finM, aire_2mean, aire_2min)
      #
      id_cycles <- rbind.data.frame(id_cycles, id_icycle)
      #
    }
    colnames(id_cycles) <- c("id", "type", "t_debut", "t_fin", "duree", "y_min", "y_max", "amplitude", "pente_debut", "pente_debutM", "pente_fin", "pente_finM", "aire_2mean", "aire_2min")
    rownames(id_cycles) <- NULL
    # 
    # 
    id_cycles <- id_cycles[id_cycles$type == 1, colnames(id_cycles) != "type"]
    # 
    # 
    # On rajoute la numérotation des phases : 
    rownames(id_cycles) <- NULL
    id_cycles$cycle_nb <- 1:nrow(id_cycles)
    # 
    #
    #
    group_cycles <- rbind.data.frame(group_cycles, id_cycles)
  }
  # 
  # 
  # 
  # 
  # et on sauvegarde : 
  return(group_cycles)
  # 
}
# 