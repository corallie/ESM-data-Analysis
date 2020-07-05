#' Title
#'
#' @param data 
#'
#' @return
#' @export
#' 
#' @author Coralie Vennin, \email{coralie.vennin@@gmail.com}
#' @author Pauline Mialhe, \email{pauline.mialhe@@univ-reunion.fr}
#'
#' @examples
do_cycles <- function(data = data) {
  # 
  # 
  # On doit découper les segments traversent la moyenne de chaque id : 
  segments_cut <- do_segments(data = data, cut_at_mean = TRUE)
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
    nb_cycle <- max(id_segments_cut$ind)
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
      id_icycle_segs <- id_segments_cut[id_segments_cut$ind == icycle, ]
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
      # Toues les variables que l'on veut :
      t1        <- min(id_data$t) # le temps de début de cycle
      tn        <- max(id_data$t) # le temps de fin de cycle
      duration  <- tn - t1 # la durée du cycle
      y_min     <- min(id_data$y) # le minimum du cycle
      y_max     <- max(id_data$y) # le maximum du cycle
      amplitude <- y_max - y_min # l'amplitude du cycle
      #
      #
      # PM : ligne suivante à revoir ?! 
      # Print if erreur sur signe dans id_icycle_segs$aire
      u_signes <- unique(sign(id_icycle_segs$auc_to_mean))
      if (length(u_signes) > 1 & all(u_signes != 0)) {
        print(paste("ID", id, ":", "cycle n°", icycle))
      }
      # 
      # 
      #
      auc_to_mean <- sum(id_icycle_segs$auc_to_mean)
      auc_to_min  <- sum(id_icycle_segs$auc_to_min)
      slope_debut <- id_icycle_segs$slope[1]
      slope_fin   <- id_icycle_segs$slope[nrow(id_icycle_segs)]
      # 
      #
      # Si l'aire est positive :
      if (auc_to_mean > 0) {
        type   <- 1
      }
      #
      # Si l'aire est négative :
      if (auc_to_mean < 0) {
        type   <- -1
      }
      #
      id_icycle <- c(id, type, 
                     t1, tn, duration, 
                     y_min, y_max, amplitude,
                     auc_to_mean)
      #
      id_cycles <- rbind.data.frame(id_cycles, id_icycle)
      #
    }
    colnames(id_cycles) <- c("id", "type", 
                             "t1", "tn", "duration", 
                             "y_min", "y_max", "amplitude", 
                             "auc_to_mean")
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
  # Function output : 
  return(group_cycles)
  # 
  # 
}
