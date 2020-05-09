#' Title
#'
#' @param segments 
#' @param save_at 
#'
#' @return
#' @export
#'
#' @examples
table_segments <- function(segments, save_at = NULL) {
  # 
  # 
  # Caractéristiques de toutes les slopes :
  slope_mean <- tapply(segments$slope, segments$id, mean)
  slope_max  <- tapply(segments$slope, segments$id, max)
  slope_min  <- tapply(segments$slope, segments$id, min)
  # 
  # 
  # Caractéristiques des slopes ascendantes :
  segments_asc   <- segments[segments$slope > 0, ]
  slope_mean_asc <- tapply(segments_asc$slope, segments_asc$id, mean)
  slope_max_asc  <- tapply(segments_asc$slope, segments_asc$id, max)
  slope_min_asc  <- tapply(segments_asc$slope, segments_asc$id, min)
  # 
  # 
  # Caractéristiques des slopes descendantes :
  segments_desc   <- segments[segments$slope < 0, ]
  slope_mean_desc <- tapply(segments_desc$slope, segments_desc$id, mean)
  slope_max_desc  <- tapply(segments_desc$slope, segments_desc$id, max)
  slope_min_desc  <- tapply(segments_desc$slope, segments_desc$id, min)
  # 
  # 
  # On regroupe le tout : 
  segments_features <- rbind(slope_mean, slope_max, slope_min, 
                             slope_mean_asc, slope_max_asc, slope_min_asc, 
                             slope_mean_desc, slope_max_desc, slope_min_desc)
  # 
  # 
  # On rajoute id dans le nom de colonne pour que ça soit bien explicite : 
  colnames(segments_features) <- paste0("id_", colnames(segments_features))
  # 
  # 
  # Function output : 
  return(segments_features)
  # 
  # 
}