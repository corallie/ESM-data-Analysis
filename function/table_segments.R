#' Segments summary by id
#'
#' @param segments a data.frame object of the segments of the ESM data.
#' @param save_at the path where to save the table (path + file name.csv)
#'
#' @return
#' @export
#' 
#' @author Coralie Vennin, \email{coralie.vennin@@gmail.com}
#' @author Pauline Mialhe, \email{pauline.mialhe@@univ-reunion.fr}
#'
#' @examples
table_segments <- function(segments, save_at = NULL) {
  # 
  # 
  # Features of all slopes
  slope_mean <- tapply(segments$slope, segments$id, mean)
  slope_max  <- tapply(segments$slope, segments$id, max)
  slope_min  <- tapply(segments$slope, segments$id, min)
  # 
  # 
  # Features of positive slopes
  segments_asc   <- segments[segments$slope > 0, ]
  slope_mean_asc <- tapply(segments_asc$slope, segments_asc$id, mean)
  slope_max_asc  <- tapply(segments_asc$slope, segments_asc$id, max)
  slope_min_asc  <- tapply(segments_asc$slope, segments_asc$id, min)
  # 
  # 
  # Features of negative slopes
  segments_desc   <- segments[segments$slope < 0, ]
  slope_mean_desc <- tapply(segments_desc$slope, segments_desc$id, mean)
  slope_max_desc  <- tapply(segments_desc$slope, segments_desc$id, max)
  slope_min_desc  <- tapply(segments_desc$slope, segments_desc$id, min)
  # 
  # 
  # Group all features together
  segments_features <- rbind(slope_mean, slope_max, slope_min, 
                             slope_mean_asc, slope_max_asc, slope_min_asc, 
                             slope_mean_desc, slope_max_desc, slope_min_desc)
  # 
  # 
  # Add id in colnames
  colnames(segments_features) <- paste0("id_", colnames(segments_features))
  # 
  # 
  # Function output : 
  return(segments_features)
  # 
  # 
}