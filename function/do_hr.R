#' Compute ellapsed hours by id
#' 
#' @param data a data.frame object describing the id x time of the ESM data. 
#' @param format a character describing the time format in the ESM data (see strptime details). 
#'
#' @return a vector whose elements are the ellasped hours by id of the ESM data described by at least two columns : id and time. 
#' @export
#' 
#' @author Coralie Vennin, \email{coralie.vennin@@gmail.com}
#' @author Pauline Mialhe, \email{pauline.mialhe@@univ-reunion.fr}
#'
#' @examples
do_hr <- function(data, format = "%Y-%m-%d %H:%M:%S") {
  # 
  # 
  # Get unique id
  u_ids <- unique(data$id)
  # 
  # 
  # Apply for each id
  list_hr <- lapply(u_ids, function(i_id) {
    # 
    # 
    # Get data of i_id
    data_id <- data[data$id == i_id, ]
    # 
    # 
    # Get datetime in character
    datetime <- as.character(data_id$time)
    # 
    # 
    # Convert it to POSIX
    datetime_POSIX <- as.numeric(as.POSIXct(strptime(datetime, format)))
    # 
    # 
    # Extract the value of the datetime of the first measure
    diff_datetime <- datetime_POSIX - datetime_POSIX[1]
    # 
    # 
    # Convert second to hour
    hr_datetime <- c(datetime_POSIX - datetime_POSIX[1])/3600
    # 
    # 
    # Apply output
    return(hr_datetime)
    # 
    # 
  })
  # 
  # 
  # Get all values of hr
  list_hr <- do.call(c, list_hr)
  # 
  # 
  # 
  # Function output
  return(list_hr)
  # 
  # 
}
