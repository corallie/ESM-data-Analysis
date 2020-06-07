#' Title
#'
#' @param data 
#' @param format 
#'
#' @return
#' @export
#' 
#' @author Coralie Vennin, \email{??}
#' @author Pauline Mialhe, \email{pauline.mialhe@@univ-reunion.fr}
#'
#' @examples
do_hr <- function(data, format = "%Y-%m-%d %H:%M:%S") {
  # 
  # 
  # Get unique id :
  u_ids <- unique(data$id)
  # 
  # 
  # Apply for each id : 
  list_hr <- lapply(u_ids, function(i_id) {
    # 
    # 
    # Get data of i_id :
    data_id <- data[data$id == i_id, ]
    # 
    # 
    # Get datetime in character : 
    datetime <- as.character(data_id$time)
    # 
    # 
    # Convert it to POSIX
    datetime_POSIX <- as.numeric(as.POSIXct(strptime(datetime, format)))
    # 
    # 
    # Extract the value of the datetime of the first measure : 
    diff_datetime <- datetime_POSIX - datetime_POSIX[1]
    # 
    # 
    # Convert second to hour :
    hr_datetime <- c(datetime_POSIX - datetime_POSIX[1])/3600
    # 
    # 
    # Apply output :
    return(hr_datetime)
    # 
    # 
  })
  # 
  # 
  # Get all values of hr : 
  list_hr <- do.call(c, list_hr)
  # 
  # 
  # 
  # Function output : 
  return(list_hr)
  # 
  # 
}