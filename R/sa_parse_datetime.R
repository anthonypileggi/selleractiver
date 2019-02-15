#' Parse SellerActive datetime
sa_parse_datetime <- function(x) {
  if (stringr::str_detect(x, "T")[1]) {
    x <- as.POSIXct(strptime(x, "%Y-%m-%dT%H:%M:%S"), tz = "GMT")
  } else {
    x <- as.POSIXct(strptime(x, "%Y-%m-%d %H:%M:%S"), tz = "GMT")
  }
  lubridate::with_tz(x, tzone = Sys.timezone())
}