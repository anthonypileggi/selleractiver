#' Make a SellerActive API call
#' @export
sa_api <- function(...) {
  pg <- 1
  x <- NULL
  while (pg == 1 || length(x_new) > 0) {
    message("Getting page ", pg, " of API data.")
    x_new <- sa_api_call(..., req.page = pg, req.size = 100)
    x <- c(x, x_new)
    pg <- pg + 1
  }
  return(x)
}

#' Make a SellerActive API call
#' @param endpoint api endpoint
#' @param ... additional named parameters
#' @export
sa_api_call <- function(endpoint = NULL, ...) {

  if (is.null(endpoint))
    stop("You must specify an endpoint for the SellerActive API!", call. = FALSE)

  # call api
  url <-
    httr::modify_url(
      url = "https://rest.selleractive.com:443",
      path = paste0("api/", endpoint),
      query = list(...)
    )
  response <-
    httr::GET(
      url,
      httr::add_headers(Authorization = paste("Basic", Sys.getenv("SELLERACTIVE_AUTH_KEY"))),
      encode = "json"
    )

  # check status code
  if (httr::status_code(response) != 200) {
    stop(
      sprintf(
        "SellerActive API request failed [%s]",
        httr::status_code(response)
      ),
      call. = FALSE
    )
  }

  #response
  httr::content(response)
}
