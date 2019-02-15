#' Parse a nested list returned by the API as a tibble
#' @param response a nested list
#' @export
sa_parse_response <- function(response) {

  purrr::map_df(
    response,
    function(r) {
      r[purrr::map_lgl(r, ~is.null(.x))] <- NA
      vars <- names(r)[purrr::map_lgl(r, ~length(.x) == 1)]        # TODO: this ignores entries with > 1 length (e.g., multiple suppliers)
      tmp <- tibble::as_tibble(r[vars])

      # parse sub-list items
      for (v in c("ProductSites", "OrderDetails")) {
        if (v %in% names(r)) {
          tmp[[v]] <- list(purrr::map_df(
            r[[v]],
            function(x) {
              x[purrr::map_lgl(x, is.null)] <- NA
              dplyr::as_tibble(x)
            }))
        }
      }

      tmp
    }
  )

}