#' Get orders from SellerActive
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @param sku a single product SKU (character/scalar)
#' @importFrom magrittr "%>%"
#' @export
sa_get_orders <- function(start_date = Sys.Date() - 1, end_date = Sys.Date() - 1, sku = NULL) {
  x <- sa_api(endpoint = "Order",
    req.sKU = sku,
    req.dateOrderedFrom = paste0(start_date, "T05:00:00"),
    req.dateOrderedTo = paste0(end_date + 1, "T04:59:59")
    )
  sa_parse_response(x) %>%
    dplyr::mutate_at(c("DateOrdered", "LatestShipDate", "LatestDeliveryDate"), sa_parse_datetime)
}