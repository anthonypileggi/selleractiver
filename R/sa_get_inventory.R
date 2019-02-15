#' Get inventory from SellerActive
#' @param skus a single product SKU (character/scalar)
#' @export
sa_get_inventory <- function(sku = NULL) {
  x <- sa_api(endpoint = "Inventory", req.sKU = sku)
  sa_parse_response(x)
}