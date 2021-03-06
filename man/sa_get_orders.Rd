% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sa_get_orders.R
\name{sa_get_orders}
\alias{sa_get_orders}
\title{Get orders from SellerActive}
\usage{
sa_get_orders(start_date = Sys.Date() - 1, end_date = Sys.Date() - 1,
  sku = NULL)
}
\arguments{
\item{start_date}{first day of data (Date/scalar)}

\item{end_date}{last day of data (Date/scalar)}

\item{sku}{a single product SKU (character/scalar)}
}
\description{
Get orders from SellerActive
}
