#' sqrm_to_hect converts square meters to hectares.
#'
#' @export
sqrm_to_hect <- function(x) {
  x*0.0001
}

#' lima_to_hect converts square meters to hectares.
#'
#' @export
lima_to_hect <- function(x) {
  x*0.25
}

#' acre_to_hect converts square meters to hectares.
#'
#' @export
acre_to_hect <- function(x) {
  x*0.40
}

#' oxcart_to_kg converts oxcarts to kg, based on Lauren Persha's estimates. This is likely very
#'  imprecise.
#'
#' @export
oxcart_to_kg <- function(x) {
  x*350
}

#' tonne_to_kg converts tonnes to kg.
#'
#' @export
tonne_to_kg <- function(x) {
  x*1000
}
