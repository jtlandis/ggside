

#' @export
ggside_facet <- function(facet, ggside) UseMethod("ggside_facet", facet)

ggside_facet.default <- function(facet, ggside) {
  abort(
    sprintf("No %s() method for object of class <%s>", "ggside_facet", class(facet)[1]),
  )
}
