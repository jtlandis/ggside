deprecated_fun <- function(fun) {
  fun_sub <- substitute(fun)
  function(...) {
    .Deprecated(deparse1(fun_sub), package = "ggside")
    fun(...)
  }
}


#' @name ggside-deprecated
#' @title Deprecated Functions
#'
#' @description
#' The following functions have been deprecated.
#'
#' as_ggsideFacet <- [ggside_facet]
#' as_ggsideCoord <- [ggside_coord]
#' is.ggside <- [is_ggside]
#' @aliases as_ggsideFacet
#'

#' @rdname ggside-deprecated
#' @usage NULL
#' @export
as_ggsideFacet <- deprecated_fun(ggside_facet)

#' @rdname ggside-deprecated
#' @usage NULL
#' @export
as_ggsideCoord <- deprecated_fun(ggside_coord)


#' @rdname ggside-deprecated
#' @usage NULL
#' @export
is.ggside <- deprecated_fun(is_ggside)
