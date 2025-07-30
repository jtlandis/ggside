#' @title Extending base ggproto classes for ggside
#' @name ggside-ggproto-geoms
#' @description
#' These ggproto classes are slightly modified from their
#' respective inherited \link[ggplot2]{ggproto} class. The
#' biggest difference is exposing 'x/yfill', 'x/ycolour', and
#' 'x/ycolor' as viable aesthetic mappings.
#'
#'
#' @param data data passed internally
#' @param params params available to ggproto object
#' @return ggproto object that is usually passed to \link[ggplot2]{layer}
NULL


#' @title Extending base ggproto classes for ggside
#' @name ggside-ggproto-facets
#' @section Extended Facets:
#'
#' The following is a list \link{ggplot2} facets that are
#' available to use by ggside base.
#'
#'  \itemize{
#' \item \link[ggplot2]{FacetNull} -> FacetSideNull
#' \item \link[ggplot2]{FacetGrid} -> FacetSideGrid
#' \item \link[ggplot2]{FacetWrap} -> FacetSideWrap
#' }
#' @return ggproto object that can be added to a ggplot object
NULL
