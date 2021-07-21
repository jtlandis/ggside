
validate_ggside <- function(e2, object) UseMethod('validate_ggside')
validate_ggside.default <- function(e2, object) object
validate_ggside.Facet <- function(e2, object){
  object[['facet']] <- as_ggsideFacet(object[['facet']], object[['ggside']])
  object
}
validate_ggside.Coord <- function(e2, object) {
  if(inherits(e2, "CoordFlip")||inherits(e2, "CoordPolar")){
    abort("ggside is not currently compatable with CoordFlip or CoordPolar")
  }
  object
}

as_ggside <- function(x, ...) UseMethod('as_ggside')
as_ggside.default <- function(x, ...) abort(glue("No as_ggside() method for class <", glue_collapse(class(x), sep = "/"),">"))
as_ggside.ggplot <- function(x, ggside = NULL, ...) {
  if(inherits(x[['coordinates']], "CoordFlip")||inherits(x[['coordinates']], "CoordPolar")){
    abort("ggside is not currently compatable with CoordFlip or CoordPolar")
  }
  ggside <- ggside %||% ggside()
  if(!is.ggside_options(ggside)) stop("argument ggside must be of class `ggside_options` or NULL")
  class(x) <- c("ggside", class(x))
  x[['ggside']] <- ggside
  update_ggside(x)
}
as_ggside.ggside <- function(x, ggside = NULL, ...) {
  ggside <- ggside %||% x[['ggside']] %||% ggside()
  if(!is.ggside_options(ggside)) stop("argument ggside must be of class `ggside_options` or NULL")
  update_ggside(x, ggside)
}

update_ggside <- function(object, ggside) UseMethod('update_ggside')
update_ggside.default <- function(object, ggside) abort(glue("No update_ggside() method for class <", glue_collapse(class(object), sep = "/"),">"))
update_ggside.ggplot <- function(object, ggside = NULL){
  object$ggside$x.pos <- ggside$x.pos %||% object$ggside$x.pos %||% "top"
  if(!object$ggside$x.pos%in%c("top","bottom")) {
    abort("x.pos may only be \"top\" or \"bottom\".")
  }
  object$ggside$y.pos <- ggside$y.pos %||% object$ggside$y.pos %||% "right"
  if(!object$ggside$y.pos%in%c("right","left")) {
    abort("y.pos may only be \"right\" or \"left\".")
  }
  object$ggside$scales <- ggside$scales %||% object$ggside$scales %||% "fixed"
  if(!object$ggside$scales%in%c("fixed","free","free_x","free_y")){
    abort("scales may only be \"fixed\", \"free\", \"free_x\" or \"free_y\".")
  }
  object$ggside$sides_used <- get_sides(object[["layers"]])
  object$ggside$collapse <- ggside$collapse %||% object$ggside$collapse %||% NULL
  object$ggside$xsidey <- ggside$xsidey %||% object$ggside$xsidey %||% NULL
  object$ggside$ysidex <- ggside$ysidex %||% object$ggside$ysidex %||% NULL
  object[['facet']] <- as_ggsideFacet(object[['facet']], object[['ggside']])
  return(object)
}

ggplot_add.ggside_layer <- function(object, plot, object_name){
  p <- NextMethod("ggplot_add")
  as_ggside(p)
}

ggplot_add.ggside_options <- function(object, plot, object_name){
  as_ggside(plot, object)
}

ggplot_add.ggside_scale <- function(object, plot, object_name){
  plot$ggside[[intersect(c("xsidey","ysidex"), object$aesthetics)]] <- object #save scale in appropriate place
  as_ggside(plot)
}

# as_ggside <- function(plot, ggside = NULL){
#   plot <- make_ggside(plot, ggside)
#   if(inherits(plot$coordinates, "CoordFlip")||inherits(plot$coordinates, "CoordPolar")){
#     abort("ggside is not currently compatable with CoordFlip or CoordPolar")
#   }
#   plot[["facet"]] <- as_ggsideFacet(plot[["facet"]], plot[["ggside"]])
#   plot
# }

get_sides <- function(layers){
  layer_mappings <- lapply(layers, guess_layer_mapping)
  sides_used <- unlist(layer_mappings)
  sides_used <- unique(sides_used[!sides_used %in% "main"])
  return(sides_used)
}

# make_ggside <- function(object, ggside){
#   if(!is.ggside(object)){
#     class(object) <- c("ggside", class(object))
#   }
#   object$ggside$x.pos <- ggside$x.pos %||% object$ggside$x.pos %||% "top"
#   if(!object$ggside$x.pos%in%c("top","bottom")) {
#     abort("x.pos may only be \"top\" or \"bottom\".")
#   }
#   object$ggside$y.pos <- ggside$y.pos %||% object$ggside$y.pos %||% "right"
#   if(!object$ggside$y.pos%in%c("right","left")) {
#     abort("y.pos may only be \"right\" or \"left\".")
#   }
#   object$ggside$scales <- ggside$scales %||% object$ggside$scales %||% "fixed"
#   if(!object$ggside$scales%in%c("fixed","free","free_x","free_y")){
#     abort("scales may only be \"fixed\", \"free\", \"free_x\" or \"free_y\".")
#   }
#   object$ggside$sides_used <- get_sides(object[["layers"]])
#   object$ggside$collapse <- ggside$collapse %||% object$ggside$collapse %||% NULL
#   object$ggside$xsidey <- ggside$xsidey %||% object$ggside$xsidey %||% NULL
#   object$ggside$ysidex <- ggside$ysidex %||% object$ggside$ysidex %||% NULL
#   return(object)
# }

check_collapse <- function(collapse, sides){
  if(!is.null(collapse)){
    if(length(sides)==0) {
      warn(glue('collapse set to "{collapse}" but no side geometry used. Setting collapse to NULL.'))
      return(NULL)
    } else if(collapse=="all"&!all(c("x","y") %in% sides)){
      warn(glue("collapse set to \"all\" but only {sides} used. Setting collapse to {sides}."))
      return(sides)
    } else if(collapse %in% c("x","y") && !collapse %in% sides){
      warn(glue('collapse set to "{collapse}", but no {collapse}side geometry used. Setting collapse to NULL.'))
      return(NULL)
    }
  }
  return(collapse)
}

# used   all , x , y
# none   N     N   N
# x      x     +   N
# y      y     N   +
# x, y   +     +   +
