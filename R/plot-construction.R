


ggplot_add.ggside_layer <- function(object, plot, object_name){
  plot <- NextMethod("ggplot_add")
  as_ggside(plot)
}

ggplot_add.ggside_options <- function(object, plot, object_name){
  as_ggside(plot, object)
}

as_ggside <- function(plot, ggside = NULL){
  plot <- make_ggside(plot, ggside)
  if(inherits(plot$coordinates, "CoordFlip")||inherits(plot$coordinates, "CoordPolar")){
    abort("ggside is not currently compatable with CoordFlip or CoordPolar")
  }
  plot[["facet"]] <- as_ggsideFacet(plot[["facet"]], plot[["ggside"]])
  plot
}

get_sides <- function(layers){
  layer_mappings <- lapply(layers, guess_layer_mapping)
  sides_used <- unlist(layer_mappings)
  sides_used <- unique(sides_used[!sides_used %in% "main"])
  return(sides_used)
}

make_ggside <- function(object, ggside){
  if(!is.ggside(object)){
    class(object) <- c("ggside", class(object))
  }
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
  return(object)
}

check_collapse <- function(collapse, sides){
  if(!is.null(collapse)){
    if(collapse=="all"&!all(c("x","y") %in% sides)){
      warn(glue("collapse set to \"all\" but only {sides} used. Setting collapse to {sides}."))
      return(sides)
    } else if(collapse=="x"&!"x"%in% sides){
      warn(glue("collapse set to \"x\", but no xside geometry used. Setting collapse to NULL."))
      return(sides)
    } else if(collapse=="y"&!"y"%in% sides){
      warn(glue("collapse set to \"y\", but no yside geometry used. Setting collapse to NULL."))
      return(sides)
    }
  }
  return(collapse)
}


