

#' @export
ggplot_add.ggside <- function(object, plot, object_name){
  plot <- make_ggside(plot, object)
  if("layer"%in%names(object)){
    plot + object$layer
  } else {
    plot
  }
}


make_ggside <- function(object, ggside){
  if(!is.ggside(object)){
    class(object) <- c("ggside",class(object))
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
  return(object)
}

is.ggside <- function(x) inherits(x, "ggside")
