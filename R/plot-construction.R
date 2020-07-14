

#' @export
ggplot_add.ggside <- function(object, plot, object_name){
  plot <- make_ggside(plot, object)
  plot + object$layer
}


make_ggside <- function(object, ggside){
  if(!is.ggside(object)){
    class(object) <- c("ggside",class(object))
  }
  object$ggside$x.pos <- ggside$x.pos %||% object$ggside$x.pos %||% "top"
  object$ggside$y.pos <- ggside$y.pos %||% object$ggside$y.pos %||% "right"
  object$ggside$scales <- ggside$scales %||% object$ggside$scales %||% "fixed"
  return(object)
}

is.ggside <- function(x) inherits(x, "ggside")
