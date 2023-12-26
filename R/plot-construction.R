

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
  fp <- object[['facet']]$params
  col <- object$ggside$collapse
  if (!is.null(fp$free) &&
      !is.null(col) &&
      inherits(object[['facet']], "FacetWrap") &&
      any(.lgl <- vapply(fp$free, identity, logical(1)))) {
    # if ggside collapse all - but scales is free - prioritize the scale and dont
    # collapse
    # i.e. facet_wrap(..., scales='free_y') + ggside(collapse="y") --> warning
    # main plots may have different y scales and thus we cannot collapse y.
    s <- sum(c(1,2)*.lgl)
    new_col <- switch(
      s,
      free_x = {
        .f <- "free_x"
        switch(col,
               all = "y",
               x = NULL,
               col)
      },
      free_y = {
        .f <- "free_y"
        switch(col,
               all = "x",
               y = NULL,
               col)
      },
      free = {
        .f <- "free"
        NULL
      }
    )

    warning(glue("Plot's Facet parameter `scales = \"{.f}\"` is ",
                 "incompatible with `ggside(..., collapse = \"{col}\")`.",
                 " Setting collapse to ",
                 if(is.null(new_col)) 'NULL' else glue('"{new_col}"')),
            call. = F)
    object$ggside$collapse <- new_col


  }
  object$ggside$xsidey <- ggside$xsidey %||% object$ggside$xsidey %||% NULL
  object$ggside$ysidex <- ggside$ysidex %||% object$ggside$ysidex %||% NULL
  object$ggside$draw_x_on <- ggside$draw_x_on %||% object$ggside$draw_x_on %||% "default"
  object$ggside$draw_y_on <- ggside$draw_y_on %||% object$ggside$draw_y_on %||% "default"
  object$ggside$strip <- ggside$strip %||% object$ggside$strip %||% "default"
  object$ggside$respect_side_labels <- ggside$respect_side_labels %||% object$ggside$respect_side_labels %||% FALSE
  return(object)
}



muffle_opts_warn <- function(f) {
  function(...) {
    withCallingHandlers(
      warning = function(cnd) {
        msg <- conditionMessage(cnd)
        if (grepl(".(<|>). not meaningful for factors", msg)) {
          rlang::cnd_muffle(cnd)
        }
      },
      f(...)
    )
  }
}


get_sides <- function(layers){
  layer_mappings <- lapply(layers, layer_type)
  sides_used <- unlist(layer_mappings)
  sides_used <- unique(sides_used[!sides_used %in% "main"])
  return(sides_used)
}



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
