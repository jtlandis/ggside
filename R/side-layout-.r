### INCLUDE BEGIN
#' @include utils-ggplot2-reimpl-.R
#' @include utils-calls.R
NULL
### INCLUDE END


find_positional_side_scale <- function (aes, x, env = parent.frame())
{
  if (!aes %in% c("xsidey","ysidex") || is.null(x) || (is_atomic(x) && all(is.infinite(x)))) {
    return(NULL)
  }
  type <- scale_type(x)
  candidates <- paste("scale", aes, type, sep = "_")
  for (scale in candidates) {
    scale_f <- find_global(scale, env, mode = "function")
    if (!is.null(scale_f)) {
      sc <- scale_f()
      sc$call <- parse_expr(paste0(scale, "()"))
      return(sc)
    }
  }
  return(NULL)
}

find_side_scale <- function(side, data) {
  lapply(data, function(layer_data, side) {
    aesthetics <- names(layer_data)
    aesthetics <- aesthetics[grep(side, aesthetics)]
    if (length(aesthetics)==0) return(NULL)
    sc <- NULL
    for (aes in aesthetics) {
      sc <- find_positional_side_scale(side, layer_data[[aes]])
      if (!is.null(sc)) return(sc)
    }
    sc
  }, side = side)
}

#' @title Construct ggside layout
#' @name ggside_layout
#' @description
#' Creates a new layout object required for ggside functionality
#'
#' @param layout a ggproto Layout object
#' @export
ggside_layout <- function(layout) UseMethod("ggside_layout")

#' @export
ggside_layout.Layout <- function(layout) {
  new_ggside_layout(layout = layout)
}

#' @export
ggside_layout.ggsideLayout <- function(layout) layout

#' @export
ggside_layout.default <- function(layout) cli::cli_abort("cannot create ggside layout from {.cls {class(layout)}}")

new_ggside_layout <- function(layout) {
  parent_layout <- layout

  ggproto(
    "ggsideLayout",
    parent_layout,
    train_position = mod_ggproto_fun(parent_layout$train_position) |>
      mod_fun_at(quote(self$find_ggside_scales(data)), at = -1),
    find_ggside_scales = function(self, data) {
      params <- self$facet_params
      layout <- self$layout
      x_scale <- lapply(self$panel_scales_x, mod_scale_map_method)
      y_scale <- lapply(self$panel_scales_y, mod_scale_map_method)

      if ("y" %in% params$ggside$sides_used &&
          is.null(params$ggside$ysidex)) {
        ysidex <- find_side_scale("ysidex", data)
        ysidex <- unlist(ysidex)[[1]]
        #assume that if it being added this way
        # we follow the x_scale's position
        if (!is.null(ysidex) && !is.null(x_scale) )
          ysidex$position <- x_scale[[1]]$position
        params$ggside$ysidex <- ysidex
      }

      if ("x" %in% params$ggside$sides_used &&
          is.null(params$ggside$xsidey)) {
        xsidey <- find_side_scale("xsidey", data)
        xsidey <- unlist(xsidey)[[1]]
        if (!is.null(xsidey) && !is.null(y_scale))
          xsidey$position <- y_scale[[1]]$position
        params$ggside$xsidey <- xsidey
      }

      if (!is.null(x_scale) && !is.null(params$ggside$ysidex) &&
          !any(vapply(x_scale, function(scale) "ysidex" %in% scale$aesthetics, logical(1)))){
        side_indx <-  layout[layout$PANEL_TYPE=="y",]$SCALE_X
        x_scale[side_indx] <- lapply(side_indx, function(i) params$ggside$ysidex$clone())
        self$panel_scales_x <- x_scale
      }

      if (!is.null(y_scale) && !is.null(params$ggside$xsidey) &&
          !any(vapply(y_scale, function(scale) "xsidey" %in% scale$aesthetics, logical(1)))){
        side_indx <-  layout[layout$PANEL_TYPE=="x",]$SCALE_Y
        y_scale[side_indx] <- lapply(side_indx, function(i) params$ggside$xsidey$clone())
        self$panel_scales_y <- y_scale
      }
      invisible()
    },
    map_position = mod_ggproto_fun(
      parent_layout$map_position,
      self$panel_scales_x[[1]]$aesthetics ~ unique(unlist(lapply(self$panel_scales_x, `[[`, "aesthetics"))),
      self$panel_scales_y[[1]]$aesthetics ~ unique(unlist(lapply(self$panel_scales_y, `[[`, "aesthetics")))
      ),
    setup_panel_params = mod_ggproto_fun(parent_layout$setup_panel_params) |>
      mod_fun_at(quote(self$panel_params <- Map(\(param, type) {
        param$ggside_panel_type <- type
        names(param) <- sub("(x|y)side","",names(param))
        is_proto <- vapply(param, is_ggproto, logical(1))
        param[is_proto] <- lapply(param[is_proto],\(x) {x$aesthetics <- sub("(x|y)side", "", x$aesthetics);x})
        param},
        self$panel_params, self$layout$PANEL_TYPE)), at = -1),
    setup_panel_guides = mod_ggproto_fun(parent_layout$setup_panel_guides),
    get_scales = mod_ggproto_fun(parent_layout$get_scales)
  )
}
