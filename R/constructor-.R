
#' @name ggside_geom
#' @title ggside geom constructor
#' @description utility function to make a ggside Geom
#' @param class_name New class name for the ggproto object
#' @param geom The Geom ggproto to inherit from
#' @param side should the resulting object be configured for x or y
#' @export
ggside_geom <- function(class_name = NULL,
                        geom = NULL,
                        side = c("x","y")) {
  side <- match.arg(side, c("x","y"))
  o <- switch(side, x = "y", y = "x")
  i <- match(side, c("x","y"))
  req_aes <- pull_aes(geom$required_aes)
  opt_aes <- pull_aes(geom$optional_aes)
  non_mis <- pull_aes(geom$non_missing_aes)
  def_aes <- names(geom$default_aes)
  all_aes <- c(req_aes, opt_aes, non_mis, def_aes)
  .aes_to_map <- all_aes[all_aes %in% .ggside_global[[sprintf(".%s_aes",o)]]]

  args <- formals(environment(geom$draw_panel)$f)
  args2 <- lapply(names(args), as.name)
  names(args2) <- names(args)
  if (!"self" %in% names(args)) {
    args$self <- quote(self)
  } else {
    args2 <- args2[setdiff(names(args2), "self")]
  }
  fun <- function() {}
  formals(fun) <- args
  body(fun) <- expr({
    data <- use_side_aes(data, side)
    data <- self$.data_unmapper(data)
    parent <- ggproto_parent(geom, self)
    parent$draw_panel(!!!args2)
  })

  ggplot2::ggproto(
    class_name,
    geom,
    .side = side,
    default_aes = new_default_aes(geom, side),
    required_aes = rename_side(geom$required_aes, side),
    optional_aes = rename_side(geom$optional_aes, side),
    non_missing_aes = rename_side(geom$non_missing_aes, side),
    setup_data = function(self, data, params) {
      #browser()
      data <- parse_side_aes(data, params)
      data <- self$.data_unmapper(data)
      geom$setup_data(data, params)
    },
    draw_panel = fun,
    draw_key = function(data, params, size) {
      data <- use_side_aes(data, side)
      geom$draw_key(data = data, params = params, size = size)
    },
    .aes_to_map = .aes_to_map,
    .data_mapper = function(self, data) {
      x <- names(data)
      aes <- x %in% self$.aes_to_map
      x[aes] <- sprintf("%sside%s", side, x[aes])
      names(data) <- x
      data
    },
    .data_unmapper = function(self, data) {
      names(data) <- sub(sprintf("%sside", side), "", names(data))
      data
    }

  )
}


# Creates new ScaleList
# Modifies the `$add` method such that
# when a positional scale is added it is
# recast with `new_pos_scale`
ggside_scales <- function(scales, ggside) {
  new_scales <- ggproto(
    NULL,
    scales,
    ggside = ggside,
    non_position_scales = function(self) {
      ggproto(NULL, self, scales = self$scales[!self$find(c("x","y", "ysidex", "xsidey"))])
    },
    add = function(self, scale) {
      # browser()
      parent <- ggproto_parent(scales, self)
      parent$add(scale)
      if (!is.null(scale)) {
        if (any(pos <- c("x","y") %in% scale$aesthetics)) {
          side <- switch(c("x","y")[pos], x = "yside", y = "xside")
          s <- new_pos_scale(self$scales[[self$n()]])
          self$scales[[self$n()]] <- s
        } else if (any(pos <- c("ysidex", "xsidey") %in% scale$aesthetics)) {
          side <- c("ysidex", "xsidey")[pos]
          main_scale <- self$find(switch(side, ysidex = "x", xsidey = "y"))
          # if main scale is already specified
          # then assume its position.
          if (any(main_scale)) {
            main_scale <- self$scales[main_scale]
            scale$position <- main_scale[[1]]$position
          }
          self$ggside[[side]] <- scale
        }
      }
    },
    input = function(self) {
      out <- lapply(self$scales, `[[`, "aesthetics")
      x_scales <- self$find("x")
      if (any(x_scales))
        out[x_scales] <- lapply(out[x_scales], function(x) x[!grepl("yside", x)])
      y_scales <- self$find("y")
      if (any(y_scales))
        out[y_scales] <- lapply(out[y_scales], function(x) x[!grepl("xside", x)])
      unlist(out)
    },
    find = function(self, aesthetic) {
      vapply(self$scales,
             function(x) {
               aes <- x$aesthetics
               if(any(pos <- c("x","y") %in% aes)) {
                 side <- switch(c("x","y")[pos], x = "yside", y = "xside")
                 aes <- aes[!grepl(side, aes)]
               }
               any(aesthetic %in% aes)
             }, logical(1))
    })

  lgl <- new_scales$find(c("x","y"))
  if (any(lgl))
    new_scales$scales[lgl] <- lapply(new_scales$scales[lgl], new_pos_scale)

  new_scales

}

# positional scales in `ggside` will contain
# the `xsidey` and `ysidex` in their aesthetics
# to ensure all data are trained properly.
# To ensure each scale retains its own transform,
# these scales will ignore `xsidey` and `ysidex`
# aesthetics when `$transform_df` is called.
new_pos_scale <- function(scale) {
  ggproto(
    NULL,
    scale,
    # transform_df = function(self, df) {
    #   browser()
    #   # local_vanilla_scale_aes(self)
    #   ggproto_parent(scale, self)$transform_df(df)
    # },
    map = function(self, x, limits = self$get_limits()) {
      if (length(x)==0) return(x)
      parent <- ggproto_parent(scale, self)
      parent$map(x, limits)
    })
}




