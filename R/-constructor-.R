

ggside_geom <- function(class_name = NULL,
                        geom = NULL,
                        side = c("x","y")) {
  side <- match.arg(side, c("x","y"))
  o <- switch(side, x = "y", y = "x")
  i <- match(side, c("x","y"))
  req_aes <- pull_side(geom$required_aes, i)
  opt_aes <- pull_side(geom$optional_aes, i)
  non_mis <- pull_side(geom$non_missing_aes, i)
  def_aes <- names(geom$default_aes)
  all_aes <- c(req_aes, opt_aes, non_mis, def_aes)
  .aes_to_map <- all_aes[all_aes %in% .ggside_global[[sprintf(".%s_aes",o)]]]
  ggplot2::ggproto(
    class_name,
    geom,
    default_aes = new_default_aes(geom, side),
    required_aes = rename_side(geom$required_aes, side),
    optional_aes = rename_side(geom$optional_aes, side),
    non_missing_aes = rename_side(geom$non_missing_aes, side),
    setup_data = function(self, data, params) {
      data <- parse_side_aes(data, params)
      data <- self$.data_unmapper(data)
      geom$setup_data(data, params)
    },
    draw_panel = function(self, data, panel_params,
                          coord, width = NULL, flipped_aes = FALSE) {
      data <- use_side_aes(data, side)
      data <- self$.data_unmapper(data)
      parent <- ggplot2::ggproto_parent(geom, self)
      parent$draw_panel(data = data, panel_params = panel_params,
                        coord = coord, width = width,
                        flipped_aes = flipped_aes)
    },
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


#' Creates new ScaleList
#' Modifies the `$add` method such that
#' when a positional scale is added it is
#' recast with `new_pos_scale`
ggside_scales <- function(scales) {
  new_scales <- ggproto(
    NULL,
    scales,
    add = function(self, scale) {
      ggproto_parent(scales, self)$add(scale)
      if (!is.null(scale) && any(pos <- c("x","y") %in% scale$aesthetics)) {
        side <- switch(c("x","y")[pos], x = "yside", y = "xside")
        s <- self$scales[[self$n()]]
        s$aesthetics <- c(s$aesthetics, paste0(side, s$aesthetics))
        self$scales[[self$n()]] <- new_pos_scale(s)
      }
    })

  lgl <- new_scales$find(c("x","y"))
  if (any(lgl))
    new_scales[lgl] <- lapply(new_scales$scales[lgl], new_pos_scale)

  new_scales

}

#' positional scales in `ggside` will contain
#' the `xsidey` and `ysidex` in their aesthetics
#' to ensure all data are trained properly.
#' To ensure each scale retains its own transform,
#' these scales will ignore `xsidey` and `ysidex`
#' aesthetics when `$transform_df` is called.
new_pos_scale <- function(scale) {
  ggproto(
    NULL,
    scale,
    transform_df = function(self, df) {
      local_vanilla_scale_aes(self)
      ggproto_parent(scale, self)$transform_df(df)
    })
}

