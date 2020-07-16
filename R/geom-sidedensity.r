#' @export
geom_xsidedensity <- function(mapping = NULL, data = NULL,
         stat = "density", position = "identity",
         ...,
         na.rm = FALSE,
         orientation = "x",
         show.legend = NA,
         inherit.aes = TRUE,
         outline.type = "upper") {
  outline.type <- match.arg(outline.type, c("both", "upper", "lower", "full"))
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsidedensity,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      outline.type = outline.type,
      ...
    )
  )
  structure(list(layer = l), class = "ggside")
}

GeomXsidedensity <- ggplot2::ggproto("GeomXsidedensity",
                                     ggplot2::GeomDensity,
                                     default_aes = aes(fill = NA, xfill = NA, weight = 1,
                                                       colour = "black", xcolour = NA, alpha = NA,
                                                       size = 0.5, linetype = 1),
                                     setup_data = function(data, params) {
                                       data <- parse_side_aes(data, params)
                                       data$flipped_aes <- params$flipped_aes
                                       data <- flip_data(data, params$flipped_aes)
                                       data <- transform(data[order(data$PANEL, data$group, data$x), ], ymin = 0, ymax = y)
                                       flip_data(data, params$flipped_aes)
                                     },
                                     draw_group = function(data, panel_params, coord, na.rm = FALSE, flipped_aes = FALSE, outline.type = "both") {
                                       #browser()
                                       data$fill <- data$xfill %NA% data$fill
                                       data$colour <- data$xcolour %NA% data$colour
                                       data <- flip_data(data, flipped_aes)
                                       if (na.rm) data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
                                       data <- data[order(data$group), ]

                                       # Check that aesthetics are constant
                                       aes <- unique(data[c("colour", "fill", "size", "linetype", "alpha")])
                                       if (nrow(aes) > 1) {
                                         abort("Aesthetics can not vary with a ribbon")
                                       }
                                       aes <- as.list(aes)

                                       # Instead of removing NA values from the data and plotting a single
                                       # polygon, we want to "stop" plotting the polygon whenever we're
                                       # missing values and "start" a new polygon as soon as we have new
                                       # values.  We do this by creating an id vector for polygonGrob that
                                       # has distinct polygon numbers for sequences of non-NA values and NA
                                       # for NA values in the original data.  Example: c(NA, 2, 2, 2, NA, NA,
                                       # 4, 4, 4, NA)
                                       missing_pos <- !stats::complete.cases(data[c("x", "ymin", "ymax")])
                                       ids <- cumsum(missing_pos) + 1
                                       ids[missing_pos] <- NA

                                       data <- unclass(data) #for faster indexing

                                       # The upper line and lower line need to processed separately (#4023)
                                       positions_upper <- new_data_frame(list(
                                         x = data$x,
                                         y = data$ymax,
                                         id = ids
                                       ))

                                       positions_lower <- new_data_frame(list(
                                         x = rev(data$x),
                                         y = rev(data$ymin),
                                         id = rev(ids)
                                       ))

                                       positions_upper <- flip_data(positions_upper, flipped_aes)
                                       positions_lower <- flip_data(positions_lower, flipped_aes)

                                       munched_upper <- coord_munch(coord, positions_upper, panel_params)
                                       munched_lower <- coord_munch(coord, positions_lower, panel_params)

                                       munched_poly <- rbind(munched_upper, munched_lower)

                                       is_full_outline <- identical(outline.type, "full")
                                       g_poly <- polygonGrob(
                                         munched_poly$x, munched_poly$y, id = munched_poly$id,
                                         default.units = "native",
                                         gp = gpar(
                                           fill = alpha(aes$fill, aes$alpha),
                                           col = if (is_full_outline) aes$colour else NA,
                                           lwd = if (is_full_outline) aes$size * .pt else 0,
                                           lty = if (is_full_outline) aes$linetype else 1
                                         )
                                       )

                                       if (is_full_outline) {
                                         return(ggname("geom_ribbon", g_poly))
                                       }

                                       # Increment the IDs of the lower line so that they will be drawn as separate lines
                                       munched_lower$id <- munched_lower$id + max(ids, na.rm = TRUE)

                                       munched_lines <- switch(outline.type,
                                                               both = rbind(munched_upper, munched_lower),
                                                               upper = munched_upper,
                                                               lower = munched_lower,
                                                               abort(glue("invalid outline.type: {outline.type}"))
                                       )
                                       g_lines <- polylineGrob(
                                         munched_lines$x, munched_lines$y, id = munched_lines$id,
                                         default.units = "native",
                                         gp = gpar(
                                           col = aes$colour,
                                           lwd = aes$size * .pt,
                                           lty = aes$linetype)
                                       )

                                       ggname("geom_ribbon", grobTree(g_poly, g_lines))
                                     },
                                     draw_key = function(data, params, size) {
                                       if (is.null(data$size)) {
                                         data$size <- 0.5
                                       }
                                       lwd <- min(data$size, min(size) / 4)
                                       colour <- data$xcolour %NA% data$colour
                                       fill <- data$xfill %NA% data$fill
                                       rectGrob(
                                         width = unit(1, "npc") - unit(lwd, "mm"),
                                         height = unit(1, "npc") - unit(lwd, "mm"),
                                         gp = gpar(
                                           col = colour,
                                           fill = alpha(fill, data$alpha),
                                           lty = data$linetype %||% 1,
                                           lwd = lwd * .pt,
                                           linejoin = params$linejoin %||% "mitre",
                                           # `lineend` is a workaround for Windows and intentionally kept unexposed
                                           # as an argument. (c.f. https://github.com/tidyverse/ggplot2/issues/3037#issuecomment-457504667)
                                           lineend = if (identical(params$linejoin, "round")) "round" else "square"
                                         ))
                                     })


#' @export
geom_ysidedensity <- function(mapping = NULL, data = NULL,
                              stat = "density", position = "identity",
                              ...,
                              na.rm = FALSE,
                              orientation = "y",
                              show.legend = NA,
                              inherit.aes = TRUE,
                              outline.type = "upper") {
  outline.type <- match.arg(outline.type, c("both", "upper", "lower", "full"))
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsidedensity,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      outline.type = outline.type,
      ...
    )
  )
  structure(list(layer = l), class = "ggside")
}

GeomYsidedensity <- ggplot2::ggproto("GeomYsidedensity",
                                     ggplot2::GeomDensity,
                                     default_aes = aes(fill = NA, yfill = NA, weight = 1,
                                                       colour = "black", xcolour = NA, alpha = NA,
                                                       size = 0.5, linetype = 1),
                                     setup_data = function(data, params) {
                                       data <- parse_side_aes(data, params)
                                       data$flipped_aes <- params$flipped_aes
                                       data <- flip_data(data, params$flipped_aes)
                                       data <- transform(data[order(data$PANEL, data$group, data$x), ], ymin = 0, ymax = y)
                                       flip_data(data, params$flipped_aes)
                                     },
                                     draw_group = function(data, panel_params, coord, na.rm = FALSE, flipped_aes = FALSE, outline.type = "both") {
                                       #browser()
                                       data$fill <- data$yfill %NA% data$fill
                                       data$colour <- data$ycolour %NA% data$colour
                                       data <- flip_data(data, flipped_aes)
                                       if (na.rm) data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
                                       data <- data[order(data$group), ]

                                       # Check that aesthetics are constant
                                       aes <- unique(data[c("colour", "fill", "size", "linetype", "alpha")])
                                       if (nrow(aes) > 1) {
                                         abort("Aesthetics can not vary with a ribbon")
                                       }
                                       aes <- as.list(aes)

                                       # Instead of removing NA values from the data and plotting a single
                                       # polygon, we want to "stop" plotting the polygon whenever we're
                                       # missing values and "start" a new polygon as soon as we have new
                                       # values.  We do this by creating an id vector for polygonGrob that
                                       # has distinct polygon numbers for sequences of non-NA values and NA
                                       # for NA values in the original data.  Example: c(NA, 2, 2, 2, NA, NA,
                                       # 4, 4, 4, NA)
                                       missing_pos <- !stats::complete.cases(data[c("x", "ymin", "ymax")])
                                       ids <- cumsum(missing_pos) + 1
                                       ids[missing_pos] <- NA

                                       data <- unclass(data) #for faster indexing

                                       # The upper line and lower line need to processed separately (#4023)
                                       positions_upper <- new_data_frame(list(
                                         x = data$x,
                                         y = data$ymax,
                                         id = ids
                                       ))

                                       positions_lower <- new_data_frame(list(
                                         x = rev(data$x),
                                         y = rev(data$ymin),
                                         id = rev(ids)
                                       ))

                                       positions_upper <- flip_data(positions_upper, flipped_aes)
                                       positions_lower <- flip_data(positions_lower, flipped_aes)

                                       munched_upper <- coord_munch(coord, positions_upper, panel_params)
                                       munched_lower <- coord_munch(coord, positions_lower, panel_params)

                                       munched_poly <- rbind(munched_upper, munched_lower)

                                       is_full_outline <- identical(outline.type, "full")
                                       g_poly <- polygonGrob(
                                         munched_poly$x, munched_poly$y, id = munched_poly$id,
                                         default.units = "native",
                                         gp = gpar(
                                           fill = alpha(aes$fill, aes$alpha),
                                           col = if (is_full_outline) aes$colour else NA,
                                           lwd = if (is_full_outline) aes$size * .pt else 0,
                                           lty = if (is_full_outline) aes$linetype else 1
                                         )
                                       )

                                       if (is_full_outline) {
                                         return(ggname("geom_ribbon", g_poly))
                                       }

                                       # Increment the IDs of the lower line so that they will be drawn as separate lines
                                       munched_lower$id <- munched_lower$id + max(ids, na.rm = TRUE)

                                       munched_lines <- switch(outline.type,
                                                               both = rbind(munched_upper, munched_lower),
                                                               upper = munched_upper,
                                                               lower = munched_lower,
                                                               abort(glue("invalid outline.type: {outline.type}"))
                                       )
                                       g_lines <- polylineGrob(
                                         munched_lines$x, munched_lines$y, id = munched_lines$id,
                                         default.units = "native",
                                         gp = gpar(
                                           col = aes$colour,
                                           lwd = aes$size * .pt,
                                           lty = aes$linetype)
                                       )

                                       ggname("geom_ribbon", grobTree(g_poly, g_lines))
                                     },
                                     draw_key = function(data, params, size) {
                                       if (is.null(data$size)) {
                                         data$size <- 0.5
                                       }
                                       lwd <- min(data$size, min(size) / 4)
                                       colour <- data$ycolour %NA% data$colour
                                       fill <- data$yfill %NA% data$fill
                                       rectGrob(
                                         width = unit(1, "npc") - unit(lwd, "mm"),
                                         height = unit(1, "npc") - unit(lwd, "mm"),
                                         gp = gpar(
                                           col = colour,
                                           fill = alpha(fill, data$alpha),
                                           lty = data$linetype %||% 1,
                                           lwd = lwd * .pt,
                                           linejoin = params$linejoin %||% "mitre",
                                           # `lineend` is a workaround for Windows and intentionally kept unexposed
                                           # as an argument. (c.f. https://github.com/tidyverse/ggplot2/issues/3037#issuecomment-457504667)
                                           lineend = if (identical(params$linejoin, "round")) "round" else "square"
                                         ))
                                     })
