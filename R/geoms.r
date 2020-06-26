

#' geom_*sidebar: Plot a sidebar along an axis
#'
#' @param mapping Set of aesthetic mappings reated by [`aes()`] or
#' [`aes_()`]. If specified and `inherit.aes = TRUE`, it will be
#' combined with the default mapping at the top level of the plot.
#' `mapping` must be supplied in this layer if ther is no plot mapping.
#' @param data The data to be displayed in this layer. If `NULL`, the default,
#' then the data is inherited from the plot data as specified i nthe call to [`ggplot()`].
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... other arguments passed on to [`layer()`]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `color = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#'
#'
#' @section Aesthetics:
#'
#' Required aesthetics are in bold.
#'
#' \itemize{
#' \item \strong{x}
#' \item \strong{y}
#' \item \emph{xfill} Fill color of the xsidebar
#' \item \emph{yfill} Fill color of the ysidebar
#' \item \emph{width} specifies the width of each bar
#' \item \emph{height} specifies the height of each bar
#' \item \emph{alpha} Transparency level of `xfill` or `yfill` depending on which [`geom_*sidbar`]
#' \item \emph{size} size of the border line. --uneeded??
#' \item \emph{location} Specifies where the sidebar should be placed.
#' geom_xsidebar may specify either "bottom" or "top" and
#' geom_ysidebar may specify either "left" or "right.
#' "bottom" and "left" are defaults of geom_xsidebar and
#' geom_ysidebar respecitively.
#'
#' }
#'
#'
#'
#'
#' @import ggplot2
#' @export
geom_xsidebar <- function(mapping = NULL, data = NULL,
                          na.rm = FALSE, show.legend = TRUE,
                          position = "identity",stat = "identity", inherit.aes = TRUE, ...) {
  #browser()
  # if(!location%in%c("bottom","top")){
  #   stop("location must be specified as top or bottom")
  # }
  layer(
    geom = GeomXSideBar, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname geom_xsidebar
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Geom GeomTile
GeomXSideBar <- ggplot2::ggproto("XSideBar",
                        ggplot2::GeomTile,
                        requied_aes = c("x","y"),
                        default_aes = aes(xfill = "grey20",
                                          width = 1, height = 1,
                                          size = 0.1, alpha = 1, location = "bottom"),
                        draw_key = function(data, params, size){
                          {
                            if (is.null(data$size)) {
                              data$size <- 0.5
                            }
                            lwd <- min(data$size, min(size)/4)
                            rectGrob(width = unit(1, "npc") - unit(lwd, "mm"),
                                     height = unit(1,"npc") - unit(lwd, "mm"),
                                     gp = gpar(col = data$colour %||% NA,
                                               fill = alpha(data$xfill %||% "grey20", data$alpha),
                                               lty = data$linetype %||% 1,
                                               lwd = lwd * .pt,
                                               linejoin = params$linejoin %||% "mitre",
                                               lineend = if (identical(params$linejoin,"round")) "round" else "square"))
                          }
                        },
                        setup_data = function(data, params){
                          #pad the width and height
                          data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
                          yres <- if(resolution(data$y, FALSE)!=1) (diff(range(data$y))*.05) else 1
                          data$height <- data$height %||% params$height %||% yres
                          data$location <- data$location %||% params$location
                          loc <- unique(data$location)
                          if(!loc%in%c("bottom","top")||length(loc)>1){
                            stop("xbar location must be either \"bottom\" or \"top\"\n")
                          }
                          if(loc=="bottom"){
                            data$yint <- min(data$y) - unique(data$height)
                          } else if(loc=="top"){
                            data$yint <- max(data$y) + unique(data$height)
                          }
                          transform(data, xmin = x - width/2, xmax = x + width/2, width = NULL,
                                    ymin = yint - height/2, ymax = yint + height/2, height = NULL)
                        },
                        draw_panel = function (self, data, panel_params, coord, linejoin = "mitre")
                        {
                          #browser()
                          loc <- unique(data$location)
                          if(loc=="bottom"){
                            indx <- 1
                           # .expand <- .6
                          } else if(loc=="top"){
                            indx <- 2
                           # .expand <- -.6
                          }
                          if(panel_params$y$is_discrete()){
                            # panel_params$y$continuous_range[indx] <- panel_params$y$continuous_range[indx] + .expand
                            # panel_params$y$limits <- if(loc=="bottom") c(panel_params$y$limits, "xbar") else c("xbar", panel_params$y$limits)
                          } else {
                            panel_params$y$continuous_range[indx] <- panel_params$y$limits[indx]
                          }
                          if (!coord$is_linear()) {
                            aesthetics <- setdiff(names(data), c("x", "y", "xmin",
                                                                 "xmax", "ymin", "ymax"))
                            polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
                              poly <- rect_to_poly(row$xmin, row$xmax, row$ymin,
                                                   row$ymax)
                              aes <- new_data_frame(row[aesthetics])[rep(1, 5),]
                              GeomPolygon$draw_panel(cbind(poly, aes), panel_params,coord)
                            })
                            ggname("bar", do.call("grobTree", polys))
                          }
                          else {
                            #browser()
                            coords <- coord$transform(data, panel_params)
                            ggname("geom_rect", rectGrob(coords$xmin, coords$ymax,
                                                         width = coords$xmax - coords$xmin,
                                                         height = coords$ymax - coords$ymin,
                                                         default.units = "native", just = c("left","top"),
                                                         gp = gpar(col = coords$colour %||% alpha(coords$xfill,coords$alpha),
                                                                   fill = alpha(coords$xfill,coords$alpha),
                                                                   lwd = coords$size * .pt,
                                                                   lty = coords$linetype,
                                                                   linejoin = linejoin,
                                                                   lineend = if (identical(linejoin,"round"))"round" else "square")))
                          }
                        }


)


#' @rdname geom_xsidebar
#' @aliases geom_ysidebar
#' @export
geom_ysidebar <- function(mapping = NULL, data = NULL,
                          na.rm = FALSE, show.legend = TRUE,
                          position = "identity",stat = "identity",
                          inherit.aes = TRUE, location = "left", ...) {
  #browser()
  # if(!location%in%c("left","right")){
  #   sright("location must be specified as right or left")
  # }
  layer(
    geom = GeomYsideBar, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(location = location, na.rm = na.rm, ...)
  )
}

#' @rdname geom_xsidebar
#' @format NULL
#' @usage NULL
GeomYsideBar <- ggplot2::ggproto("YsideBar",
                                 ggplot2::GeomTile,
                                 requied_aes = c("x","y"),
                                 default_aes = aes(yfill = "grey20",
                                                   width = 1, height = 1,
                                                   size = 0.1, alpha = 1, location = "left"),
                                 draw_key = function(data, params, size){
                                   {
                                     if (is.null(data$size)) {
                                       data$size <- 0.5
                                     }
                                     lwd <- min(data$size, min(size)/4)
                                     rectGrob(width = unit(1, "npc") - unit(lwd, "mm"),
                                              height = unit(1,"npc") - unit(lwd, "mm"),
                                              gp = gpar(col = data$colour %||% NA,
                                                        fill = alpha(data$yfill %||% "grey20", data$alpha),
                                                        lty = data$linetype %||% 1,
                                                        lwd = lwd * .pt,
                                                        linejoin = params$linejoin %||% "mitre",
                                                        lineend = if (identical(params$linejoin,"round")) "round" else "square"))
                                   }
                                 },
                                 setup_data = function(data, params){
                                   #browser()
                                   #pad the width and height
                                   data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)
                                   xres <- if(resolution(data$x, FALSE)!=1) (diff(range(data$x))*.05) else 1
                                   data$width <- data$width %||% params$width %||% xres
                                   data$location <- data$location %||% params$location
                                   loc <- unique(data$location)
                                   if(!loc%in%c("left","right")||length(loc)>1){
                                     stop("ybar location must be either \"left\" or \"right\"\n")
                                   }
                                   if(loc=="left"){
                                     data$xint <- min(data$x) - unique(data$width)
                                   } else if(loc=="right"){
                                     data$xint <- max(data$x) + unique(data$width)
                                   }
                                   transform(data, xmin = xint - width/2, xmax = xint + width/2, width = NULL,
                                             ymin = y - height/2, ymax = y + height/2, height = NULL)
                                 },
                                 draw_panel = function (self, data, panel_params, coord, linejoin = "mitre")
                                 {
                                   #browser()
                                   loc <- unique(data$location)
                                   if(loc=="left"){
                                     indx <- 1
                                     # .expand <- .6
                                   } else if(loc=="right"){
                                     indx <- 2
                                     # .expand <- -.6
                                   }
                                   if(panel_params$y$is_discrete()){
                                     # panel_params$y$continuous_range[indx] <- panel_params$y$continuous_range[indx] + .expand
                                     # panel_params$y$limits <- if(loc=="left") c(panel_params$y$limits, "ybar") else c("ybar", panel_params$y$limits)
                                   } else {
                                     panel_params$y$continuous_range[indx] <- panel_params$y$limits[indx]
                                   }
                                   if (!coord$is_linear()) {
                                     aesthetics <- setdiff(names(data), c("x", "y", "xmin",
                                                                          "xmax", "ymin", "ymax"))
                                     polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
                                       poly <- rect_to_poly(row$xmin, row$xmax, row$ymin,
                                                            row$ymax)
                                       aes <- new_data_frame(row[aesthetics])[rep(1, 5),]
                                       GeomPolygon$draw_panel(cbind(poly, aes), panel_params,coord)
                                     })
                                     ggname("bar", do.call("grobTree", polys))
                                   }
                                   else {
                                     browser()
                                     coords <- coord$transform(data, panel_params)
                                     ggname("geom_rect", rectGrob(coords$xmin, coords$ymax,
                                                                  width = coords$xmax - coords$xmin,
                                                                  height = coords$ymax - coords$ymin,
                                                                  default.units = "native", just = c("left","right"),
                                                                  gp = gpar(col = coords$colour %||% alpha(coords$yfill,coords$alpha),
                                                                            fill = alpha(coords$yfill,coords$alpha),
                                                                            lwd = coords$size * .pt,
                                                                            lty = coords$linetype,
                                                                            linejoin = linejoin,
                                                                            lineend = if (identical(linejoin,"round"))"round" else "square")))
                                   }
                                 }


)




