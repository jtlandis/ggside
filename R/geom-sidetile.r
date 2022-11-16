#' @title Side tile plot
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_tile}
#' @inheritParams ggplot2::geom_tile
#' @aliases geom_*sidetile
#' @return XLayer or YLayer object to be added to a ggplot object
#' @examples
#' library(dplyr)
#' library(tidyr)
#' df <- mutate(diamonds,
#'              colclar = interaction(color, clarity, sep = "_", drop = TRUE)) %>%
#'       group_by(color, clarity, colclar, cut) %>%
#'       summarise(m_price = mean(price))
#'
#' xside_data <- df %>%
#'   ungroup() %>%
#'   select(colclar, clarity, color) %>%
#'   mutate_all(~factor(as.character(.x), levels = levels(.x))) %>%
#'   pivot_longer(cols = c(clarity, color)) %>% distinct()
#'
#'
#' p <- ggplot(df, aes(x = colclar, cut)) +
#'   geom_tile(aes(fill = m_price)) +
#'   viridis::scale_fill_viridis(option = "magma") +
#'   theme(axis.text.x = element_blank())
#'
#' p + geom_xsidetile(data = xside_data, aes(y = name, xfill = value)) +
#'    guides(xfill = guide_legend(nrow = 8))
#' @export
geom_xsidetile <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          linejoin = "mitre",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsidetile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    ),
    layer_class = XLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidetile <- ggplot2::ggproto("GeomXsidetile",
                                  ggplot2::GeomTile,
                                  default_aes = new_default_aes(
                                    aes(xcolour = NA, xfill = NA),
                                    ggplot2::GeomTile$default_aes
                                  ),
                                  setup_data = function(data, params) {
                                    data <- parse_side_aes(data, params)
                                    ggplot2::GeomTile$setup_data(data, params)
                                  },
                                  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
                                    data <- use_xside_aes(data)
                                    ggplot2::GeomTile$draw_panel(data = data, panel_params = panel_params, coord = coord, linejoin = linejoin)
                                  },
                                  draw_key = function(data, params, size){
                                    data <- use_xside_aes(data)
                                    ggplot2::GeomTile$draw_key(data, params, size)
                                  })

#' @rdname geom_xsidetile
#' @export
geom_ysidetile <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           linejoin = "mitre",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsidetile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    ),
    layer_class = YLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidetile <- ggplot2::ggproto("GeomYsidetile",
                                  ggplot2::GeomTile,
                                  default_aes = new_default_aes(
                                    aes(ycolour = NA, yfill = NA),
                                    ggplot2::GeomTile$default_aes
                                  ),
                                  setup_data = function(data, params) {
                                    data <- parse_side_aes(data, params)
                                    ggplot2::GeomTile$setup_data(data, params)
                                  },
                                  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
                                    #browser()
                                    data <- use_yside_aes(data)
                                    ggplot2::GeomTile$draw_panel(data = data, panel_params = panel_params, coord = coord, linejoin = linejoin)
                                  },
                                  draw_key = function(data, params, size){
                                    data <- use_yside_aes(data)
                                    ggplot2::GeomTile$draw_key(data, params, size)
                                  })
