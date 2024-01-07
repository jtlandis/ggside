### INCLUDE BEGIN
#' @include compat-plyr.R
#' @include ggside.R
#' @include side-layer.R
#' @include constructor-.R
NULL
### INCLUDE END
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
geom_xsidetile <- ggside_layer_function(fun = geom_tile, side = "x")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidetile <- ggside_geom("GeomXsidetile", GeomTile, "x")

#' @rdname geom_xsidetile
#' @export
geom_ysidetile <- ggside_layer_function(fun = geom_tile, side = "y")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidetile <- ggside_geom("GeomYsidetile", GeomTile, "y")
