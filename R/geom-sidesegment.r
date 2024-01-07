### INCLUDE BEGIN
#' @include compat-plyr.R
#' @include ggside.R
#' @include side-layer.R
#' @include constructor-.R
NULL
### INCLUDE END
#' @title Side line Segments
#' @description
#'  The [xside] and [yside] of \link[ggplot2]{geom_segment}.
#' @inheritParams ggplot2::geom_segment
#'
#' @aliases geom_*sidesegment
#' @return XLayer or YLayer object to be added to a ggplot object
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(ggdendro)
#' #dendrogram with geom_*sidesegment
#' df0 <- mutate(diamonds,
#' colclar = interaction(color, clarity,
#'                       sep = "_", drop = TRUE))
#' df1 <- df0 %>%
#'   group_by(color, clarity, colclar, cut) %>%
#'   summarise(m_price = mean(price))
#' df <- df1 %>%
#'   pivot_wider(id_cols = colclar,
#'               names_from = cut,
#'               values_from = m_price,
#'               values_fill = 0L)
#'
#' mat <- as.matrix(df[,2:6])
#' rownames(mat) <- df[["colclar"]]
#' dst <- dist(mat)
#' hc_x <- hclust(dst)
#' lvls <- rownames(mat)[hc_x$order]
#' df1[["colclar"]] <- factor(df1[["colclar"]], levels = lvls)
#' dendrox <- dendro_data(hc_x)
#'
#' p <- ggplot(df1, aes(x = colclar, cut)) +
#'   geom_tile(aes(fill = m_price)) +
#'   viridis::scale_fill_viridis(option = "magma") +
#'   theme(axis.text.x = element_text(angle = 90, vjust = .5))
#' p +
#'   geom_xsidesegment(data = dendrox$segments,aes(x = x, y = y, xend = xend, yend = yend))
#' @export
geom_xsidesegment <- ggside_layer_function(fun = geom_segment, side = "x")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidesegment <- ggside_geom("GeomXsidesegment", GeomSegment, "x")

#' @rdname geom_xsidesegment
#' @export
geom_ysidesegment <- ggside_layer_function(fun = geom_segment, side = "y")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidesegment <- ggside_geom("GeomYsidesegment", GeomSegment, "y")
