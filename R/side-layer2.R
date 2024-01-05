### INCLUDE BEGIN
#' @include utils.R
NULL
### INCLUDE END

#' @name ggside_layer
#' @title New ggside layer
#' @description utility function to make a ggside layer compatible with `ggside` internals
#' @inheritParams ggplot2::layer
#' @export
ggside_layer2 <- function(geom = NULL, stat = NULL, data = NULL, mapping = NULL,
position = NULL, params = list(), inherit.aes = TRUE, check.aes = TRUE,
check.param = TRUE, show.legend = NA, key_glyph = NULL, side = NULL) {

resolve_arg(side, c("x", "y"), null.ok = FALSE)
`_class` <- switch(side, x = "XLayer", y = "YLayer")
Side <- switch(side, x = "Xside", y = "Yside")

names(mapping) <- rename_side(names(mapping), side)

# remaps
geom_aes_map <- aes_to_map(geom, side)
stat_aes_map <- aes_to_map(stat, side)
remap <- union(geom_aes_map, stat_aes_map)
# ggside_geom
geom <- ggside_geom2(paste0(Side, class(geom)[1]),
geom = geom,
side = side)
stat <- ggside_stat(paste0(Side, class(stat)[1]),
stat = stat,
side = side)
layer <- ggplot2::layer(geom = geom, stat = stat,
data = data, mapping = mapping,
position = position,
show.legend = show.legend,
inherit.aes = inherit.aes,
params = params,
check.aes = check.aes,
check.param = check.param,
key_glyph = key_glyph)

new_ggside_layer2(
layer = ggproto(`_class`, layer),
side = side, remap = remap)

}


#' @rdname ggside_layer
#' @param layer a LayerInstance object made from \link[ggplot2]{layer}
#' @param side should the resulting `ggplot2_layer` be configured for x or y side
#' @export
as_ggside_layer <- function(layer, side) UseMethod("as_ggside_layer", layer)

#' @export
as_ggside_layer.ggside_layer <- function(layer, side) layer

#' @export
as_ggside_layer.LayerInstance <- function(layer, side = NULL) {

if (! side %in% c("x", "y")) {
stop("You must specify a side for the layer")
}
geom <- layer$geom
new_geom <- ggside_geom("ggside_psudo_geom", geom, side)
mapping <- force_panel_type_mapping(layer$mapping, side)
names(mapping) <- rename_side(names(mapping), side)
layer$mapping <- mapping
layer$geom <- new_geom
new_ggside_layer(layer, side)

}

new_ggside_layer2 <- function(layer, side, remap, constructor) {
other <- switch(side, x = "y", y = "x")

ggproto(
"ggside_layer",
layer,
setup_layer = new_ggproto_fun(
layer$setup_layer,
{
names(plot$mapping) <- rename_side(names(plot$mapping), !!side)
plot$mapping <- drop_plot_aes(plot$mapping, self$mapping, !!side)
data <- call_parent_method
data[["PANEL_TYPE"]] <- !!side
data
}),
compute_aesthetics = new_ggproto_fun(
layer$compute_aesthetics,
{
data <- call_parent_method
aes_to_drop <- !!other
if (all(paste0(c(!!side, ""), "colour") %in% names(data))) {
aes_to_drop <- c(aes_to_drop, "colour")
}
if (all(paste0(c(!!side, ""), "fill") %in% names(data))) {
aes_to_drop <- c(aes_to_drop, "fill")
}
data[, setdiff(names(data), aes_to_drop), drop = FALSE]
}),
compute_statistic = new_ggproto_fun(
layer$compute_statistic,
{
data <- data_unmap(data, !!side)
data <- call_parent_method
data_map(data, !!side, !!remap)
}),
map_statistic = new_ggproto_fun(
layer$map_statistic,
{
# old_nms <- names(self$stat$default_aes)
# names(self$stat$default_aes) <- rename_side(names(self$stat$default_aes), !!side)
data <- call_parent_method
# names(self$stat$default_aes) <- old_nms
data
}),
compute_geom_1 = new_ggproto_fun(
layer$compute_geom_1,
{
data <- parse_side_aes(data)
self$geom$required_aes <- sub(sprintf("%sside", !!side), "", aes_ <- self$geom$required_aes)
levels <- levels(data$PANEL)
data$PANEL <- droplevels(data$PANEL)
data <- data_unmap(data, !!side)
data <- call_parent_method
data$PANEL <- factor(data$PANEL, levels = levels)
self$geom$required_aes <- aes_
data_map(data, !!side, !!remap)
}
),
draw_geom = new_ggproto_fun(
layer$draw_geom,
{
data <- data_unmap(data, !!side)
data <- call_parent_method
data_map(data, !!side, !!remap)
}
),
compute_position = new_ggproto_fun(
layer$compute_position,
{

data <- data_unmap(data, !!side)
data <- call_parent_method
data_map(data, !!side, !!remap)
})
)
}


drop_plot_aes <- function(plot_map, layer_map, side) {
#if layer mapping fill/colour variant exists
#remove it
p_nms <- names(plot_map)
l_nms <- names(layer_map)
if (paste0(side, "colour") %in% l_nms && any(to_drop <- p_nms %in% "colour"))
plot_map <- plot_map[!to_drop]

if (paste0(side, "fill") %in% l_nms && any(to_drop <- p_nms %in% "fill"))
plot_map <- plot_map[!to_drop]

plot_map
}


