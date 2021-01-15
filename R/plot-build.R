
guess_layer_mapping <- function(layer) {
  geom_class <- stringr::str_extract(class(layer$geom), "(X|Y)side")
  val <- if(all(is.na(geom_class))){
    "main"
  } else {
    geom_class <- geom_class[!is.na(geom_class)]
    to_lower_ascii(substr(geom_class,1,1))
  }
  return(val)
}

clone_layers <- function(layers){
  layers <- lapply(layers, function(layer){
   #browser()
    #cl <- grep("(X|Y)side",class(layer$geom))
    l <- cloneLayer(layer)
    g <- l$geom
    fclass <- stringr::str_extract(class(g), "(?<=Geom).+")[1]
    geom <- simple_switch(fclass, default = g)
    layer <- ggproto(NULL, l,
                 compute_aesthetics = aes_compute,
                 geom = geom)

    layer
  })
  layers
}

#' @import ggplot2
#' @export
ggplot_build.ggside <- function(plot){

    plot <- plot_clone(plot)
    if (length(plot$layers) == 0) {
      plot <- plot + geom_blank()
    }

    if(inherits(plot$coordinates, "CoordFlip")||inherits(plot$coordinates, "CoordPolar")){
      abort("ggside is not currently compatable with CoordFlip or CoordPolar")
    }
    browser()
    #plot$layers <- clone_layers(plot$layers)
    layers <- plot$layers
    layer_mappings <- lapply(layers, guess_layer_mapping)
    layer_data <- lapply(layers, function(y) y$layer_data(plot$data))
    sides_used <- unlist(layer_mappings)
    sides_used <- sides_used[!sides_used %in% "main"]
    for(i in seq_along(layer_data)){
      layer_data[[i]]$PANEL_TYPE <- layer_mappings[[i]]
    }
    #browser()
    scales <- plot$scales
    # Apply function to layer and matching data
    by_layer <- function(f) {
      out <- vector("list", length(data))
      for (i in seq_along(data)) {
        out[[i]] <- f(l = layers[[i]], d = data[[i]])
      }
      out
    }

    # Allow all layers to make any final adjustments based
    # on raw input data and plot info
    data <- layer_data
    data <- by_layer(function(l, d) l$setup_layer(d, plot))

    # Initialise panels, add extra data for margins & missing faceting
    # variables, and add on a PANEL variable to data
    pfacet <- if(length(sides_used)==0) {
      plot$facet
    }else{
      make_sideFacets(plot$facet, sides = sides_used, ggside = plot$ggside)
    }
    layout <- ggproto(NULL, Layout, facet = pfacet, coord = plot$coordinates)
    # browser()
    data <- layout$setup(data, plot$data, plot$plot_env)

    # Compute aesthetics to produce data with generalised variable names
    data <- by_layer(function(l, d) l$compute_aesthetics(d, plot))

    # Transform all scales
    data <- lapply(data, scales_transform_df, scales = scales)

    # Map and train positions so that statistics have access to ranges
    # and all positions are numeric
    scale_x <- function() scales$get_scales("x")
    scale_y <- function() scales$get_scales("y")

    layout$train_position(data, scale_x(), scale_y())
    data <- layout$map_position(data)

    # Apply and map statistics
    data <- by_layer(function(l, d) l$compute_statistic(d, layout))
    data <- by_layer(function(l, d) l$map_statistic(d, plot))

    # Make sure missing (but required) aesthetics are added
    scales_add_missing(plot, c("x", "y"), plot$plot_env)

    # Reparameterise geoms from (e.g.) y and width to ymin and ymax
    data <- by_layer(function(l, d) l$compute_geom_1(d))

    # Apply position adjustments
    data <- by_layer(function(l, d) l$compute_position(d, layout))

    # Reset position scales, then re-train and map.  This ensures that facets
    # have control over the range of a plot: is it generated from what is
    # displayed, or does it include the range of underlying data
    layout$reset_scales()
    layout$train_position(data, scale_x(), scale_y())
    layout$setup_panel_params()
    data <- layout$map_position(data)

    # Train and map non-position scales
    npscales <- scales$non_position_scales()
    if (npscales$n() > 0) {
      lapply(data, scales_train_df, scales = npscales)
      data <- lapply(data, scales_map_df, scales = npscales)
    }

    # Fill in defaults etc.
    data <- by_layer(function(l, d) l$compute_geom_2(d))

    # Let layer stat have a final say before rendering
    data <- by_layer(function(l, d) l$finish_statistics(d))

    # Let Layout modify data before rendering
    data <- layout$finish_data(data)

    structure(
      list(data = data, layout = layout, plot = plot),
      class = "ggplot_built"
    )

}
