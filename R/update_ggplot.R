### INCLUDE BEGIN
#' @include all_classes.r
#' @include ggplot_add.R
#' @include ggside.R
#' @include plot-construction.R
#' @include side-coord-cartesian.R
#' @include utils-.R
NULL
### INCLUDE END

# update_ggside_opt <- S7::new_generic(
#   "update_ggside_opt",
#   dispatch_args = c("object", "ggside_opt")
# )

# S7::method(
#   update_ggside_opt,
#   list(class_ggside_layer, class_ggside_opt)
# ) <-
#   function(object, ggside_opt) {
#     ggside_opt$sides_used <- get_sides(gg)
#     NULL
#   }

layer_update_plot <- S7::method(
  update_ggplot,
  class = list(class_layer, class_ggplot)
)

# whenever we add a ggside object to a ggplot, we
# will get a new ggside object

S7::method(
  update_ggplot,
  list(class_ggside_opt, class_ggplot)
) <-
  function(object, plot, ...) {
    # incase the user just adds a vanilla ggside object
    # we need to make sure we update NULL values to defaults
    class_ggside(plot, ggside_opt = new_ggside()) |>
      update_ggside(ggside = object)
  }

S7::method(
  update_ggplot,
  list(class_ggside_opt, class_ggside)
) <-
  function(object, plot, ...) {
    clone_ggside_plot(plot) |>
      update_ggside(ggside = object)
  }

S7::method(
  update_ggplot,
  list(class_ggside_layer, class_ggplot)
) <-
  function(object, plot, ...) {
    ggside_opt <- new_ggside()
    ggside_opt$sides_used <- layer_type(object)
    layer_update_plot(object = object, plot = plot, ...) |>
      class_ggside(ggside_opt = ggside_opt)
  }

S7::method(
  update_ggplot,
  list(class_ggside_layer, class_ggside)
) <-
  function(object, plot, ...) {
    plot <- clone_ggside_plot(plot)
    plot <- layer_update_plot(object = object, plot = plot, ...)
    ggside_opt <- S7::prop(plot, "ggside_opt")
    ggside_opt$sides_used <- get_sides(S7::prop(plot, "layers"))
    plot
  }

S7::method(
  update_ggplot,
  list(class_ggside_scale, class_ggplot)
) <-
  function(object, plot, ...) {
    ggside_opt <- new_ggside()
    member <- intersect(c("xsidey", "ysidex"), object$aesthetics)
    ggside_opt[[member]] <- object
    new_scale <- object$clone()
    new_scale$guide <- waiver()
    plot$scales$add(new_scale)
    plot
  }


S7::method(
  update_ggplot,
  list(class_ggside_scale, class_ggside)
) <-
  function(object, plot, ...) {
    plot <- clone_ggside_plot(plot)
    ggside_opt <- S7::prop(plot, "ggside_opt")
    member <- intersect(c("xsidey", "ysidex"), object$aesthetics)
    ggside_opt[[member]] <- object
    new_scale <- object$clone()
    new_scale$guide <- waiver()
    plot$scales$add(new_scale)
    plot
  }

S7::method(
  update_ggplot,
  list(class_facet, class_ggside)
) <-
  function(object, plot, ...) {
    plot <- update_ggplot(
      object = object,
      plot = S7::super(plot, class_ggplot),
      ...
    )
    clone_ggside_plot(plot)
  }

S7::method(
  update_ggplot,
  list(S7::class_any, class_ggside)
) <-
  function(object, plot, ...) {
    plot <- clone_ggside_plot(plot)
    update_ggside(
      object = object,
      plot = S7::super(plot, class_ggplot),
      ...
    )
  }

S7::method(
  update_ggplot,
  list(class_coord, class_ggside)
) <- function(object, plot, ...) {
  plot <- clone_ggside_plot(plot)
  update_ggplot(
    object = ggside_coord(object),
    plot = S7::super(plot, class_ggplot),
    ...
  )
}


S7::method(
  ggplot_build,
  class_ggside
) <- function(plot, ...) {
  plot <- clone_ggside_plot(plot)
  NextMethod()
}
