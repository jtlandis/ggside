### INCLUDE BEGIN
#' @include side-coord-cartesian.R
#' @include side-facet_.R
#' @include side-layout-.r
#' @include ggside.R
NULL
### INCLUDE END

#' @title Class Definitions
#' @name class_definitions
#' @description
#' This documentation provides an overview of the S7 and ggproto classes used in the `ggside` package.
#' @section ggproto classes:
#' * `class_ggside_opt` is a subclass of `class_ggproto` and is
#' more described in the \link[ggside:ggside]{ggside-options} documentation.
#' @export
#' @format NULL
#' @usage NULL
class_ggside_opt <- S7::new_S3_class("ggside_options")

#' @rdname class_definitions
#' @section ggproto classes:
#' * `class_ggside_layer` is a subclass of `class_ggproto` and is
#' more described in the \link[ggside:ggside_layer]{ggside-layers}
#' documentation.
#' @export
#' @format NULL
#' @usage NULL
class_ggside_layer <- S7::new_S3_class("ggside_layer")

#' @rdname class_definitions
#' @section ggproto classes:
#' * `class_ggside_scale` is a subclass of `class_ggproto` and is
#' more described in the \link[ggside:ggside_scales]{ggside-scales}
#' documentation.
#' @export
#' @format NULL
#' @usage NULL
class_ggside_scale <- S7::new_S3_class("ggside_scale")


#' @rdname class_definitions
#' @section S7 classes:
#' * `class_ggside` is a subclass of
#' \link[ggplot2:class_ggplot]{ggplot2's class_ggplot} and
#' is used to represent a ggplot object with ggside options.
#' @export
#' @format NULL
#' @usage NULL
class_ggside <- S7::new_class(
  name = "ggside", parent = ggplot2::class_ggplot,
  package = "ggside",
  properties = list(
    ggside_opt = class_ggside_opt
  ),
  constructor = function(ggplot = NULL,
                         ggside_opt = new_ggside()) {
    old_class <- class(ggplot)
    obj <- S7::new_object(ggplot, ggside_opt = ggside_opt)
    class(obj) <- c(setdiff(class(obj), old_class), old_class)
    S7::set_props(obj,
      facet = ggside_facet(S7::prop(obj, "facet"), ggside = ggside_opt),
      coordinates = ggside_coord(S7::prop(obj, "coordinates")),
      layout = ggside_layout(S7::prop(obj, "layout"))
    )
  }
)
