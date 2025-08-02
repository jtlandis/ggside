### INCLUDE BEGIN
#' @include ggside.R
NULL
### INCLUDE END

#' @title Class Definitions
#' @name class_definitions
#' @description
#' This documentation provides an overview of the S7 and ggproto classes used in the `ggside` package.
#' @rdname class_definitions
#' @section ggproto classes:
#' * `class_ggside_opt` is a subclass of `class_ggproto` and is
#' more described in the [ggside-options] documentation.
#' @export
#' @format NULL
#' @usage NULL
class_ggside_opt <- S7::new_S3_class("ggside_options")


#' @rdname class_definitions
#' @section S7 classes:
#' * `class_ggside` is a subclass of `ggplot2::class_ggplot` and
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
                         ggside_opt = ggside()) {
    old_class <- class(ggplot)
    obj <- S7::new_object(ggplot, ggside_opt = ggside_opt)
    class(obj) <- c(setdiff(class(obj), old_class), old_class)
    as_ggside(obj)
  }
)
