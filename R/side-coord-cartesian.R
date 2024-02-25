### INCLUDE BEGIN
#' @include utils-side-coord.R
NULL
### INCLUDE END

#'@rdname ggside-ggproto-coord
#'@title Coord Compatible with ggside
#'@description
#' S3 class that converts old Coord into one that
#' is compatible with ggside. Can also update
#' ggside on the object. Typically, the new ggproto
#' will inherit from the object being replaced.
#' @param coord coord ggproto Object to replace
#' @export
ggside_coord <- function(coord) UseMethod("ggside_coord")

#' @rdname ggside-ggproto-coord
#' @export
ggside_coord.default <- function(coord){
  abort(glue("No known method to make {class(coord)[1]} ggside friendly"))
}

#' @rdname ggside-ggproto-coord
#' @export
ggside_coord.CoordCartesian <- function(coord){
  # insure classes that inherit from CoordCartesian fail
  # if there is no S3 method called.
  if (class(coord)[1L]!="CoordCartesian") abort(glue("No known method to make {class(coord)[1]} ggside friendly"))
  ggplot2::ggproto("CoordSide",
                   CoordSideCartesian,
                   limits = coord$limits,
                   expand = coord$expand,
                   default = coord$default,
                   clip = coord$clip)
}

#' @rdname ggside-ggproto-coord
#' @export
ggside_coord.CoordSide <- function(coord) {
  coord
}

CoordSideCartesian <- ggplot2::ggproto(
  "CoordSideCartesian",
  ggplot2::CoordCartesian,
  setup_panel_params = new_ggproto_fun(
    ggplot2::CoordCartesian$setup_panel_params,
    {

      if (!is.null(old_y_lim <- self$limits$y) &
           is.ggside_scale(scale_y)) {
        self$limits$y <- NULL
        out <- call_parent_method
        self$limits$y <- old_y_lim
      } else if (!is.null(old_x_lim <- self$limits$x) &
          is.ggside_scale(scale_x)) {
        self$limits$x <- NULL
        out <- call_parent_method
        self$limits$x <- old_x_lim
      } else {
        out <- call_parent_method
      }
      out
    }),
  render_bg = function(panel_params, theme) {
    panel_type <- panel_params$ggside_panel_type
    if (is.element(panel_type, c("x", "y"))) {
      ggside_guide_grid(
        theme,
        panel_params$x$break_positions_minor(),
        panel_params$x$break_positions(),
        panel_params$y$break_positions_minor(),
        panel_params$y$break_positions(),
        side = panel_type
      )
    } else {
      guide_grid(
        theme,
        panel_params$x$break_positions_minor(),
        panel_params$x$break_positions(),
        panel_params$y$break_positions_minor(),
        panel_params$y$break_positions()
      )
    }
  },
  render_fg = ggside_render_fg,
  render_axis_h = function (panel_params, theme) {
    panel_type <- panel_params$ggside_panel_type
    if (panel_type=="y") {
      list(top = ggside_panel_guides_grob(panel_params$guides, position = "top", theme = theme, labels = panel_params$draw_labels$top),
           bottom = ggside_panel_guides_grob(panel_params$guides, position = "bottom", theme = theme, labels = panel_params$draw_labels$bottom))
    } else {
      list(top = panel_guides_grob(panel_params$guides, position = "top", theme = theme, labels = panel_params$draw_labels$top),
           bottom = panel_guides_grob(panel_params$guides, position = "bottom", theme = theme, labels = panel_params$draw_labels$bottom))

    }
  },
  render_axis_v = function (panel_params, theme) {
    panel_type <- panel_params$ggside_panel_type

    if (panel_type=="x") {
      list(left = ggside_panel_guides_grob(panel_params$guides, position = "left", theme = theme, labels = panel_params$draw_labels$left),
           right = ggside_panel_guides_grob(panel_params$guides, position = "right", theme = theme, labels = panel_params$draw_labels$right))
    } else {
      list(left = panel_guides_grob(panel_params$guides, position = "left", theme = theme, labels = panel_params$draw_labels$left),
           right = panel_guides_grob(panel_params$guides, position = "right", theme = theme, labels = panel_params$draw_labels$right))
    }
  }
)


#' @rdname ggside-ggproto-coord
#' @export
ggside_coord.CoordTrans <- function(coord) {
  ggplot2::ggproto("CoordSide",
                   CoordSideTrans,
                   trans = coord$trans,
                   limits = coord$limits,
                   expand = coord$expand,
                   clip = coord$clip)
}

CoordSideTrans <- ggplot2::ggproto(
  "CoordSideTrans",
  ggplot2::CoordTrans,
  setup_panel_params = new_ggproto_fun(
    ggplot2::CoordTrans$setup_panel_params,
    {
      if (!is.null(old_y_lim <- self$limits$y) &
          is.ggside_scale(scale_y)) {
        self$limits$y <- NULL
        out <- call_parent_method
        self$limits$y <- old_y_lim
      } else if (!is.null(old_x_lim <- self$limits$x) &
                 is.ggside_scale(scale_x)) {
        self$limits$x <- NULL
        out <- call_parent_method
        self$limits$x <- old_x_lim
      } else {
        out <- call_parent_method
      }
      out
    }),
  render_bg = function(panel_params, theme) {
    panel_type <- eval(quote(self$layout[self$layout$PANEL==i,]$PANEL_TYPE), sys.parent(2))
    if (is.element(panel_type, c("x", "y"))) {
      ggside_guide_grid(
        theme,
        panel_params$x.minor,
        panel_params$x.major,
        panel_params$y.minor,
        panel_params$y.major,
        side = panel_type
      )
    } else {
      guide_grid(
        theme,
        panel_params$x.minor,
        panel_params$x.major,
        panel_params$y.minor,
        panel_params$y.major
      )
    }
  },
  render_fg = ggside_render_fg,
  render_axis_h = function (panel_params, theme) {
    panel_type <- panel_params$ggside_panel_type
    if (panel_type=="y") {
      list(top = ggside_panel_guides_grob(panel_params$guides, position = "top", theme = theme, labels = panel_params$draw_labels$top),
           bottom = ggside_panel_guides_grob(panel_params$guides, position = "bottom", theme = theme, labels = panel_params$draw_labels$bottom))
    } else {
      list(top = panel_guides_grob(panel_params$guides, position = "top", theme = theme, labels = panel_params$draw_labels$top),
           bottom = panel_guides_grob(panel_params$guides, position = "bottom", theme = theme, labels = panel_params$draw_labels$bottom))
    }
  },
  render_axis_v = function (panel_params, theme) {
    panel_type <- panel_params$ggside_panel_type
    if (panel_type=="x") {
      list(left = ggside_panel_guides_grob(panel_params$guides, position = "left", theme = theme, labels = panel_params$draw_labels$left),
           right = ggside_panel_guides_grob(panel_params$guides, position = "right", theme = theme, labels = panel_params$draw_labels$right))
    } else {
      list(left = panel_guides_grob(panel_params$guides, position = "left", theme = theme, labels = panel_params$draw_labels$left),
           right = panel_guides_grob(panel_params$guides, position = "right", theme = theme, labels = panel_params$draw_labels$right))
    }
  }
)

#' @rdname ggside-ggproto-coord
#' @export
ggside_coord.CoordFixed <- function(coord){
  # insure classes that inherit from CoordCartesian fail
  # if there is no S3 method called.
  ggplot2::ggproto("CoordSide",
                   CoordSideFixed,
                   limits = coord$limits,
                   ratio = coord$ratio,
                   expand = coord$expand,
                   clip = coord$clip)
}


CoordSideFixed <- ggplot2::ggproto(
  "CoordSideFixed",
  ggplot2::CoordFixed,
  setup_panel_params = new_ggproto_fun(
    ggplot2::CoordFixed$setup_panel_params,
    {

      if (!is.null(old_y_lim <- self$limits$y) &
          is.ggside_scale(scale_y)) {
        self$limits$y <- NULL
        out <- call_parent_method
        self$limits$y <- old_y_lim
      } else if (!is.null(old_x_lim <- self$limits$x) &
                 is.ggside_scale(scale_x)) {
        self$limits$x <- NULL
        out <- call_parent_method
        self$limits$x <- old_x_lim
      } else {
        out <- call_parent_method
      }
      out
    }),
  render_bg = function(panel_params, theme) {
    panel_type <- panel_params$ggside_panel_type
    if (is.element(panel_type, c("x", "y"))) {
      ggside_guide_grid(
        theme,
        panel_params$x.minor,
        panel_params$x.major,
        panel_params$y.minor,
        panel_params$y.major,
        side = panel_type
      )
    } else {
      guide_grid(
        theme,
        panel_params$x.minor,
        panel_params$x.major,
        panel_params$y.minor,
        panel_params$y.major
      )
    }
  },
  render_fg = ggside_render_fg,
  render_axis_h = function (panel_params, theme) {
    panel_type <- panel_params$ggside_panel_type
    if (panel_type=="y") {
      list(top = ggside_panel_guides_grob(panel_params$guides, position = "top", theme = theme, labels = panel_params$draw_labels$top),
           bottom = ggside_panel_guides_grob(panel_params$guides, position = "bottom", theme = theme, labels = panel_params$draw_labels$bottom))
    } else {
      list(top = panel_guides_grob(panel_params$guides, position = "top", theme = theme, labels = panel_params$draw_labels$top),
           bottom = panel_guides_grob(panel_params$guides, position = "bottom", theme = theme, labels = panel_params$draw_labels$bottom))

    }
  },
  render_axis_v = function (panel_params, theme) {
    panel_type <- panel_params$ggside_panel_type
    if (panel_type=="x") {
      list(left = ggside_panel_guides_grob(panel_params$guides, position = "left", theme = theme, labels = panel_params$draw_labels$left),
           right = ggside_panel_guides_grob(panel_params$guides, position = "right", theme = theme, labels = panel_params$draw_labels$right))
    } else {
      list(left = panel_guides_grob(panel_params$guides, position = "left", theme = theme, labels = panel_params$draw_labels$left),
           right = panel_guides_grob(panel_params$guides, position = "right", theme = theme, labels = panel_params$draw_labels$right))
    }
  }
)
