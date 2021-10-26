
calc_element2 <- function (element, theme, verbose = FALSE, skip_blank = FALSE) {
  if (verbose)
    message(element, " --> ", appendLF = FALSE)
  el_out <- theme[[element]]
  if (inherits(el_out, "element_blank")) {
    if (isTRUE(skip_blank)) {
      el_out <- NULL
    }
    else {
      if (verbose)
        message("element_blank (no inheritance)")
      return(el_out)
    }
  }
  element_tree <- get_element_tree()
  if (!is.null(el_out) && !inherits(el_out, element_tree[[element]]$class)) {
    abort(glue("{element} should have class {ggplot_global$element_tree[[element]]$class}"))
  }
  pnames <- element_tree[[element]]$inherit
  if (is.null(pnames)) {
    if (verbose)
      message("nothing (top level)")
    nullprops <- vapply(el_out, is.null, logical(1))
    if (!any(nullprops)) {
      return(el_out)
    }
    return(combine_elements(el_out, ggplot2:::ggplot_global$theme_default[[element]]))

  }
  if (verbose)
    message(paste(pnames, collapse = ", "))
  parents <- lapply(pnames, calc_element2, theme, verbose = verbose,
                    skip_blank = skip_blank || (!is.null(el_out) && !isTRUE(el_out$inherit.blank)))
  Reduce(combine_elements, parents, el_out)
}

panel_guides_grob <- function (guides, position, theme)
{
  guide <- guide_for_position(guides, position) %||% guide_none()
  guide_gengrob(guide, theme)
}

as_ggside_axis <- function(x) {
  if (inherits(x, "guide_none")) return(x)
  class(x) <- c("ggside_axis", class(x))
  x
}

guide_for_position <- function (guides, position)
{
  has_position <- vapply(guides, function(guide) identical(guide$position,
                                                           position), logical(1))
  guides <- guides[has_position]
  guides_order <- vapply(guides, function(guide) as.numeric(guide$order)[1],
                         numeric(1))
  Reduce(guide_merge, guides[order(guides_order)])
}

ggside_panel_guides_grob <- function(guides, position, theme) {
  guide <- guide_for_position(guides, position) %||% guide_none()
  guide <- as_ggside_axis(guide)
  guide_gengrob(guide, theme)
}

#' @importFrom ggplot2 guide_gengrob
#' @export
guide_gengrob.ggside_axis <- function(guide, theme) {
  aesthetic <- names(guide$key)[!grepl("^\\.", names(guide$key))][1]
  draw_ggside_axis(break_positions = guide$key[[aesthetic]], break_labels = guide$key$.label,
                   axis_position = guide$position, theme = theme, check.overlap = guide$check.overlap,
                   angle = guide$angle, n.dodge = guide$n.dodge)
}

axis_label_element_overrides <- function (axis_position, angle = NULL) {
  if (is.null(angle)) {
    return(element_text(angle = NULL, hjust = NULL, vjust = NULL))
  }
  if (angle > 90 || angle < -90) {
    abort("`angle` must be between 90 and -90")
  }
  if (axis_position == "bottom") {
    element_text(angle = angle, hjust = if (angle > 0)
      1
      else if (angle < 0)
        0
      else 0.5, vjust = if (abs(angle) == 90)
        0.5
      else 1)
  }
  else if (axis_position == "left") {
    element_text(angle = angle, hjust = if (abs(angle) ==
                                            90)
      0.5
      else 1, vjust = if (angle > 0)
        0
      else if (angle < 0)
        1
      else 0.5, )
  }
  else if (axis_position == "top") {
    element_text(angle = angle, hjust = if (angle > 0)
      0
      else if (angle < 0)
        1
      else 0.5, vjust = if (abs(angle) == 90)
        0.5
      else 0)
  }
  else if (axis_position == "right") {
    element_text(angle = angle, hjust = if (abs(angle) ==
                                            90)
      0.5
      else 0, vjust = if (angle > 0)
        1
      else if (angle < 0)
        0
      else 0.5, )
  }
  else {
    abort(glue("Unrecognized position: '{axis_position}'"))
  }
}
draw_axis <- function (break_positions, break_labels, axis_position, theme,
          check.overlap = FALSE, angle = NULL, n.dodge = 1)
{
  axis_position <- match.arg(axis_position, c("top", "bottom",
                                              "right", "left"))
  aesthetic <- if (axis_position %in% c("top", "bottom"))
    "x"
  else "y"
  line_element_name <- paste0("axis.line.", aesthetic, ".",
                              axis_position)
  tick_element_name <- paste0("axis.ticks.", aesthetic, ".",
                              axis_position)
  tick_length_element_name <- paste0("axis.ticks.length.",
                                     aesthetic, ".", axis_position)
  label_element_name <- paste0("axis.text.", aesthetic, ".",
                               axis_position)
  line_element <- calc_element(line_element_name, theme)
  tick_element <- calc_element(tick_element_name, theme)
  tick_length <- calc_element(tick_length_element_name, theme)
  label_element <- calc_element(label_element_name, theme)
  if (inherits(label_element, "element_text")) {
    label_overrides <- axis_label_element_overrides(axis_position,
                                                    angle)
    if (!is.null(label_overrides$angle)) {
      label_element$angle <- label_overrides$angle
    }
    if (!is.null(label_overrides$hjust)) {
      label_element$hjust <- label_overrides$hjust
    }
    if (!is.null(label_overrides$vjust)) {
      label_element$vjust <- label_overrides$vjust
    }
  }
  is_vertical <- axis_position %in% c("left", "right")
  position_dim <- if (is_vertical)
    "y"
  else "x"
  non_position_dim <- if (is_vertical)
    "x"
  else "y"
  position_size <- if (is_vertical)
    "height"
  else "width"
  non_position_size <- if (is_vertical)
    "width"
  else "height"
  gtable_element <- if (is_vertical)
    gtable_row
  else gtable_col
  measure_gtable <- if (is_vertical)
    gtable_width
  else gtable_height
  measure_labels_non_pos <- if (is_vertical)
    grobWidth
  else grobHeight
  is_second <- axis_position %in% c("right", "top")
  tick_direction <- if (is_second)
    1
  else -1
  non_position_panel <- if (is_second)
    unit(0, "npc")
  else unit(1, "npc")
  tick_coordinate_order <- if (is_second)
    c(2, 1)
  else c(1, 2)
  labels_first_gtable <- axis_position %in% c("left", "top")
  n_breaks <- length(break_positions)
  opposite_positions <- c(top = "bottom", bottom = "top", right = "left",
                          left = "right")
  axis_position_opposite <- unname(opposite_positions[axis_position])
  line_grob <- exec(element_grob, line_element, `:=`(!!position_dim,
                                                     unit(c(0, 1), "npc")), `:=`(!!non_position_dim, unit.c(non_position_panel,
                                                                                                            non_position_panel)))
  if (n_breaks == 0) {
    return(absoluteGrob(gList(line_grob), width = grobWidth(line_grob),
                        height = grobHeight(line_grob)))
  }
  if (is.list(break_labels)) {
    if (any(vapply(break_labels, is.language, logical(1)))) {
      break_labels <- do.call(expression, break_labels)
    }
    else {
      break_labels <- unlist(break_labels)
    }
  }
  dodge_pos <- rep(seq_len(n.dodge), length.out = n_breaks)
  dodge_indices <- split(seq_len(n_breaks), dodge_pos)
  label_grobs <- lapply(dodge_indices, function(indices) {
    draw_axis_labels(break_positions = break_positions[indices],
                     break_labels = break_labels[indices], label_element = label_element,
                     is_vertical = is_vertical, check.overlap = check.overlap)
  })
  ticks_grob <- exec(element_grob, tick_element, `:=`(!!position_dim,
                                                      rep(unit(break_positions, "native"), each = 2)), `:=`(!!non_position_dim,
                                                                                                            rep(unit.c(non_position_panel + (tick_direction * tick_length),
                                                                                                                       non_position_panel)[tick_coordinate_order], times = n_breaks)),
                     id.lengths = rep(2, times = n_breaks))
  non_position_sizes <- paste0(non_position_size, "s")
  label_dims <- do.call(unit.c, lapply(label_grobs, measure_labels_non_pos))
  grobs <- c(list(ticks_grob), label_grobs)
  grob_dims <- unit.c(max(tick_length, unit(0, "pt")), label_dims)
  if (labels_first_gtable) {
    grobs <- rev(grobs)
    grob_dims <- rev(grob_dims)
  }
  gt <- exec(gtable_element, name = "axis", grobs = grobs,
             `:=`(!!non_position_sizes, grob_dims), `:=`(!!position_size,
                                                         unit(1, "npc")))
  justvp <- exec(viewport, `:=`(!!non_position_dim, non_position_panel),
                 `:=`(!!non_position_size, measure_gtable(gt)), just = axis_position_opposite)
  absoluteGrob(gList(line_grob, gt), width = gtable_width(gt),
               height = gtable_height(gt), vp = justvp)
}

draw_ggside_axis <- function (break_positions, break_labels, axis_position, theme,
                              check.overlap = FALSE, angle = NULL, n.dodge = 1)
{

  axis_position <- match.arg(axis_position, c("top", "bottom",
                                              "right", "left"))
  aesthetic <- if (axis_position %in% c("top", "bottom"))  "x" else "y"

  line_element_name <- paste0("ggside.axis.line.", aesthetic, ".",
                              axis_position)
  tick_element_name <- paste0("ggside.axis.ticks.", aesthetic, ".",
                              axis_position)
  tick_length_element_name <- paste0("ggside.axis.ticks.length.",
                                     aesthetic, ".", axis_position)
  label_element_name <- paste0("ggside.axis.text.", aesthetic, ".",
                               axis_position)
  line_element <- calc_element(line_element_name, theme)
  tick_element <- calc_element(tick_element_name, theme)
  tick_length <- calc_element(tick_length_element_name, theme)
  label_element <- calc_element(label_element_name, theme)
  if (inherits(label_element, "element_text")) {
    label_overrides <- axis_label_element_overrides(axis_position,
                                                    angle)
    if (!is.null(label_overrides$angle)) {
      label_element$angle <- label_overrides$angle
    }
    if (!is.null(label_overrides$hjust)) {
      label_element$hjust <- label_overrides$hjust
    }
    if (!is.null(label_overrides$vjust)) {
      label_element$vjust <- label_overrides$vjust
    }
  }
  is_vertical <- axis_position %in% c("left", "right")
  position_dim <- if (is_vertical)
    "y"
  else "x"
  non_position_dim <- if (is_vertical)
    "x"
  else "y"
  position_size <- if (is_vertical)
    "height"
  else "width"
  non_position_size <- if (is_vertical)
    "width"
  else "height"
  gtable_element <- if (is_vertical)
    gtable_row
  else gtable_col
  measure_gtable <- if (is_vertical)
    gtable_width
  else gtable_height
  measure_labels_non_pos <- if (is_vertical)
    grobWidth
  else grobHeight
  is_second <- axis_position %in% c("right", "top")
  tick_direction <- if (is_second)
    1
  else -1
  non_position_panel <- if (is_second)
    unit(0, "npc")
  else unit(1, "npc")
  tick_coordinate_order <- if (is_second)
    c(2, 1)
  else c(1, 2)
  labels_first_gtable <- axis_position %in% c("left", "top")
  n_breaks <- length(break_positions)
  opposite_positions <- c(top = "bottom", bottom = "top", right = "left",
                          left = "right")
  axis_position_opposite <- unname(opposite_positions[axis_position])
  line_grob <- exec(element_grob, line_element, `:=`(!!position_dim,
                                                     unit(c(0, 1), "npc")), `:=`(!!non_position_dim, unit.c(non_position_panel,
                                                                                                            non_position_panel)))
  if (n_breaks == 0) {
    return(absoluteGrob(gList(line_grob), width = grobWidth(line_grob),
                        height = grobHeight(line_grob)))
  }
  if (is.list(break_labels)) {
    if (any(vapply(break_labels, is.language, logical(1)))) {
      break_labels <- do.call(expression, break_labels)
    }
    else {
      break_labels <- unlist(break_labels)
    }
  }
  dodge_pos <- rep(seq_len(n.dodge), length.out = n_breaks)
  dodge_indices <- split(seq_len(n_breaks), dodge_pos)
  label_grobs <- lapply(dodge_indices, function(indices) {
    draw_axis_labels(break_positions = break_positions[indices],
                     break_labels = break_labels[indices], label_element = label_element,
                     is_vertical = is_vertical, check.overlap = check.overlap)
  })
  ticks_grob <- exec(element_grob, tick_element, `:=`(!!position_dim,
                                                      rep(unit(break_positions, "native"), each = 2)), `:=`(!!non_position_dim,
                                                                                                            rep(unit.c(non_position_panel + (tick_direction * tick_length),
                                                                                                                       non_position_panel)[tick_coordinate_order], times = n_breaks)),
                     id.lengths = rep(2, times = n_breaks))
  non_position_sizes <- paste0(non_position_size, "s")
  label_dims <- do.call(unit.c, lapply(label_grobs, measure_labels_non_pos))
  grobs <- c(list(ticks_grob), label_grobs)
  grob_dims <- unit.c(max(tick_length, unit(0, "pt")), label_dims)
  if (labels_first_gtable) {
    grobs <- rev(grobs)
    grob_dims <- rev(grob_dims)
  }
  gt <- exec(gtable_element, name = "axis", grobs = grobs,
             `:=`(!!non_position_sizes, grob_dims), `:=`(!!position_size,
                                                         unit(1, "npc")))
  justvp <- exec(viewport, `:=`(!!non_position_dim, non_position_panel),
                 `:=`(!!non_position_size, measure_gtable(gt)), just = axis_position_opposite)
  absoluteGrob(gList(line_grob, gt), width = gtable_width(gt),
               height = gtable_height(gt), vp = justvp)
}

absoluteGrob <- function (grob, width = NULL, height = NULL, xmin = NULL, ymin = NULL,
                          vp = NULL) {
  gTree(children = grob, width = width, height = height, xmin = xmin,
        ymin = ymin, vp = vp, cl = "absoluteGrob")
}

draw_axis_labels <- function (break_positions, break_labels, label_element, is_vertical,
                              check.overlap = FALSE) {
  position_dim <- if (is_vertical)
    "y"
  else "x"
  label_margin_name <- if (is_vertical)
    "margin_x"
  else "margin_y"
  n_breaks <- length(break_positions)
  break_positions <- unit(break_positions, "native")
  if (check.overlap) {
    priority <- axis_label_priority(n_breaks)
    break_labels <- break_labels[priority]
    break_positions <- break_positions[priority]
  }
  labels_grob <- exec(element_grob, label_element, `:=`(!!position_dim,
                                                        break_positions), `:=`(!!label_margin_name, TRUE), label = break_labels,
                      check.overlap = check.overlap)
}

combine_elements <- function (e1, e2) {
  if (is.null(e2) || inherits(e1, "element_blank")) {
    return(e1)
  }
  if (is.null(e1)) {
    return(e2)
  }
  if (!inherits(e1, "element") && !inherits(e2, "element")) {
    return(e1)
  }
  if (inherits(e2, "element_blank")) {
    if (e1$inherit.blank) {
      return(e2)
    }
    else {
      return(e1)
    }
  }
  n <- names(e1)[vapply(e1, is.null, logical(1))]
  e1[n] <- e2[n]
  if (inherits(e1$size, "rel")) {
    e1$size <- e2$size * unclass(e1$size)
  }
  e1
}
guide_grid <- function (theme, x.minor, x.major, y.minor, y.major) {
  x.minor <- setdiff(x.minor, x.major)
  y.minor <- setdiff(y.minor, y.major)
  ggname("grill",
         grobTree(element_render(theme, "panel.background"),
                  if (length(y.minor) > 0)
                    element_render(theme, "panel.grid.minor.y",
                                   x = rep(0:1,length(y.minor)), y = rep(y.minor, each = 2),
                                   id.lengths = rep(2, length(y.minor))),
                  if (length(x.minor) > 0)
                    element_render(theme, "panel.grid.minor.x",
                                   x = rep(x.minor, each = 2), y = rep(0:1, length(x.minor)),
                                   id.lengths = rep(2,length(x.minor))),
                  if (length(y.major) > 0)
                    element_render(theme, "panel.grid.major.y",
                                   x = rep(0:1, length(y.major)), y = rep(y.major, each = 2),
                                   id.lengths = rep(2, length(y.major))),
                  if (length(x.major) >  0)
                    element_render(theme, "panel.grid.major.x",
                                   x = rep(x.major,  each = 2), y = rep(0:1, length(x.major)),
                                   id.lengths = rep(2, length(x.major)))))
}


ggside_guide_grid <- function(theme, x.minor, x.major, y.minor, y.major) {
  x.minor <- setdiff(x.minor, x.major)
  y.minor <- setdiff(y.minor, y.major)
  ggname("grill",
         grobTree(element_render(theme, "ggside.panel.background"),
                  if (length(y.minor) > 0)
                    element_render(theme, "ggside.panel.grid.minor.y",
                                   x = rep(0:1,length(y.minor)), y = rep(y.minor, each = 2),
                                   id.lengths = rep(2, length(y.minor))),
                  if (length(x.minor) > 0)
                    element_render(theme, "ggside.panel.grid.minor.x",
                                   x = rep(x.minor, each = 2), y = rep(0:1, length(x.minor)),
                                   id.lengths = rep(2,length(x.minor))),
                  if (length(y.major) > 0)
                    element_render(theme, "ggside.panel.grid.major.y",
                                   x = rep(0:1, length(y.major)), y = rep(y.major, each = 2),
                                   id.lengths = rep(2, length(y.major))),
                  if (length(x.major) >  0)
                    element_render(theme, "ggside.panel.grid.major.x",
                                   x = rep(x.major,  each = 2), y = rep(0:1, length(x.major)),
                                   id.lengths = rep(2, length(x.major)))))

}



ggside_render_fg <- function(panel_params, theme) {
  panel_type <- eval(quote(self$layout[self$layout$PANEL==i,]$PANEL_TYPE), sys.parent(2))
  if (is.element(panel_type, c("x", "y"))) {
    element_render(theme, "ggside.panel.border")
  } else {
    element_render(theme, "panel.border")
  }
}

render_axis <- function (panel_params, axis, scale, position, theme)
{
  if (axis == "primary") {
    draw_axis(panel_params[[paste0(scale, ".major")]], panel_params[[paste0(scale,
                                                                            ".labels")]], position, theme)
  }
  else if (axis == "secondary" && !is.null(panel_params[[paste0(scale,
                                                                ".sec.major")]])) {
    draw_axis(panel_params[[paste0(scale, ".sec.major")]],
              panel_params[[paste0(scale, ".sec.labels")]], position,
              theme)
  }
  else {
    zeroGrob()
  }
}

ggside_render_axis <- function (panel_params, axis, scale, position, theme)
{
  if (axis == "primary") {
    draw_ggside_axis(panel_params[[paste0(scale, ".major")]], panel_params[[paste0(scale,
                                                                            ".labels")]], position, theme)
  }
  else if (axis == "secondary" && !is.null(panel_params[[paste0(scale,
                                                                ".sec.major")]])) {
    draw_ggside_axis(panel_params[[paste0(scale, ".sec.major")]],
              panel_params[[paste0(scale, ".sec.labels")]], position,
              theme)
  }
  else {
    zeroGrob()
  }
}

