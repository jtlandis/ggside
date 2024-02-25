
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

panel_guides_grob <- function (guides, position, theme, labels = NULL)
{
  if (!inherits(guides, "Guides")) {
    return(zeroGrob())
  }
  pair <- guides$get_position(position)
  pair$params$draw_label <- labels %||% NULL
  pair$guide$draw(theme, params = pair$params)
}



clone_guide <- function(guide) {
  ggproto(NULL, guide)
}

ggside_panel_guides_grob <- function(guides, position, theme, labels = NULL) {
  if (!inherits(guides, "Guides")) {
    return(zeroGrob())
  }
  pair <- guides$get_position(position)
  pair$guide <- clone_guide(pair$guide)
  pair$params$draw_label <- labels %||% NULL
  # only use ggside themes if specified...
  ggside_eles <- names(theme)[grep("^ggside", names(theme))]
  to_rename <- apply(vapply(pair$guide$elements,
                            grepl, x = ggside_eles,
                            logical(length(ggside_eles))), 2, any)
  if (any(to_rename)) {
    pair$guide$elements[to_rename] <- paste("ggside", pair$guide$elements[to_rename], sep = ".")
  }
  pair$guide$draw(theme, params = pair$params)
}


use_ggside_ele <- function(ele, side = NULL, family = NULL, theme) {
  theme_nms <- names(theme)

  #most specific
  if (!is.null(side)) {
    .lgl <- grepl(paste("^ggside",side, ele, sep = "\\."), theme_nms)

    if (any(.lgl)) return(paste("ggside", side, ele, sep = "."))
  }


  .lgl <- Reduce(`|`, lapply(paste("^ggside", c(ele, family), sep = "\\."), grepl, x = theme_nms))

  if (any(.lgl))
    return(paste("ggside",ele, sep = "."))
  else
    return(ele)
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


ggside_guide_grid <- function(theme, x.minor, x.major, y.minor, y.major, side = NULL) {
  x.minor <- setdiff(x.minor, x.major)
  y.minor <- setdiff(y.minor, y.major)
  side <- paste0(side, "side")
  ele <- use_ggside_ele("panel.grid", side = side, family = "line", theme = theme)
  ggname("grill",
         grobTree(element_render(theme, use_ggside_ele("panel.background", side = side, family = "rect", theme = theme)),
                  if (length(y.minor) > 0)
                    element_render(theme, paste0(ele, ".minor.y"),
                                   x = rep(0:1,length(y.minor)), y = rep(y.minor, each = 2),
                                   id.lengths = rep(2, length(y.minor))),
                  if (length(x.minor) > 0)
                    element_render(theme, paste0(ele, ".minor.x"),
                                   x = rep(x.minor, each = 2), y = rep(0:1, length(x.minor)),
                                   id.lengths = rep(2,length(x.minor))),
                  if (length(y.major) > 0)
                    element_render(theme, paste0(ele, ".major.y"),
                                   x = rep(0:1, length(y.major)), y = rep(y.major, each = 2),
                                   id.lengths = rep(2, length(y.major))),
                  if (length(x.major) >  0)
                    element_render(theme, paste0(ele, ".major.x"),
                                   x = rep(x.major,  each = 2), y = rep(0:1, length(x.major)),
                                   id.lengths = rep(2, length(x.major)))))

}



ggside_render_fg <- function(panel_params, theme) {
  panel_type <- panel_params$ggside_panel_type
  if (is.element(panel_type, c("x", "y"))) {
    element_render(theme, use_ggside_ele("panel.border", side = paste0(panel_type, "side"), family = "rect", theme = theme))
  } else {
    element_render(theme, "panel.border")
  }
}

