

#' @rdname ggside-theme
#' @export
theme_ggside_grey <- function(base_size = 11,
                              base_family = "",
                              base_line_size = base_size/22,
                              base_rect_size = base_size/22) {
  half_line <- base_size/2

  t <- theme(
    ggside.line = element_line(colour = "black", linewidth = base_line_size, linetype = 1, lineend = "butt"),
    ggside.rect = element_rect(fill = "white", colour = "black", linewidth = base_rect_size, linetype = 1),
    ggside.text = element_text(family = base_family, face = "plain", colour = "black",
                               size = base_size, lineheight = 0.9, hjust = 0.5, vjust = 0.5,
                               angle = 0, margin = margin(), debug = FALSE),
    ggside.axis.line = element_blank(),
    ggside.axis.text = element_text(linewidth = rel(0.8), colour = "grey30"),
    ggside.axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), vjust = 1),
    ggside.axis.text.x.top = element_text(margin = margin(b = 0.8 * half_line/2), vjust = 0),
    ggside.axis.text.y = element_text(margin = margin(l = 0.8 * half_line/2), hjust = 1),
    ggside.axis.text.y.right = element_text(margin = margin(l = 0.8 * half_line/2), hjust = 0),
    ggside.axis.ticks = element_line(colour = "grey20"),
    ggside.axis.ticks.length = unit(half_line/2, "pt"),
    ggside.panel.background = element_rect(fill = "grey92", colour = NA),
    ggside.panel.border = element_blank(),
    ggside.panel.grid = element_line(colour = "white"),
    ggside.panel.grid.minor = element_line(linewidth = rel(0.5))
  )

  t

}


#' @rdname ggside-theme
#' @export
theme_ggside_gray <- theme_ggside_grey


#' @rdname ggside-theme
#' @export
theme_ggside_bw <- function(base_size = 11,
                            base_family = "",
                            base_line_size = base_size/22,
                            base_rect_size = base_size/22) {
  theme_ggside_grey(base_size = base_size, base_family = base_family,
                    base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      ggside.panel.background = element_rect(fill = "white", color = NA),
      ggside.panel.border = element_rect(fill = NA, colour = "grey20"),
      ggside.panel.grid = element_line(colour = "grey92"),
      ggside.panel.grid.minor = element_line(linewidth = rel(0.5))
    )
}

#' @rdname ggside-theme
#' @export
theme_ggside_linedraw <- function(base_size = 11,
                            base_family = "",
                            base_line_size = base_size/22,
                            base_rect_size = base_size/22) {
  half_line <- base_size/2
  theme_ggside_bw(base_size = base_size, base_family = base_family,
                    base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      ggside.axis.text = element_text(colour = "black", size = rel(0.8)),
      ggside.axis.ticks = element_line(colour = "black", linewidth = rel(0.5)),
      ggside.panel.border= element_rect(fill = NA, colour = "black", linewidth = rel(1)),
      ggside.panel.grid = element_line(colour = "black"),
      ggside.panel.grid.major = element_line(linewidth = rel(0.1)),
      ggside.panel.grid.minor = element_line(linewidth = rel(0.05))
    )
}

#' @rdname ggside-theme
#' @export
theme_ggside_light <- function(base_size = 11,
                                  base_family = "",
                                  base_line_size = base_size/22,
                                  base_rect_size = base_size/22) {

  theme_ggside_grey(base_size = base_size, base_family = base_family,
                  base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      ggside.panel.background = element_rect(fill = "white", colour = NA),
      ggside.panel.border = element_rect(fill = NA, colour = "grey70", linewidth = rel(1)),
      ggside.panel.grid = element_line(colour = "grey87"),
      ggside.panel.grid.major = element_line(linewidth = rel(0.5)),
      ggside.panel.grid.minor = element_line(linewidth = rel(0.25)),
      ggside.axis.ticks = element_line(colour = "grey70", linewidth = rel(0.5))
    )
}

#' @rdname ggside-theme
#' @export
theme_ggside_dark <- function(base_size = 11,
                                  base_family = "",
                                  base_line_size = base_size/22,
                                  base_rect_size = base_size/22) {

  theme_ggside_grey(base_size = base_size, base_family = base_family,
                  base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      ggside.panel.background = element_rect(fill = "grey50", colour = NA),
      ggside.panel.grid = element_line(colour = "grey42"),
      ggside.panel.grid.major = element_line(linewidth = rel(0.5)),
      ggside.panel.grid.minor = element_line(linewidth = rel(0.25)),
      ggside.axis.ticks = element_line(colour = "grey20", linewidth = rel(0.5))
    )

}

#' @rdname ggside-theme
#' @export
theme_ggside_minimal <- function(base_size = 11,
                                  base_family = "",
                                  base_line_size = base_size/22,
                                  base_rect_size = base_size/22) {

  theme_ggside_bw(base_size = base_size, base_family = base_family,
                  base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      ggside.axis.ticks = element_blank(),
      ggside.panel.border = element_blank()
    )
}

#' @rdname ggside-theme
#' @export
theme_ggside_classic <- function(base_size = 11,
                                 base_family = "",
                                 base_line_size = base_size/22,
                                 base_rect_size = base_size/22) {

  theme_ggside_bw(base_size = base_size, base_family = base_family,
                  base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      ggside.panel.border = element_blank(),
      ggside.panel.grid.major = element_blank(),
      ggside.panel.grid.minor = element_blank(),
      ggside.axis.line = element_line(colour = "black", linewidth = rel(1))
    )

}

#' @rdname ggside-theme
#' @export
theme_ggside_void <- function(base_size = 11,
                                 base_family = "",
                                 base_line_size = base_size/22,
                                 base_rect_size = base_size/22) {
  t <- theme(
    ggside.line = element_blank(),
    ggside.rect = element_blank(),
    ggside.text = element_text(family = base_family, face = "plain", colour = "black",
                               size = base_size, lineheight = 0.9, hjust =0.5, vjust = 0.5,
                               angle = 0, margin = margin(), debug = FALSE),
    ggside.axis.text = element_blank(),
    ggside.axis.ticks.length = unit(0, "pt")
  )
  t

}
