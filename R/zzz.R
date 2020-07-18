

#'
.onLoad <- function(libname, pkgname){
  register_theme_elements(
    ggside.panel.scale = 0.1,
    ggside.panel.scale.x = NULL,
    ggside.panel.scale.y = NULL,
    ggside.panel.spacing = unit(2,"pt"),
    ggside.panel.spacing.x = NULL,
    ggside.panel.spacing.y = NULL,
    element_tree = list(ggside.panel.scale = el_def("numeric", "numeric"),
                        ggside.panel.scale.x = el_def("numeric", "numeric"),
                        ggside.panel.scale.y = el_def("numeric", "numeric"),
                        ggside.panel.spacing = el_def("unit", "unit"),
                        ggside.panel.spacing.x = el_def("unit", "unit"),
                        ggside.panel.spacing.y = el_def("unit", "unit")
                        )
    )
}
