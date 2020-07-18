

#'
.onLoad <- function(libname, pkgname){
  register_theme_elements(
    ggside.panel.scale = 0.1,
    ggside.panel.scale.x = NULL,
    ggside.panel.scale.y = NULL,
    element_tree = list(ggside.panel.scale = el_def("numeric", "numeric"),
                        ggside.panel.scale.x = el_def("numeric", "numeric"),
                        ggside.panel.scale.y = el_def("numeric", "numeric")
  ))
}
