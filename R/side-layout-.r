
list_of_calls <- function(x) {
  vapply(x, function(y) is.call(y) || is.name(y) || (length(y)==1 && is.character(y)), logical(1))
}

mod_ggproto_fun <- function(ggproto_method, ...) {
  call <- match.call(expand.dots = F)
  formulas <- vapply(call$..., rlang::is_formula, logical(1))
  if (!all(formulas)) stop("all `...` should be formulas")
  proto_env <- environment(ggproto_method)
  body <- body(proto_env$f)
  len <- ...length()
  for (i in seq_len(len)) {
    spec <- ...elt(i)
    body <- modify_body(body, spec[[2]], spec[[3]])
  }
  rlang::new_function(
    args = formals(proto_env$f),
    body = body,
    env = proto_env)
}

new_side_layout <- function(layout) {
  parent_layout <- layout

  ggproto(
    NULL,
    parent_layout,
    map_position = mod_ggproto_fun(
      parent_layout$map_position,
      self$panel_scales_x[[1]]$aesthetics ~ unique(unlist(lapply(self$panel_sales_x, `[[`, "aesthetics"))),
      self$panel_scales_y[[1]]$aesthetics ~ unique(unlist(lapply(self$panel_sales_y, `[[`, "aesthetics")))
      ),
    setup_panel_params = function(self) {
      browser()
      ggproto_parent(parent_layout, self)$setup_panel_params()
      invisible()
    }
  )
}
