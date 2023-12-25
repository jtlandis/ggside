
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

mod_fun_at <- function(fun, insert, at) {
  body(fun) <- insert_call_at(body(fun), insert, at)
  fun
}

insert_call_at <- function(call, insert, at) {
  stopifnot("`call` isnt a call"=is.call(call),
            "`at` isnt integer"=is.numeric(at))
  len <- length(call)
  at <- as.integer(at)
  if (at < 0) {
    at <- len + at
  }
  if (at <= 0) {
    at <- 1L
  } else if (at > len) {
    at <- len
  }

  seq_args <- seq_along(call)[-1]
  seq_upto <- seq_args[seq_len(at-1)]
  seq_after <- setdiff(seq_args, seq_upto)

  new_call <- as.call(list(call[[1]]))
  for (i in seq_upto) {
    new_call[[i]] <- call[[i]]
  }
  new_call[[at+1]] <- insert

  for (i in seq_after) {
    new_call[[i + 1]] <- call[[i]]
  }

  new_call

}

browse_fun <- function(fun, at = 1) {
  body(fun) <- insert_call_at(body(fun), quote(browser()), at)
  fun
}

new_side_layout <- function(layout) {
  parent_layout <- layout

  ggproto(
    NULL,
    parent_layout,
    map_position = mod_ggproto_fun(
      parent_layout$map_position,
      self$panel_scales_x[[1]]$aesthetics ~ unique(unlist(lapply(self$panel_scales_x, `[[`, "aesthetics"))),
      self$panel_scales_y[[1]]$aesthetics ~ unique(unlist(lapply(self$panel_scales_y, `[[`, "aesthetics")))
      ),
    setup_panel_params = mod_ggproto_fun(parent_layout$setup_panel_params) |>
      mod_fun_at(quote(self$panel_params <- Map(\(param, type) {
        param$ggside_panel_type <- type
        names(param) <- sub("(x|y)side","",names(param))
        is_proto <- vapply(param, is.ggproto, logical(1))
        param[is_proto] <- lapply(param[is_proto],\(x) {x$aesthetics <- sub("(x|y)side", "", x$aesthetics);x})
        param},
        self$panel_params, self$layout$PANEL_TYPE)), at = -1),
    setup_panel_guides = mod_ggproto_fun(parent_layout$setup_panel_guides)
  )
}
