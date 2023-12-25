
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

insert_call_at <- function(call, insert, at) {
  stopifnot("`call` isnt a call"=is.call(call),
            "`at` isnt numeric/integer"=is.numeric(at))
  len <- length(call)
  if (at > len) {
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
    train_position = function(self, data, x_scale, y_scale) {
      parent <- ggproto_parent(parent_layout, self)
      out <- parent$train_position(data, x_scale, y_scale)
      out
    },
    setup_panel_params = function(self) {
      browser()
      parent <- ggproto_parent(parent_layout, self)
      out <- parent$setup_panel_params()
      invisible()
    }
  )
}
