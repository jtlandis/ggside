

get_body_lst <- function(f) {
  b <- body(f)
  if (class(b)=="name" || !identical(b[[1]], as.name("{"))) return(list(b))
  lapply(b[-1], identity)
}


force_expr_vals <- function(expr, env) {
  eval(call("substitute", expr, env))
}

as_ggside_scale <- function(scales, side_scale, side_indx, params) {
  new_scales <- lapply(side_indx, function(i) {
    s <- params$ggside[[side_scale]]$clone()
    s$ggside_sent <- list(map = TRUE, train = TRUE)
    s})
  old_scales <- scales[side_indx]
  # regradless of what the scale was - get its inverse
  invs <- lapply(old_scales, function(x) x$trans$inverse)

  #for each new scale, and previous inverse
  # recontextulize new scale with proper code so data is correct
  Map(function(new, inv) {
    inv_b <- get_body_lst(inv)
    inv_b_force <- lapply(inv_b, force_expr_vals, env = environment(inv))
    .l <- length(inv_b_force)
    inv_b_force[[.l]] <- substitute(x <- .b, env = list(.b = inv_b_force[[.l]]))

    lapply(c("map", "train"), function(fun, this){
      fun_env <- environment(new[[fun]])
      f <- fun_env[["f"]]

      if_body <- rlang::call2("{",!!!inv_b_force,
                              quote(x <- self$trans$transform(x)),
                              substitute(self$ggside_sent[[.fun]] <- FALSE, env = list(.fun = fun)))
      if_call <- call("if", substitute(length(x)>0 && self$ggside_sent[[.fun]], env = list(.fun = fun)), if_body)
      new_body <- rlang::call2("{",
                               if_call,
                               !!!get_body_lst(f))
      body(f) <- new_body
      new[[fun]] <- f
      invisible(TRUE)
    }, this = new)

  }, new_scales, invs)
  new_scales
}
