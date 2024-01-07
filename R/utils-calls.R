### INCLUDE BEGIN
#' @include side-facet-ggplot_clones.R
NULL
### INCLUDE END


zap_dots <- function(call, zap = character(), ...) {
  dots <- enexprs(...)
  # remove dots and splice them in
  call <- call_modify(call, ... = zap(), !!!dots)
  if (length(zap) > 0) {
    to_zap <- rep_named(zap, list(zap()))
    call <- call_modify(call, !!!to_zap)
  }
  call
}

call_layer_param_aware <-
  function(expr,
           zap = character(),
           ...,
           env = caller_env()) {
    call <- match.call()$expr
    call <- zap_dots(call, zap = zap, ...)
    layer <- eval(call, envir = env)
    any_zap <- length(zap) > 0
    dot_names <- ...names()
    if (length(intersect(dot_names, zap)) > 0) {
      ind <- which(dot_names %in% zap)
      new_name <- sub("color", "colour", dot_names[ind], fixed = TRUE)
      lst <- vector("list", length(ind))
      for (i in seq_along(lst))
        lst[[i]] <- ...elt(ind[i])
      layer$aes_params[new_name] <- lst
    }
    layer
}

modify_body <- function(call_body, from, to) {
  for (i in seq_along(call_body)) {
    call <- call_body[[i]]
    if(!rlang::is_missing(call)) {
      if(identical(call, from)) {
        call_body[[i]] <- to
      } else if (length(call)>1) {
        call_body[[i]] <- modify_body(call, from, to)
      }
    }
  }
  call_body
}

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
