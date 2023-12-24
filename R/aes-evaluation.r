

after_stat <- function(x) {
  x
}

stat <- function(x) {
  x
}

after_scale <- function(x) {
  x
}

stage <- function(start = NULL, after_stat = NULL, after_scale = NULL) {
  start
}
stage_calculated <- function(start = NULL, after_stat = NULL, after_scale = NULL) {
  after_stat
}
stage_scaled <- function(start = NULL, after_stat = NULL, after_scale = NULL) {
  after_scale
}

# Regex to determine if an identifier refers to a calculated aesthetic
match_calculated_aes <- "^\\.\\.([a-zA-Z._]+)\\.\\.$"

is_dotted_var <- function(x) {
  grepl(match_calculated_aes, x)
}

# Determine if aesthetic is calculated
is_calculated_aes <- function(aesthetics) {
  vapply(aesthetics, is_calculated, logical(1), USE.NAMES = FALSE)
}
is_scaled_aes <- function(aesthetics) {
  vapply(aesthetics, is_scaled, logical(1), USE.NAMES = FALSE)
}
is_staged_aes <- function(aesthetics) {
  vapply(aesthetics, is_staged, logical(1), USE.NAMES = FALSE)
}
is_calculated <- function(x) {
  if (is_call(get_expr(x), "after_stat")) {
    return(TRUE)
  }
  # Support of old recursive behaviour
  if (is.atomic(x)) {
    FALSE
  } else if (is.symbol(x)) {
    is_dotted_var(as.character(x))
  } else if (is_quosure(x)) {
    is_calculated(quo_get_expr(x))
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(stat))) {
      TRUE
    } else {
      any(vapply(x, is_calculated, logical(1)))
    }
  } else if (is.pairlist(x)) {
    FALSE
  } else {
    abort(glue("Unknown input: {class(x)[1]}"))
  }
}
is_scaled <- function(x) {
  is_call(get_expr(x), "after_scale")
}
is_staged <- function(x) {
  is_call(get_expr(x), "stage")
}

# Strip dots from expressions
strip_dots <- function(expr, env, strip_pronoun = FALSE) {
  if (is.atomic(expr)) {
    expr
  } else if (is.name(expr)) {
    expr_ch <- as.character(expr)
    if (nchar(expr_ch) > 0) {
      as.name(gsub(match_calculated_aes, "\\1", expr_ch))
    } else {
      expr
    }
  } else if (is_quosure(expr)) {
    # strip dots from quosure and reconstruct the quosure
    new_quosure(
      strip_dots(quo_get_expr(expr), env = quo_get_env(expr), strip_pronoun = strip_pronoun),
      quo_get_env(expr)
    )
  } else if (is.call(expr)) {
    if (strip_pronoun && is_call(expr, "$") && is_symbol(expr[[2]], ".data")) {
      strip_dots(expr[[3]], env, strip_pronoun = strip_pronoun)
    } else if (strip_pronoun && is_call(expr, "[[") && is_symbol(expr[[2]], ".data")) {
      tryCatch(
        sym(eval(expr[[3]], env)),
        error = function(e) expr[[3]]
      )
    } else if (is_call(expr, "stat")) {
      strip_dots(expr[[2]], env, strip_pronoun = strip_pronoun)
    } else {
      expr[-1] <- lapply(expr[-1], strip_dots, env = env, strip_pronoun = strip_pronoun)
      expr
    }
  } else if (is.pairlist(expr)) {
    # In the unlikely event of an anonymous function
    as.pairlist(lapply(expr, strip_dots, env = env, strip_pronoun = strip_pronoun))
  } else if (is.list(expr)) {
    # For list of aesthetics
    lapply(expr, strip_dots, env = env, strip_pronoun = strip_pronoun)
  } else {
    abort(glue("Unknown input: {class(expr)[1]}"))
  }
}

strip_stage <- function(expr) {
  uq_expr <- get_expr(expr)
  if (is_call(uq_expr, c("after_stat", "after_scale"))) {
    uq_expr[[2]]
  } else if (is_call(uq_expr, "stage")) {
    # Prefer stat mapping if present, otherwise original mapping (fallback to
    # scale mapping) but there should always be two arguments to stage()
    uq_expr$after_stat %||% uq_expr$start %||% (if (is.null(uq_expr$after_scale)) uq_expr[[3]]) %||% uq_expr[[2]]
  } else {
    expr
  }
}

# Convert aesthetic mapping into text labels
make_labels <- function(mapping) {
  default_label <- function(aesthetic, mapping) {
    # e.g., geom_smooth(aes(colour = "loess")) or aes(y = NULL)
    if (is.atomic(mapping)) {
      return(aesthetic)
    }
    mapping <- strip_stage(mapping)
    mapping <- strip_dots(mapping, strip_pronoun = TRUE)
    if (is_quosure(mapping) && quo_is_symbol(mapping)) {
      name <- as_string(quo_get_expr(mapping))
    } else {
      name <- quo_text(mapping)
      name <- gsub("\n.*$", "...", name)
    }
    name
  }
  Map(default_label, names(mapping), mapping)
}


