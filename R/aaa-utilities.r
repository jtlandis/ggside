### INCLUDE BEGIN
#' @include aab-other_utils.r
#' @include compat-plyr.R
#' @include performance.R
#' @include utils-ggproto.R
NULL
### INCLUDE END
NO_PANEL <- -1L
PANEL_TYPE <- c("x","y", "main")
self <- NULL


force_panel_type_mapping <- function(mapping, type) {
  if ("PANEL_TYPE" %in% names(mapping)) return(mapping)
  switch(type,
         x = aes(!!!mapping, PANEL_TYPE = "x"),
         y = aes(!!!mapping, PANEL_TYPE = "y"))
}



remove_missing <- function(df, na.rm = FALSE, vars = names(df), name = "",
                           finite = FALSE) {
  if (!is.logical(na.rm)) {
    abort("`na.rm` must be logical")
  }

  missing <- detect_missing(df, vars, finite)

  if (any(missing)) {
    df <- df[!missing, ]
    if (!na.rm) {
      if (name != "") name <- paste(" (", name, ")", sep = "")
      str <- if (finite) "non-finite" else "missing"
      warning_wrap(
        "Removed ", sum(missing), " rows containing ", str, " values", name, "."
      )
    }
  }

  df
}
detect_missing <- function(df, vars, finite = FALSE) {
  vars <- intersect(vars, names(df))
  !cases(df[, vars, drop = FALSE], if (finite) is_finite else is_complete)
}

# Returns a logical vector of same length as nrow(x). If all data on a row
# is finite (not NA, NaN, Inf, or -Inf) return TRUE; otherwise FALSE.
cases <- function(x, fun) {
  ok <- vapply(x, fun, logical(nrow(x)))

  # Need a special case test when x has exactly one row, because rowSums
  # doesn't respect dimensions for 1x1 matrices. vapply returns a vector (not
  # a matrix when the input has one row.
  if (is.vector(ok)) {
    all(ok)
  } else {
    # Find all the rows where all are TRUE
    rowSums(as.matrix(ok)) == ncol(x)
  }
}

# Wrapper around is.finite to handle list cols
is_finite <- function(x) {
  if (typeof(x) == "list") {
    !vapply(x, is.null, logical(1))
  } else {
    is.finite(x)
  }
}

is_complete <- function(x) {
  if (typeof(x) == "list") {
    !vapply(x, is.null, logical(1))
  } else {
    !is.na(x)
  }
}


should_stop <- function(expr) {
  res <- try(print(force(expr)), TRUE)
  if (!inherits(res, "try-error")) {
    abort("No error!")
  }
  invisible()
}


#' A waiver object.
#'
#' A waiver is a "flag" object, similar to `NULL`, that indicates the
#' calling function should just use the default value.  It is used in certain
#' functions to distinguish between displaying nothing (`NULL`) and
#' displaying a default value calculated elsewhere (`waiver()`)
#'
#' @keywords internal
#' @return simple 'waiver' class as a placeholder
waiver <- function() structure(list(), class = "waiver")

is.waive <- function(x) inherits(x, "waiver")


rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

binned_pal <- function(palette) {
  function(x) {
    palette(length(x))
  }
}



has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    return(rep(FALSE, length(x)))
  }

  !is.na(nms) & nms != ""
}

# Use chartr() for safety since toupper() fails to convert i to I in Turkish locale
lower_ascii <- "abcdefghijklmnopqrstuvwxyz"
upper_ascii <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
to_lower_ascii <- function(x) chartr(upper_ascii, lower_ascii, x)
to_upper_ascii <- function(x) chartr(lower_ascii, upper_ascii, x)

tolower <- function(x) {
  abort("Please use `to_lower_ascii()`, which works fine in all locales.")
}

toupper <- function(x) {
  abort("Please use `to_upper_ascii()`, which works fine in all locales.")
}

# Convert a snake_case string to camelCase
camelize <- function(x, first = FALSE) {
  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
  if (first) x <- firstUpper(x)
  x
}

snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  to_lower_ascii(x)
}

firstUpper <- function(s) {
  paste0(to_upper_ascii(substring(s, 1, 1)), substring(s, 2))
}

snake_class <- function(x) {
  snakeize(class(x)[1])
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || is.waive(df)
}

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

# This function checks that all columns of a dataframe `x` are data and returns
# the names of any columns that are not.
# We define "data" as atomic types or lists, not functions or otherwise.
# The `inherits(x, "Vector")` check is for checking S4 classes from Bioconductor
# and wether they can be expected to follow behavior typical of vectors. See
# also #3835
check_nondata_cols <- function(x) {
  idx <- (vapply(x, function(x) {
    is.null(x) || rlang::is_vector(x) || inherits(x, "Vector")
  }, logical(1)))
  names(x)[which(!idx)]
}

compact <- function(x) {
  null <- vapply(x, is.null, logical(1))
  x[!null]
}

is.formula <- function(x) inherits(x, "formula")

deparse2 <- function(x) {
  y <- deparse(x, backtick = TRUE)
  if (length(y) == 1) {
    y
  } else {
    paste0(y[[1]], "...")
  }
}

message_wrap <- function(...) {
  msg <- paste(..., collapse = "", sep = "")
  wrapped <- strwrap(msg, width = getOption("width") - 2)
  message(paste0(wrapped, collapse = "\n"))
}

warning_wrap <- function(...) {
  msg <- paste(..., collapse = "", sep = "")
  wrapped <- strwrap(msg, width = getOption("width") - 2)
  warn(glue_collapse(wrapped, "\n", last = "\n"))
}








split_with_index <- function(x, f, n = max(f)) {
  if (n == 1) return(list(x))
  f <- as.integer(f)
  attributes(f) <- list(levels = as.character(seq_len(n)), class = "factor")
  unname(split(x, f))
}

check_subclass <- function (x, subclass, argname = to_lower_ascii(subclass), env = parent.frame(), call = caller_env())
{
  if (inherits(x, subclass)) {
    x
  }
  else if (is.character(x) && length(x) == 1) {
    name <- paste0(subclass, camelize(x, first = TRUE))
    obj <- find_global(name, env = env)
    if (is.null(obj) || !inherits(obj, subclass)) {
      cli::cli_abort("Can't find {argname} called {.val {x}}", call = call)
    }
    else {
      obj
    }
  }
  else {
    cli::cli_abort("{argname} must be either a string or a {.cls {subclass}} object", call = call)
  }
}

