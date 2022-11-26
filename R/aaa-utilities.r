
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

"%|W|%" <- function(a, b) {
  if (!is.waive(a)) a else b
}

force_panel_type_mapping <- function(mapping, type) {
  if ("PANEL_TYPE" %in% names(mapping)) return(mapping)
  switch(type,
         x = aes(!!!mapping, PANEL_TYPE = "x"),
         y = aes(!!!mapping, PANEL_TYPE = "y"))
}

side_pos_scale <- function(scale, side) {
  child_scale <- side %||% scale
  new_scale <- ggproto(
    NULL,
    scale,
    side = child_scale,
    # if `df` contains `PANEL_TYPE` (x/y),
    # redo the method with the parent being
    # the associated side scale
    transform_df = function(self, df) {
      panel_type <- unique(df[["PANEL_TYPE"]])
      if (length(panel_type)==0) {
        ggproto_parent(scale, self)$transform_df(df)
      } else {
        self$side$transform_df(df)
      }
    }
  )
  trans_fun <- new_scale$trans$transform
  #force this scale to always be checked by `scales_transform_df`
  new_scale$trans$transform <- function(x) trans_fun(x)
  new_scale
}

make_new_scale_list <- function(scales, ggside) {

  new_scales <- ggproto(
    NULL,
    scales,
    .sides = list(xsidey = ggside$xsidey, ysidex = ggside$ysidex),
    add = function(self, scale) {

      aes <- scale$aesthetics
      if (any(pos_aes <- aes %in% c("x", "y"))) {
        side_scale <- switch(aes[pos_aes], x = self$.sides$ysidex, y = self$.sides$xsidey)
        scale <- side_pos_scale(scale, side_scale)
      }
      ggproto_parent(scales, self)$add(scale = scale)

    },
    get_scales = function(self, output) {
      s <- ggproto_parent(scales, self)$get_scales(output)
      if (inherits(s, "ScaleContinuousPosition")) {
        f <- s$oob
        s$oob <- muffle_opts_warn(f)
      }
      s
    }
  )

  x_scale <- new_scales$get_scales("x")
  if (!is.null(x_scale)) {
    new_scales$scales[new_scales$find("x")] <- list(side_pos_scale(x_scale, ggside$ysidex))
  }
  y_scale <- new_scales$get_scales("y")
  if (!is.null(y_scale)) {
    new_scales$scales[new_scales$find("y")] <- list(side_pos_scale(y_scale, ggside$xsidey))
  }
  new_scales

}

check_required_aesthetics <- function(required, present, name) {
  if (is.null(required)) return()

  required <- strsplit(required, "|", fixed = TRUE)
  if (any(vapply(required, length, integer(1)) > 1)) {
    required <- lapply(required, rep_len, 2)
    required <- list(
      vapply(required, `[`, character(1), 1),
      vapply(required, `[`, character(1), 2)
    )
  } else {
    required <- list(unlist(required))
  }
  missing_aes <- lapply(required, setdiff, present)
  if (any(vapply(missing_aes, length, integer(1)) == 0)) return()

  abort(glue(
    "{name} requires the following missing aesthetics: ",
    glue_collapse(lapply(missing_aes, glue_collapse, sep = ", ", last = " and "), sep = " or ")
  ))
}

# Concatenate a named list for output
# Print a `list(a=1, b=2)` as `(a=1, b=2)`
#
# @param list to concatenate
# @keyword internal
#X clist(list(a=1, b=2))
#X clist(par()[1:5])
clist <- function(l) {
  paste(paste(names(l), l, sep = " = ", collapse = ", "), sep = "")
}


# Test whether package `package` is available. `fun` provides
# the name of the ggplot2 function that uses this package, and is
# used only to produce a meaningful error message if the
# package is not available.
try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    return(invisible())
  }

  abort(glue("
    Package `{package}` required for `{fun}`.
    Please install and try again.
  "))
}

# Return unique columns
# This is used for figuring out which columns are constant within a group
#
# @keyword internal
uniquecols <- function(df) {
  df <- df[1, sapply(df, function(x) length(unique(x)) == 1), drop = FALSE]
  rownames(df) <- 1:nrow(df)
  df
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

var_list <- function(x) {
  x <- encodeString(x, quote = "`")
  if (length(x) > 5) {
    x <- c(x[1:5], paste0("and ", length(x) - 5, " more"))
  }

  paste0(x, collapse = ", ")
}

dispatch_args <- function(f, ...) {
  args <- list(...)
  formals <- formals(f)
  formals[names(args)] <- args
  formals(f) <- formals
  f
}

is_missing_arg <- function(x) identical(x, quote(expr = ))
# Get all arguments in a function as a list. Will fail if an ellipsis argument
# named .ignore
# @param ... passed on in case enclosing function uses ellipsis in argument list
find_args <- function(...) {
  env <- parent.frame()
  args <- names(formals(sys.function(sys.parent(1))))

  vals <- mget(args, envir = env)
  vals <- vals[!vapply(vals, is_missing_arg, logical(1))]

  modify_list(vals, list(..., `...` = NULL))
}

# Used in annotations to ensure printed even when no
# global data
dummy_data <- function() new_data_frame(list(x = NA), n = 1)

# Check inputs with tibble but allow column vectors (see #2609 and #2374)
as_gg_data_frame <- function(x) {
  x <- lapply(x, validate_column_vec)
  new_data_frame(x)
}

validate_column_vec <- function(x) {
  if (is_column_vec(x)) {
    dim(x) <- NULL
  }
  x
}
is_column_vec <- function(x) {
  dims <- dim(x)
  length(dims) == 2L && dims[[2]] == 1L
}

# Parse takes a vector of n lines and returns m expressions.
# See https://github.com/tidyverse/ggplot2/issues/2864 for discussion.
#
# parse(text = c("alpha", "", "gamma"))
# #> expression(alpha, gamma)
#
# parse_safe(text = c("alpha", "", "gamma"))
# #> expression(alpha, NA, gamma)
#
parse_safe <- function(text) {
  if (!is.character(text)) {
    abort("`text` must be a character vector")
  }
  out <- vector("expression", length(text))
  for (i in seq_along(text)) {
    expr <- parse(text = text[[i]])
    out[[i]] <- if (length(expr) == 0) NA else expr[[1]]
  }
  out
}

switch_orientation <- function(aesthetics) {
  # We should have these as globals somewhere
  x <- ggplot_global$x_aes
  y <- ggplot_global$y_aes
  x_aes <- match(aesthetics, x)
  x_aes_pos <- which(!is.na(x_aes))
  y_aes <- match(aesthetics, y)
  y_aes_pos <- which(!is.na(y_aes))
  if (length(x_aes_pos) > 0) {
    aesthetics[x_aes_pos] <- y[x_aes[x_aes_pos]]
  }
  if (length(y_aes_pos) > 0) {
    aesthetics[y_aes_pos] <- x[y_aes[y_aes_pos]]
  }
  aesthetics
}



split_with_index <- function(x, f, n = max(f)) {
  if (n == 1) return(list(x))
  f <- as.integer(f)
  attributes(f) <- list(levels = as.character(seq_len(n)), class = "factor")
  unname(split(x, f))
}

check_subclass <- function (x, subclass, argname = to_lower_ascii(subclass), env = parent.frame())
{
  if (inherits(x, subclass)) {
    x
  }
  else if (is.character(x) && length(x) == 1) {
    name <- paste0(subclass, camelize(x, first = TRUE))
    obj <- find_global(name, env = env)
    if (is.null(obj) || !inherits(obj, subclass)) {
      abort(glue("Can't find `{argname}` called '{x}'"))
    }
    else {
      obj
    }
  }
  else {
    abort(glue("`{argname}` must be either a string or a {subclass} object, not {obj_desc(x)}"))
  }
}

is.sec_axis <- function(x) inherits(x, "AxisSecondary")

set_sec_axis <- function (sec.axis, scale) {
  if (!is.waive(sec.axis)) {
    if (is.formula(sec.axis))
      sec.axis <- sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis))
      abort("Secondary axes must be specified using 'sec_axis()'")
    scale$secondary.axis <- sec.axis
  }
  return(scale)
}




Range <- ggproto("Range", NULL,
                 range = NULL,
                 reset = function(self) {
                   self$range <- NULL
                 }
)


RangeContinuous <- ggproto("RangeContinuous", Range,
                           train = function(self, x) {
                             self$range <- scales::train_continuous(x, self$range)
                           }
)

continuous_range <- function(){
  ggproto(NULL, RangeContinuous)
}

