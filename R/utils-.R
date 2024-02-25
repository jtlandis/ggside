### INCLUDE BEGIN
#' @include constructor-.R
#' @include utils-ggplot2-reimpl-.R
NULL
### INCLUDE END

#global variables to pass RMD checks
NO_PANEL <- -1L
PANEL_TYPE <- c("x","y", "main")
self <- NULL
call_parent_method <- function(...) cli::cli_abort("method not implemented")
orientation <- NULL
`!<-` <- `(<-` <- function(x, value) cli::cli_abort("function not meant to be called")

force_panel_type_mapping <- function(mapping, type) {
  if ("PANEL_TYPE" %in% names(mapping)) return(mapping)
  switch(type,
         x = aes(!!!mapping, PANEL_TYPE = "x"),
         y = aes(!!!mapping, PANEL_TYPE = "y"))
}

.ggside_global <- new.env(parent = emptyenv())
.ggside_global$.y_aes <- c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final",
                           "ymax_final", "lower", "middle", "upper", "y0")
.ggside_global$.x_aes <- c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final",
                           "xmax_final", "xlower", "xmiddle", "xupper", "x0")

`%NA%` <- function(a, b) {
  if(all(is.na(a))) b else a
}

use_side_aes <- function(data, side) {
  data[["fill"]] <- data[[sprintf("%sfill", side)]] %NA% data[["fill"]]
  data[["colour"]] <- data[[sprintf("%scolour", side)]] %NA% data[["colour"]]
  data
}

rename_side <- function(str, side) {
  other <- switch(side, x = "y", y = "x")
  is_or <- grepl("|", str, fixed = T)
  aes <- .ggside_global[[paste0(".", other,"_aes")]]
  rename_aes <- function(x) {
    to_rename <- x %in% aes
    if (any(to_rename))
      x[to_rename] <- sprintf("%sside%s", side, x[to_rename])
    x
  }
  if (any(is_or)) {
    or <- str[is_or]
    splits <- strsplit(or, "|", T)
    splits <- lapply(splits, rename_aes)
    str[!is_or] <- rename_aes(str[!is_or])
    str[is_or] <- vapply(splits, paste, character(1), collapse = "|")
  } else {
    str <- rename_aes(str)
  }
  str
}


# utility to pull out an aesthetic we care about.
# helps code around the `|` aesthetics
# @return a character vector
pull_aes <- function(x) {
  if (any(is_or <- grepl("|", x, fixed = T))) {
    splits <- strsplit(x[is_or], "|", T)
    out <- unlist(splits)
    x <- c(x[!is_or], out)
  }
  x
}


# utility to recode default aesthetics of a geom.
# @returns an object of class 'uneval'
new_default_aes <- function(geom, side) {
  defaults <- geom$default_aes
  names(defaults) <- rename_side(names(defaults), side)
  new_defaults <- list(NA, NA, PANEL_TYPE = side)
  names(new_defaults)[c(1,2)] <- paste0(side, c("colour", "fill"))
  args <- dots_list(!!!defaults, !!!new_defaults, .homonyms = "first")
  do.call('aes', args)
}

assert_lgl <- function(arg) {
  arg_sym <- caller_arg(arg)
  vctrs::vec_assert(x = arg,
                    ptype = logical(), size = 1L,
                    arg = arg_sym,
                    call = parent.frame())
  if (is.na(arg))
    cli::cli_abort("{.arg {arg_sym}} cannot be {.obj_type_friendly {NA}}", call = parent.frame())

}

resolve_arg <- function(arg, opt, several.ok = FALSE, null.ok = TRUE) {
  assert_lgl(several.ok)
  assert_lgl(null.ok)
  arg_sym <- caller_arg(arg)
  if (!is.null(arg)) {
    arg <- opt[opt %in% arg]
    len <- length(arg)
    opt_len <- length(opt)
    if (len==0)
      cli::cli_abort("valid {cli::qty(opt_len)} option{?s} for argument {.arg {arg_sym}} {?is/are} {.val {opt}}",
                     call = parent.frame())
    if (length(arg)>1 && !several.ok)
      cli::cli_abort("you specified {length(arg)} value{?s} to argument {.arg {arg_sym}}, but only one of {.or {.val {opt}}} are allowed",
                     call = parent.frame())
  } else if (!null.ok) {
    cli::cli_abort("argument {.arg {arg_sym}} cannot be {.obj_type_friendly {NULL}}", call = parent.frame())
  }
  arg
}

layer_type <- function(layer) {
  layer_class <- str_extr(class(layer), "(X|Y)Layer")
  val <- if(all(is.na(layer_class))){
    "main"
  } else {
    layer_class <- layer_class[!is.na(layer_class)]
    to_lower_ascii(substr(layer_class,1,1))
  }
  return(val)
}

is_ggside_subclass <- function(obj) {
  class_ <- class(obj)
  any(grepl("((X|Y)Layer|(X|Y)side)", class_))
}
