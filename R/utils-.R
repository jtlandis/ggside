
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
  do.call('aes', c(defaults, new_defaults))
}
