
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
  is_or <- all(grepl("|", str, fixed = T))
  aes <- .ggside_global[[paste0(".", other,"_aes")]]
  if (is_or) {
    splits <- strsplit(str, "|", T)
    i <- match(other, c("x", "y"))

    splits <- lapply(splits,
                     function(x) {
                       l <- x %in% aes
                       x[l] <- paste0(side, "side", x[l])
                       x})
    str <- vapply(splits, paste, character(1), collapse = "|")
  } else {
    to_rename <- str %in% aes
    if (any(to_rename))
      str[to_rename] <- sprintf("%sside%s", side, str[to_rename])
  }
  str
}

#' utility to pull out an aesthetic we care about.
#' helps code around the `|` aesthetics
#' @return a character vector
pull_side <- function(x, i) {
  if (any(is_or <- grepl("|", x, fixed = T))) {
    splits <- strsplit(x[is_or], "|", T)
    out <-vapply(splits, `[`, i, FUN.VALUE = character(1))
    x[is_or] <- out
  }
  x
}

pull_aes <- function(x) {
  if (any(is_or <- grepl("|", x, fixed = T))) {
    splits <- strsplit(x[is_or], "|", T)
    out <- unlist(splits)
    x <- c(x[!is_or], out)
  }
  x
}

#' utility to recode default aesthetics of a geom.
#' @returns an object of class 'uneval'
new_default_aes <- function(geom, side) {
  defaults <- geom$default_aes
  names(defaults) <- rename_side(names(defaults), side)
  new_defaults <- list(NA, NA, PANEL_TYPE = side)
  names(new_defaults)[c(1,2)] <- paste0(side, c("colour", "fill"))
  do.call('aes', c(defaults, new_defaults))
}


#' Temporarily changes any `xside` or `yside` prefix to be removed
#' from scale$aesthetic. Values are returned when exiting the frame this
#' function was called in.
#' @param scale a ggproto Scale object
local_vanilla_scale_aes <- function(scale, frame = parent.frame()) {
  s_quo <- enexpr(scale)
  aes_ <- expr((!!s_quo)$aesthetics)
  old <- eval_bare(expr(!!aes_), frame)
  eval_bare(expr((!!aes_) <- (!!aes_)[!grepl('(x|y)side', !!old)]), frame)
  eval_bare(expr(on.exit((!!aes_) <- !!old, add = T)), frame)
}

#' Temporarily changes the first element of a list to contain the
#' union aesthetics of all other elements
#' @param scales a list containing a ggproto Scale object
local_union_scale_aes <- function(scales, frame = parent.frame()) {
  s_quo <- enexpr(scales)
  aes_ <- expr((!!s_quo)[[1]]$aesthetics)
  old <- eval_bare(expr(!!aes_), frame)
  new <- eval_bare(expr(unique(unlist(lapply(!!s_quo, `[[`, "aesthetics")))), frame)
  eval_bare(expr(!!aes_ <- !!new), frame)
  expr <- expr(on.exit(!!aes_ <- !!old, add = TRUE))
  eval_bare(expr, frame)
}


#' Standardize the panel params such that any `xside` or `yside` prefixes
#' are removed from the object
#' @param panel_params the panel_params element of the Layout ggproto object
#' @param layout the layout element of the Layout ggproto object
standardise_panel_params <- function(built) {
  panel_params <- built$layout$panel_params
  layout <- built$layout$layout
  layout <- layout[layout$PANEL_TYPE!="main",]
  if (nrow(layout)==0) return(panel_params)
  for (i in 1:nrow(layout)) {
    side <- paste0(layout[["PANEL_TYPE"]][i], "side")
    panel <- layout[["PANEL"]][i]
    params <- panel_params[[panel]]
    to_rename <- grep(side, names(params))
    names(params)[to_rename] <- sub(side, "", names(params)[to_rename])
    for (j in to_rename) {
      el <- params[[j]]
      if(is.environment(el))
        el$aesthetics <- sub(side, "", el$aesthetics)
    }
    panel_params[[panel]] <- params
  }
  built$layout$panel_params <- panel_params
  built
}


deprecated_fun <- function(fun) {
  fun_sub <- substitute(fun)
  function(...) {
    .Deprecated(deparse1(fun_sub), package = "ggside")
    fun(...)
  }
}


