warn_for_aes_extract_usage <- function(mapping, data) {
  lapply(mapping, function(quosure) {
    warn_for_aes_extract_usage_expr(get_expr(quosure), data, get_env(quosure))
  })
}

warn_for_aes_extract_usage_expr <- function(x, data, env = emptyenv()) {
  if (is_call(x, "[[") || is_call(x, "$")) {
    if (extract_target_is_likely_data(x, data, env)) {
      good_usage <- alternative_aes_extract_usage(x)
      warn(glue("Use of `{format(x)}` is discouraged. Use `{good_usage}` instead."))
    }
  } else if (is.call(x)) {
    lapply(x, warn_for_aes_extract_usage_expr, data, env)
  }
}

alternative_aes_extract_usage <- function(x) {
  if (is_call(x, "[[")) {
    good_call <- call2("[[", quote(.data), x[[3]])
    format(good_call)
  } else if (is_call(x, "$")) {
    as.character(x[[3]])
  } else {
    abort(glue("Don't know how to get alternative usage for `{format(x)}`"))
  }
}

extract_target_is_likely_data <- function(x, data, env) {
  if (!is.name(x[[2]])) {
    return(FALSE)
  }

  tryCatch({
    data_eval <- eval_tidy(x[[2]], data, env)
    identical(data_eval, data)
  }, error = function(err) FALSE)
}

NO_GROUP <- -1L

scales_add_defaults <- function(scales, data, aesthetics, env) {
  if (is.null(aesthetics)) return()
  names(aesthetics) <- unlist(lapply(names(aesthetics), aes_to_scale))

  new_aesthetics <- setdiff(names(aesthetics), scales$input())
  # No new aesthetics, so no new scales to add
  if (is.null(new_aesthetics)) return()

  datacols <- lapply(aesthetics[new_aesthetics], eval_tidy, data = data)
  datacols <- compact(datacols)

  for (aes in names(datacols)) {
    scales$add(find_scale(aes, datacols[[aes]], env))
  }

}

find_scale <- function(aes, x, env = parent.frame()) {
  # Inf is ambiguous; it can be used either with continuous scales or with
  # discrete scales, so just skip in the hope that we will have a better guess
  # with the other layers
  if (is.null(x) || (is_atomic(x) && all(is.infinite(x)))) {
    return(NULL)
  }

  type <- scale_type(x)
  candidates <- paste("scale", aes, type, sep = "_")

  for (scale in candidates) {
    scale_f <- find_global(scale, env, mode = "function")
    if (!is.null(scale_f))
      return(scale_f())
  }

  # Failure to find a scale is not an error because some "aesthetics" don't
  # need scales (e.g. group), and it allows others to extend ggplot2 with
  # their own aesthetics

  return(NULL)
}

find_global <- function (name, env, mode = "any") {
  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }
  nsenv <- asNamespace("ggplot2")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }
  NULL
}

check_aesthetics <- function (x, n) {
  ns <- vapply(x, length, numeric(1))
  good <- ns == 1L | ns == n
  if (all(good)) {
    return()
  }
  abort(glue("Aesthetics must be either length 1 or the same as the data ({n}): ",
             glue_collapse(names(which(!good)), ", ", last = " and ")))
}

add_group <- function (data) {
  if (empty(data))
    return(data)
  if (is.null(data$group)) {
    disc <- vapply(data, is.discrete, logical(1))
    disc[names(disc) %in% c("label", "PANEL")] <- FALSE
    if (any(disc)) {
      data$group <- id(data[disc], drop = TRUE)
    }
    else {
      data$group <- NO_GROUP
      attr(data$group, "n") <- 1L
    }
  }
  else {
    data$group <- id(data["group"], drop = TRUE)
  }
  data
}

substitute_aes <- function (x) {
  x <- lapply(x, function(aesthetic) {
    as_quosure(standardise_aes_symbols(quo_get_expr(aesthetic)),
               env = environment(aesthetic))
  })
  class(x) <- "uneval"
  x
}

standardise_aes_symbols <- function (x) {
  if (is.symbol(x)) {
    name <- standardise_aes_names(as_string(x))
    return(sym(name))
  }
  if (!is.call(x)) {
    return(x)
  }
  x[-1] <- lapply(x[-1], standardise_aes_symbols)
  x
}

scales_add_defaults <- function (scales, data, aesthetics, env) {
  if (is.null(aesthetics))
    return()
  names(aesthetics) <- unlist(lapply(names(aesthetics), aes_to_scale))
  new_aesthetics <- setdiff(names(aesthetics), scales$input())
  if (is.null(new_aesthetics))
    return()
  datacols <- lapply(aesthetics[new_aesthetics], eval_tidy,
                     data = data)
  datacols <- compact(datacols)
  for (aes in names(datacols)) {
    scales$add(find_scale(aes, datacols[[aes]], env))
  }
}

aes_to_scale <- function (var) {
  var[var %in% c("x", "xmin", "xmax", "xend", "xintercept")] <- "x"
  var[var %in% c("y", "ymin", "ymax", "yend", "yintercept")] <- "y"
  var
}

rename_aes <- function (x) {
  names(x) <- standardise_aes_names(names(x))
  duplicated_names <- names(x)[duplicated(names(x))]
  if (length(duplicated_names) > 0L) {
    duplicated_message <- paste0(unique(duplicated_names),
                                 collapse = ", ")
    warn(glue("Duplicated aesthetics after name standardisation: {duplicated_message}"))
  }
  x
}

scales_transform_df <- function (scales, df) {
  if (empty(df) || length(scales$scales) == 0)
    return(df)
  transformed <- unlist(lapply(scales$scales, function(s) s$transform_df(df = df)),
                        recursive = FALSE)
  new_data_frame(c(transformed, df[setdiff(names(df), names(transformed))]))
}

cunion <- function (a, b) {
  if (length(a) == 0)
    return(b)
  if (length(b) == 0)
    return(a)
  cbind(a, b[setdiff(names(b), names(a))])
}

Layer <- ggproto("Layer", NULL,
  geom = NULL,
  geom_params = NULL,
  stat = NULL,
  stat_params = NULL,
  data = NULL,
  aes_params = NULL,
  mapping = NULL,
  position = NULL,
  inherit.aes = FALSE,

  print = function(self) {
    if (!is.null(self$mapping)) {
      cat("mapping:", clist(self$mapping), "\n")
    }
    cat(snakeize(class(self$geom)[[1]]), ": ", clist(self$geom_params), "\n",
      sep = "")
    cat(snakeize(class(self$stat)[[1]]), ": ", clist(self$stat_params), "\n",
      sep = "")
    cat(snakeize(class(self$position)[[1]]), "\n")
  },

  layer_data = function(self, plot_data) {
    if (is.waive(self$data)) {
      plot_data
    } else if (is.function(self$data)) {
      data <- self$data(plot_data)
      if (!is.data.frame(data)) {
        abort("Data function must return a data.frame")
      }
      data
    } else {
      self$data
    }
  },

  # hook to allow a layer access to the final layer data
  # in input form and to global plot info
  setup_layer = function(self, data, plot) {
    data
  },

  compute_aesthetics = function(self, data, plot) {
    # For annotation geoms, it is useful to be able to ignore the default aes
    if (self$inherit.aes) {
      aesthetics <- defaults(self$mapping, plot$mapping)
    } else {
      aesthetics <- self$mapping
    }

    # Drop aesthetics that are set or calculated
    set <- names(aesthetics) %in% names(self$aes_params)
    calculated <- is_calculated_aes(aesthetics)
    modifiers <- is_scaled_aes(aesthetics)

    aesthetics <- aesthetics[!set & !calculated & !modifiers]

    # Override grouping if set in layer
    if (!is.null(self$geom_params$group)) {
      aesthetics[["group"]] <- self$aes_params$group
    }

    scales_add_defaults(plot$scales, data, aesthetics, plot$plot_env)

    # Evaluate aesthetics
    env <- child_env(baseenv(), stage = stage)
    evaled <- lapply(aesthetics, eval_tidy, data = data, env = env)
    evaled <- compact(evaled)

    # Check for discouraged usage in mapping
    warn_for_aes_extract_usage(aesthetics, data[setdiff(names(data), "PANEL")])

    # Check aesthetic values
    nondata_cols <- check_nondata_cols(evaled)
    if (length(nondata_cols) > 0) {
      msg <- paste0(
        "Aesthetics must be valid data columns. Problematic aesthetic(s): ",
        paste0(vapply(nondata_cols, function(x) {paste0(x, " = ", as_label(aesthetics[[x]]))}, character(1)), collapse = ", "),
        ". \nDid you mistype the name of a data column or forget to add after_stat()?"
      )
      abort(msg)
    }

    n <- nrow(data)
    if (n == 0) {
      # No data, so look at longest evaluated aesthetic
      if (length(evaled) == 0) {
        n <- 0
      } else {
        n <- max(vapply(evaled, length, integer(1)))
      }
    }
    check_aesthetics(evaled, n)

    # Set special group and panel vars
    if (empty(data) && n > 0) {
      evaled$PANEL <- 1
    } else {
      evaled$PANEL <- data$PANEL
    }
    evaled <- lapply(evaled, unname)
    evaled <- as_gg_data_frame(evaled)
    evaled <- add_group(evaled)
    evaled
  },

  compute_statistic = function(self, data, layout) {
    if (empty(data))
      return(new_data_frame())

    params <- self$stat$setup_params(data, self$stat_params)
    data <- self$stat$setup_data(data, params)
    self$stat$compute_layer(data, params, layout)
  },

  map_statistic = function(self, data, plot) {
    if (empty(data)) return(new_data_frame())

    # Make sure data columns are converted to correct names. If not done, a
    # column with e.g. a color name will not be found in an after_stat()
    # evaluation (since the evaluation symbols gets renamed)
    data <- rename_aes(data)

    # Assemble aesthetics from layer, plot and stat mappings
    aesthetics <- self$mapping
    if (self$inherit.aes) {
      aesthetics <- defaults(aesthetics, plot$mapping)
    }
    aesthetics <- defaults(aesthetics, self$stat$default_aes)
    aesthetics <- compact(aesthetics)

    new <- strip_dots(aesthetics[is_calculated_aes(aesthetics) | is_staged_aes(aesthetics)])
    if (length(new) == 0) return(data)

    # Add map stat output to aesthetics
    env <- child_env(baseenv(), stat = stat, after_stat = after_stat)
    stage_mask <- child_env(emptyenv(), stage = stage_calculated)
    mask <- new_data_mask(as_environment(data, stage_mask), stage_mask)
    mask$.data <- as_data_pronoun(mask)

    new <- substitute_aes(new)
    stat_data <- lapply(new, eval_tidy, mask, env)

    # Check that all columns in aesthetic stats are valid data
    nondata_stat_cols <- check_nondata_cols(stat_data)
    if (length(nondata_stat_cols) > 0) {
      msg <- paste0(
        "Aesthetics must be valid computed stats. Problematic aesthetic(s): ",
        paste0(vapply(nondata_stat_cols, function(x) {paste0(x, " = ", as_label(aesthetics[[x]]))}, character(1)), collapse = ", "),
        ". \nDid you map your stat in the wrong layer?"
      )
      abort(msg)
    }

    names(stat_data) <- names(new)
    stat_data <- new_data_frame(compact(stat_data))

    # Add any new scales, if needed
    scales_add_defaults(plot$scales, data, new, plot$plot_env)
    # Transform the values, if the scale say it's ok
    # (see stat_spoke for one exception)
    if (self$stat$retransform) {
      stat_data <- scales_transform_df(plot$scales, stat_data)
    }

    cunion(stat_data, data)
  },

  compute_geom_1 = function(self, data) {
    if (empty(data)) return(new_data_frame())

    check_required_aesthetics(
      self$geom$required_aes,
      c(names(data), names(self$aes_params)),
      snake_class(self$geom)
    )
    self$geom_params <- self$geom$setup_params(data, c(self$geom_params, self$aes_params))
    self$geom$setup_data(data, self$geom_params)
  },

  compute_position = function(self, data, layout) {
    if (empty(data)) return(new_data_frame())

    params <- self$position$setup_params(data)
    data <- self$position$setup_data(data, params)

    self$position$compute_layer(data, params, layout)
  },

  compute_geom_2 = function(self, data) {
    # Combine aesthetics, defaults, & params
    if (empty(data)) return(data)

    aesthetics <- self$mapping
    modifiers <- aesthetics[is_scaled_aes(aesthetics) | is_staged_aes(aesthetics)]

    self$geom$use_defaults(data, self$aes_params, modifiers)
  },

  finish_statistics = function(self, data) {
    self$stat$finish_layer(data, self$stat_params)
  },

  draw_geom = function(self, data, layout) {
    if (empty(data)) {
      n <- nrow(layout$layout)
      return(rep(list(zeroGrob()), n))
    }

    data <- self$geom$handle_na(data, self$geom_params)
    self$geom$draw_layer(data, self$geom_params, layout, layout$coord)
  }
)

is.layer <- function(x) inherits(x, "Layer")


obj_desc <- function(x) {
  if (isS4(x)) {
    paste0("an S4 object with class ", class(x)[[1]])
  } else if (is.object(x)) {
    if (is.data.frame(x)) {
      "a data frame"
    } else if (is.factor(x)) {
      "a factor"
    } else {
      paste0("an S3 object with class ", paste(class(x), collapse = "/"))
    }
  } else {
    switch(typeof(x),
      "NULL" = "a NULL",
      character = "a character vector",
      integer = "an integer vector",
      logical = "a logical vector",
      double = "a numeric vector",
      list = "a list",
      closure = "a function",
      paste0("a base object of type", typeof(x))
    )
  }
}

