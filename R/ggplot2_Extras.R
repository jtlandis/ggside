#ggplot2 extras

plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()

  p
}

check_aesthetics <- function(x, n) {
  ns <- vapply(x, length, numeric(1))
  good <- ns == 1L | ns == n

  if (all(good)) {
    return()
  }

  abort(glue(
    "Aesthetics must be either length 1 or the same as the data ({n}): ",
    glue_collapse(names(which(!good)), ", ", last = " and ")
  ))
}

add_group <- function(data) {
  if (empty(data)) return(data)

  if (is.null(data$group)) {
    disc <- vapply(data, is.discrete, logical(1))
    disc[names(disc) %in% c("label", "PANEL")] <- FALSE

    if (any(disc)) {
      data$group <- id(data[disc], drop = TRUE)
    } else {
      data$group <- NO_GROUP
      attr(data$group, "n") <- 1L
    }
  } else {
    data$group <- id(data["group"], drop = TRUE)
  }

  data
}


find_global <- function(name, env, mode = "any") {
  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }

  nsenv <- asNamespace("ggplot2")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }

  NULL
}


aes_compute <- function(self, data, plot) {
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
    evaled$PANEL_TYPE <- "main"
  } else {
    evaled$PANEL <- data$PANEL
    evaled$PANEL_TYPE <- data$PANEL_TYPE
  }
  evaled <- lapply(evaled, unname)
  evaled <- as_gg_data_frame(evaled)
  evaled <- add_group(evaled)
  evaled
}
