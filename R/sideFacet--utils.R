
map_panel_type <- function(panel_params, panel_types) {
  mapply(function(x, y) {x$ggside_panel_type <- y; x}, x = panel_params, y = panel_types, SIMPLIFY = F)
}

eval_facet <- function (facet, data, possible_columns = NULL) {
  if (quo_is_symbol(facet)) {
    facet <- as.character(quo_get_expr(facet))
    if (facet %in% names(data)) {
      out <- data[[facet]]
    }
    else {
      out <- NULL
    }
    return(out)
  }
  env <- new_environment(data)
  missing_columns <- setdiff(possible_columns, names(data))
  undefined_error <- function(e) abort("", class = "ggplot2_missing_facet_var")
  bindings <- rep_named(missing_columns, list(undefined_error))
  env_bind_active(env, !!!bindings)
  mask <- new_data_mask(env)
  mask$.data <- as_data_pronoun(mask)
  tryCatch(eval_tidy(facet, mask), ggplot2_missing_facet_var = function(e) NULL)
}

eval_facets <- function (facets, data, possible_columns = NULL) {
  vars <- compact(lapply(facets, eval_facet, data, possible_columns = possible_columns))
  new_data_frame(tibble::as_tibble(vars))
}
downto <- function (a, b) {
  rev(upto(a, rev(b)))
}
upto <- function (a, b) {
  b[seq_len(match(a, b, nomatch = 0))]
}
reshape_margins <- function (vars, margins = NULL) {
  if (is.null(margins) || identical(margins, FALSE))
    return(NULL)
  all_vars <- unlist(vars)
  if (isTRUE(margins)) {
    margins <- all_vars
  }
  dims <- lapply(vars, intersect, margins)
  dims <- mapply(function(vars, margin) {
    lapply(margin, downto, vars)
  }, vars, dims, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  seq_0 <- function(x) c(0, seq_along(x))
  indices <- expand.grid(lapply(dims, seq_0), KEEP.OUT.ATTRS = FALSE)
  lapply(seq_len(nrow(indices)), function(i) {
    unlist(mapply("[", dims, indices[i, ], SIMPLIFY = FALSE))
  })
}

reshape_add_margins <- function (df, vars, margins = TRUE) {
  margin_vars <- reshape_margins(vars, margins)
  if (length(margin_vars) == 0)
    return(df)
  addAll <- function(x) {
    x <- addNA(x, TRUE)
    factor(x, levels = c(levels(x), "(all)"), exclude = NULL)
  }
  vars <- unique(unlist(margin_vars))
  df[vars] <- lapply(df[vars], addAll)
  rownames(df) <- NULL
  margin_dfs <- lapply(margin_vars, function(vars) {
    df[vars] <- rep(list(factor("(all)")), length(vars))
    df
  })
  do.call("rbind", margin_dfs)
}
