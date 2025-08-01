### INCLUDE BEGIN
#' @include utils-ggplot2-reimpl-.R
#' @include side-layer.R
#' @include utils-.R
#' @include utils-calls.R
#' @include utils-constructors.R
#' @include utils-ggproto.R
NULL
### INCLUDE END


#' @name ggside_geom
#' @title ggside geom constructor
#' @description utility function to make a ggside Geom
#' @param class_name New class name for the ggproto object
#' @param geom The Geom ggproto to inherit from
#' @param side should the resulting object be configured for x or y
#' @param ... additional members to add to the ggproto class.
#' @export
ggside_geom <- function(class_name = NULL,
                        geom = NULL,
                        side = NULL,
                        ...) {
  side <- resolve_arg(side, c("x", "y"), null.ok = FALSE)

  members <- try_fetch(
    dots_list(
      ...,
      default_aes = new_default_aes(geom, side),
      required_aes = rename_side(geom$required_aes, side),
      optional_aes = rename_side(geom$optional_aes, side),
      non_missing_aes = rename_side(geom$non_missing_aes, side),
      draw_key = new_ggproto_fun(geom$draw_key, {
        data <- use_side_aes(data, !!side)
        data <- data_unmap(data, !!side)
        call_parent_method
      }),
      .homonyms = "error"
    ),
    error = function(cnd) {
      reserved_args <- str_extr(cnd$body, "`[^`]+`")
      len <- length(reserved_args)
      cli::cli_abort(
        "{cli::qty(len)} argument{?s} {reserved_args} {?is/are} reserved and cannot be specified in `...`",
        parent = cnd
      )
    }
  )
  if (is_ggside_subclass(geom)) {
    class_name <- NULL
  }
  ggplot2::ggproto(
    class_name,
    geom,
    !!!members
  )
}

ggside_stat <- function(class_name = NULL,
                        stat = NULL,
                        side = NULL,
                        ...) {
  side <- resolve_arg(side, c("x", "y"), null.ok = FALSE)
  mapping <- stat$default_aes
  names(mapping) <- rename_side(names(mapping), side)
  members <- try_fetch(
    dots_list(
      ...,
      default_aes = mapping,
      required_aes = rename_side(stat$required_aes, side),
      optional_aes = rename_side(stat$optional_aes, side),
      non_missing_aes = rename_side(stat$non_missing_aes, side),
      .homonyms = "error"
    ),
    error = function(cnd) {
      reserved_args <- str_extr(cnd$body, "`[^`]+`")
      len <- length(reserved_args)
      cli::cli_abort(
        "{cli::qty(len)} argument{?s} {reserved_args} {?is/are} reserved and cannot be specified in `...`",
        parent = cnd
      )
    }
  )
  if (is_ggside_subclass(stat)) {
    class_name <- NULL
  }
  ggplot2::ggproto(
    class_name,
    stat,
    !!!members
  )
}

stat_from_formals <- function(formals) {
  if (is.null(formals$stat)) {
    return(NULL)
  }
  if (is.character(formals$stat)) {
    stat <- formals$stat
    stat <- paste0(toupper(substring(stat, 1, 1)), substring(stat, 2))
    return(find_global("Stat", stat, env = parent.frame()))
  }
  if (inherits(formals$stat, "ggproto")) {
    return(formals$stat)
  }
  cli::cli_abort("`stat` should be a character or ggproto object")
}

# calls source function first to make the layer.
#
#' @keywords internal
ggside_layer_function <-
  function(fun,
           side = NULL,
           env = caller_env(),
           ...,
           force_missing,
           stat_orientation = NULL) {
    # browser()
    resolve_arg(side, c("x", "y"), null.ok = FALSE)
    resolve_arg(stat_orientation, c("x", "y"), null.ok = TRUE)
    fun_sym <- caller_arg(fun)
    formals_ <- formals(fun)
    # at some point it seems like orientation was moved out of
    # the geom functions...
    # we make an effort to check to stat variant
    formals_stat <- formals_[["stat"]]
    stat_variant <- NULL
    if (!is.null(formals_stat) &&
      is.character(formals_stat) &&
      length(formals_stat) == 1) {
      stat_variant <- find_global(sprintf("stat_%s", formals_stat),
        env = parent.frame(), mode = "function"
      )
    }

    stat_formals <- if (!is.null(stat_variant)) {
      formals(stat_variant)
    } else {
      NULL
    }
    formals_ <- dots_list(!!!formals_, ..., .homonyms = "last")
    formals_ <- formals_[!vapply(formals_, is_zap, logical(1))]
    pull_mapping <- expr(mapping <- layer$mapping)
    default_stat_set_manual <- !is.null(stat_orientation)
    has_orientation <-
      !is.null(formals_[["orientation"]]) ||
        !is.null(stat_formals[["orientation"]]) ||
        "orientation" %in% names(formals_)
    if (has_orientation) {
      from_frmls <- formals_[["orientation"]]
      from_stat_frmls <- stat_formals[["orientation"]]
      if (!is.null(from_frmls) && is.na(from_frmls)) {
        formals_[["orientation"]] <- side
      } else if (!is.null(from_stat_frmls) && is.na(from_stat_frmls)) {
        formals_[["orientation"]] <- side
      }
      pull_mapping <-
        expr(mapping <-
          default_stat_aes(layer$mapping, layer$stat, orientation))
    } else if (default_stat_set_manual) {
      pull_mapping <-
        expr(mapping <-
          default_stat_aes(layer$mapping, layer$stat, !!stat_orientation))
    }
    defaults_ <- formals_as_defaults(formals_)
    if (!missing(force_missing)) {
      defaults_ <- defaults_[!names(defaults_) %in% force_missing]
    }
    non_pos_aes <- paste0(side, c("fill", "colour", "color"))
    pos_aes <- paste0(side, "side")
    `_class` <- switch(side,
      x = "XLayer",
      y = "YLayer"
    )
    Side <- switch(side,
      x = "\\1Xside\\L\\2",
      y = "\\1Yside\\L\\2"
    )
    body <- expr({
      map_names <- names(mapping)
      non_pos_aes <- !!non_pos_aes
      side_aes_used <- map_names %in% non_pos_aes
      # temporarily remove ggside specific aes
      # need to re-add afterwards
      if (any(side_aes_used)) {
        names(mapping)[side_aes_used] <-
          sub(!!side, "", map_names[side_aes_used])
      }
      pos_aes_used <- grepl(!!pos_aes, map_names, fixed = TRUE)
      if (any(pos_aes_used)) {
        names(mapping)[pos_aes_used] <-
          sub(!!pos_aes, "", map_names[pos_aes_used])
      }
      to_zap <- non_pos_aes[non_pos_aes %in% ...names()]
      layer <- call_layer_param_aware((!!fun_sym)(!!!defaults_),
        zap = to_zap,
        ...
      )
      !!pull_mapping
      # re-add after vanilla layer has been constructed
      # mapping <- layer$mapping
      if (any(side_aes_used)) {
        names(mapping)[side_aes_used] <- map_names[side_aes_used]
      }

      names(mapping) <- rename_side(names(mapping), !!side)

      # remaps
      geom_aes_map <- aes_to_map(layer$geom, !!side)
      stat_aes_map <- aes_to_map(layer$stat, !!side)
      remap <- union(geom_aes_map, stat_aes_map)
      # ggside_geom
      geom <- ggside_geom(
        gsub("(Geom)([A-Z])", !!Side,
          class(layer$geom)[1],
          perl = T
        ),
        geom = layer$geom,
        side = !!side
      )
      stat <- ggside_stat(
        gsub("(Stat)([A-Z])", !!Side,
          class(layer$stat)[1],
          perl = T
        ),
        stat = layer$stat,
        side = !!side
      )
      SideLayer <- ggproto(
        !!`_class`,
        layer,
        geom = geom,
        stat = stat,
        mapping = mapping
      )
      new_ggside_layer(SideLayer, !!side, remap)
    })

    new_function(formals_,
      body = body,
      env = env
    )
  }


str_extr <- function(string, pattern) {
  matches <- regexec(pattern, text = string)
  unlist(Map(function(x, y) {
    start <- y[1]
    end <- start + attr(y, "match.length")[1] - 1L
    if (start == -1L) {
      return(NA_character_)
    }
    substr(x, start, end)
  }, x = string, y = matches))
}

default_stat_aes <- function(mapping, stat, orientation = "x") {
  if (is.null(mapping)) {
    mapping <- aes()
  }
  stat <- check_subclass(stat, "Stat", env = parent.frame())
  computed_var <- setdiff(c("x", "y"), orientation)
  # if value assigned to computed_var isn't defined by user,
  # grab the default used by stat$default_aes if named aes exists.
  defaults <- stat$default_aes
  if (!computed_var %in% names(mapping) && computed_var %in% names(defaults)) {
    mapping[[computed_var]] <- stat$default_aes[[computed_var]]
  }
  return(mapping)
}
