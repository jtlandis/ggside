### INCLUDE BEGIN
#' @include aab-other_utils.r
#' @include aes-evaluation.r
#' @include compat-plyr.R
#' @include side-layer2.R
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
      draw_key = new_ggproto_fun(geom$draw_key,
                                 {
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
  ggplot2::ggproto(class_name,
                   geom,
                   !!!members)
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

  ggplot2::ggproto(class_name,
                   stat,
                   !!!members)
}



# calls source function first to make the layer.
#
#' @keywords internal
ggside_layer_function <-
  function(fun,
           side = NULL,
           env = caller_env(),
           ...,
           force_missing) {
    # browser()
    resolve_arg(side, c("x", "y"), null.ok = FALSE)
    fun_sym <- caller_arg(fun)
    formals_ <- formals(fun)
    formals_ <- dots_list(!!!formals_, ..., .homonyms = "last")
    formals_ <- formals_[!vapply(formals_, is_zap, logical(1))]
    pull_mapping <- expr(mapping <- layer$mapping)
    has_orientation <-
      !is.null(formals_[["orientation"]]) ||
      "orientation" %in% names(formals_)
    if (has_orientation) {
      if (is.na(formals_[["orientation"]]))
        formals_[["orientation"]] <- side

      pull_mapping <-
        expr(mapping <-
               default_stat_aes(layer$mapping, layer$stat, orientation))
    }
    defaults_ <- formals_as_defaults(formals_)
    if (!missing(force_missing)) {
      defaults_ <- defaults_[!names(defaults_) %in% force_missing]
    }
    non_pos_aes <- paste0(side, c("fill", "colour", "color"))
    pos_aes <- paste0(side, "side")
    `_class` <- switch(side, x = "XLayer", y = "YLayer")
    Side <- switch(side, x = "\\1Xside\\L\\2", y = "\\1Yside\\L\\2")
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
          sub(pos_aes, "", map_names[pos_aes_used])
      }
      to_zap <- non_pos_aes[non_pos_aes %in% ...names()]
      layer <- call_layer_param_aware((!!fun_sym)(!!!defaults_),
                                      zap = to_zap,
                                      ...)
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
             class(layer$geom)[1], perl = T),
        geom = layer$geom,
        side = !!side
      )
      stat <- ggside_stat(
        gsub("(Stat)([A-Z])", !!Side,
             class(layer$stat)[1], perl = T),
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
      new_ggside_layer2(SideLayer, !!side, remap)
    })

    new_function(formals_,
                 body = body,
                 env = env)

  }


