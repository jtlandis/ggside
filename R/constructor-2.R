
aes_to_map <- function(ggproto, side) {
  resolve_arg(side, c("x", "y"), null.ok = FALSE)
  other_side <- switch(side, x = "y", y = "x")
  req_aes <- pull_aes(ggproto$required_aes)
  opt_aes <- pull_aes(ggproto$optional_aes)
  non_mis <- pull_aes(ggproto$non_missing_aes)
  def_aes <- names(ggproto$default_aes)
  all_aes <- unique(c(req_aes, opt_aes, non_mis, def_aes))
  all_aes[all_aes %in% .ggside_global[[sprintf(".%s_aes", other_side)]]]
}

data_unmap <- function(data, side) {
  names(data) <- sub(sprintf("%sside", side), "", names(data))
  data
}

data_map <- function(data, side, map) {
  x <- names(data)
  aes <- x %in% map
  x[aes] <- sprintf("%sside%s", side, x[aes])
  names(data) <- x
  data
}

enforce_geom <- function(geom) {
  stopifnot("`geom` should be a ggproto object"=inherits(geom, "ggproto"),
            "`geom` should be a Geom object"=inherits(geom, "Geom"))
  class_name <- class(geom)[1]
  where <- find(class_name)
  if (length(where)==0) {
    stop(
      sprintf("could not find ggproto Geom <%s>", class_name)
    )
  }
  class_name <- as.name(class_name)
  if (grepl("^package:", where)) {
    pkg <- as.name(sub("package:", "", where))
    expr <- call("::", pkg, class_name)
  } else {
    expr <- class_name
  }

  #finally check that it is identical
  stopifnot("Could not confirm geom"=identical(geom, eval(expr)))
  expr

}

ggside_geom_setup_data <- function(geom, side, env = parent.frame()) {
  ggprotoGeom <- eval(geom, envir = env)
  args <- ggproto_formals0(ggprotoGeom$setup_data)
  #all ggplot2 geoms do NOT have `self`,
  # but in case that changes in the future
  # or another geom is extended that does have `self`
  has_self <- !is.null(args[["self"]]) || "self" %in% names(args)
  body <- if(has_self) {
    args <- args[setdiff(names(args), "self")]
    expr({
      data <- parse_side_aes(data)
      data <- data_unmap(data, !!side)
      parent <- ggproto_parent(!!geom, self)
      parent$setup_data(!!!args)
    })
  } else {
    expr({
      data <- parse_side_aes(data)
      data <- data_unmap(data, !!side)
      (!!geom)$setup_data(!!!args)
     })
  }
  rlang::new_function(
    args = ggproto_formals(ggprotoGeom$setup_data),
    body = body,
    env = env
  )
}

ggside_geom_draw_panel <- function(geom, side, env = parent.frame()) {

  ggprotoGeom <- eval(geom, envir = env)
  args <- ggproto_formals0(ggprotoGeom$draw_panel)
  has_self <- !is.null(args[["self"]]) || "self" %in% names(args)
  body <- if(has_self) {
    args <- args[!names(args) %in% "self"]
    expr({
      data <- use_side_aes(data, !!side)
      data <- data_unmap(data, !!side)
      parent <- ggproto_parent(!!geom, self)
      parent$draw_panel(!!!args)
    })
  } else {
    expr({
      data <- use_side_aes(data, !!side)
      data <- data_unmap(data, !!side)
      (!!geom)$draw_panel(!!!args)
    })
  }
  rlang::new_function(
    args = ggproto_formals(ggprotoGeom$draw_panel),
    body = body,
    env = env
  )
}

ggside_geom_draw_key <- function(geom, side, env = parent.frame()) {
  ggprotoGeom <- eval(geom, envir = env)
  args <- ggproto_formals0(ggprotoGeom$draw_key)
  has_self <- !is.null(args[["self"]]) || "self" %in% names(args)
  body <- if(has_self) {
    args <- args[setdiff(names(args), "self")]
    expr({
      data <- use_side_aes(data, !!side)
      data <- data_unmap(data, !!side)
      parent <- ggproto_parent(!!geom, self)
      parent$draw_key(!!!args)
    })
  } else {
    expr({
      data <- use_side_aes(data, !!side)
      data <- data_unmap(data, !!side)
      (!!geom)$draw_key(!!!args)
    })
  }
  rlang::new_function(
    args = ggproto_formals(ggprotoGeom$draw_key),
    body = body,
    env = env
  )
}

#' @name ggside_geom
#' @title ggside geom constructor
#' @description utility function to make a ggside Geom
#' @param class_name New class name for the ggproto object
#' @param geom The Geom ggproto to inherit from
#' @param side should the resulting object be configured for x or y
#' @export
ggside_geom <- function(class_name = NULL,
                        geom = NULL,
                        side = NULL,
                        env = parent.frame()) {


  side <- resolve_arg(side, c("x", "y"), null.ok = FALSE)
  geom_name <- enforce_geom(geom)

  ggplot2::ggproto(
    class_name,
    geom,
    default_aes = new_default_aes(geom, side),
    required_aes = rename_side(geom$required_aes, side),
    optional_aes = rename_side(geom$optional_aes, side),
    non_missing_aes = rename_side(geom$non_missing_aes, side),
    setup_data = ggside_geom_setup_data(geom_name, side, env),
    draw_panel = ggside_geom_draw_panel(geom_name, side, env),
    draw_key = ggside_geom_draw_key(geom_name, side, env)
  )
}

ggside_stat <- function(class_name = NULL,
                        stat = NULL,
                        side = NULL) {
  side <- resolve_arg(side, c("x", "y"), null.ok = FALSE)
  mapping <- stat$default_aes
  names(mapping) <- rename_side(names(mapping), side)
  ggplot2::ggproto(
    class_name,
    stat,
    default_aes = mapping,
    required_aes = rename_side(stat$required_aes, side),
    optional_aes = rename_side(stat$optional_aes, side),
    non_missing_aes = rename_side(stat$non_missing_aes, side)
  )
}

ggside_geom2 <- function(class_name = NULL,
                        geom = NULL,
                        side = NULL) {
  side <- resolve_arg(side, c("x", "y"), null.ok = FALSE)
  env <- parent.frame()
  geom_name <- enforce_geom(geom)
  ggplot2::ggproto(
    class_name,
    geom,
    default_aes = new_default_aes(geom, side),
    required_aes = rename_side(geom$required_aes, side),
    optional_aes = rename_side(geom$optional_aes, side),
    non_missing_aes = rename_side(geom$non_missing_aes, side),
    draw_key = ggside_geom_draw_key(geom_name, side, env)
  )
}

# calls source function first to make the layer.
#
#' @export
ggside_layer_function <- function(fun, side, env = caller_env()) {
  # browser()
  fun_sym <- caller_arg(fun)
  formals_ <- formals(fun)
  defaults_ <- formals_as_defaults(formals_)
  non_pos_aes <- paste0(side, c("fill", "colour", "color"))
  pos_aes <- paste0(side, "side")
  `_class` <- switch(side, x = "XLayer", y = "YLayer")
  Side <- switch(side, x = "\\1Xside\\L\\2", y = "\\1Yside\\L\\2")
  body <- expr({
    map_names <- names(mapping)
    side_aes_used <- map_names %in% !!non_pos_aes
    # temporarily remove ggside specific aes
    # need to re-add afterwards
    if (any(side_aes_used)) {
      names(mapping)[side_aes_used] <- sub(!!side, "", map_names[side_aes_used])
    }
    pos_aes_used <- grepl(!!pos_aes, map_names, fixed = TRUE)
    if (any(pos_aes_used)) {
      names(mapping)[pos_aes_used] <- sub(!!pos_aes, "", map_names[pos_aes_used])
    }
    layer <- (!!fun_sym)(!!!defaults_)
    # re-add after vanilla layer has been constructed
    mapping <- layer$mapping
    if (any(side_aes_used)) {
      names(mapping)[side_aes_used] <- map_names[side_aes_used]
    }

    names(mapping) <- rename_side(names(mapping), !!side)

    # remaps
    geom_aes_map <- aes_to_map(layer$geom, !!side)
    stat_aes_map <- aes_to_map(layer$stat, !!side)
    remap <- union(geom_aes_map, stat_aes_map)
    # ggside_geom
    geom <- ggside_geom2(gsub("(Geom)([A-Z])", !!Side,
                              class(layer$geom)[1], perl = T),
                         geom = layer$geom,
                         side = !!side)
    stat <- ggside_stat(gsub("(Stat)([A-Z])", !!Side,
                             class(layer$stat)[1], perl = T),
                        stat = layer$stat,
                        side = !!side)
    SideLayer <- ggproto(
      !!`_class`, layer,
      geom = geom, stat = stat,
      mapping = mapping
    )
    new_ggside_layer2(SideLayer, !!side, remap)
  })

  new_function(
    formals_,
    body = body,
    env = env
  )

}
