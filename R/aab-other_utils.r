### INCLUDE BEGIN
#' @include compat-plyr.R
#' @include ggplot2-reimpl-.R
#' @include performance.R
NULL
### INCLUDE END


find_build_plotEnv <- function(){
  items <- lapply(sys.frames(), ls)
  expected_items <- c("by_layer","data","layer_data",
                      "layers","layout","plot","scale_x",
                      "scale_y","scales")
  EnvIndex <- unlist(lapply(items, function(x,y){sum(y%in%x)}, y = expected_items))
  Env <- sys.frames()[which(EnvIndex==max(EnvIndex))]
  return(Env[[1]])
}

"%NA%" <- function(a, b){
  if(all(is.na(a))) b else a
}

get_variable <- function(x, envir){
  if(is.null(envir)) return(NULL)
  if(!x%in%ls(envir, all.names = T)) return(NULL)
  return(get(x, envir = envir))
}

grab_Main_Mapping <- function(env = NULL){
  if(is.null(env)|!is.environment(env)){
    env <- find_build_plotEnv()
  }
  p <- get_variable("plot", env)
  # Evaluate aesthetics
  evaled <- lapply(p$mapping, eval_tidy, data = p$data)
  evaled <- compact(evaled)
  evaled <- as_gg_data_frame(evaled)
  evaled[,names(evaled)] <- lapply(evaled, FUN = function(x){
    if(!(is.numeric(x)|is.integer(x))) return(as.numeric(as.factor(x)))
    return(x)
  })
  return(evaled)
}

#' @rdname ggside-ggproto-geoms
#' @export
use_xside_aes <- function(data){
  data$fill <- data$xfill %NA% data$fill
  data$colour <- data$xcolour %NA% data$colour
  data
}

#' @rdname ggside-ggproto-geoms
#' @export
use_yside_aes <- function(data){
  data$fill <- data$yfill %NA% data$fill
  data$colour <- data$ycolour %NA% data$colour
  data
}

#' @rdname ggside-ggproto-geoms
#' @export
parse_side_aes <- function(data, params){
  #determine if fill, xfill, or yfill should be used
  all_names <- c(colnames(data))
  if(any(c("fill", "xfill", "yfill")%in% all_names)) {
    fill_opts <- all_names[all_names %in% c("fill", "xfill", "yfill")]
    side_fill <- c("xfill","yfill")%in%fill_opts
    if(any(side_fill)){
      fill_prec <- c("xfill","yfill")[side_fill]
    } else {
      fill_prec <- "fill"
    }
    data[[fill_prec]] <- data[[fill_prec]]
    exclude <- fill_opts[!fill_opts %in% fill_prec]
    if(length(exclude)!=0){
      data <- data[, setdiff(colnames(data), exclude), drop = F]
    }
  }

  if(any(c("colour", "xcolour", "ycolour")%in% all_names)) {
    colour_opts <- all_names[all_names %in% c("colour", "xcolour", "ycolour")]
    side_colour <- c("xcolour","ycolour")%in%colour_opts
    if(any(side_colour)){
      colour_prec <- c("xcolour","ycolour")[side_colour]
    } else {
      colour_prec <- "colour"
    }
    data[[colour_prec]] <- data[[colour_prec]]
    exclude <- colour_opts[!colour_opts %in% colour_prec]
    if(length(exclude)!=0){
      data <- data[, setdiff(colnames(data), exclude), drop = F]
    }
  }

  return(data)
}

# proto2 TODO: better way of getting formals for self$draw

ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

manual_scale <- function(aesthetic, values = NULL, breaks = waiver(), name = waiver(),
                         ..., limits = NULL, call = caller_call())
{
  call <- call %||% current_call()
  if (is_missing(values)) {
    values <- NULL
  }
  else {
    force(values)
  }
  if (is.null(limits) && !is.null(names(values))) {
    force(aesthetic)
    limits <- function(x) {
      x <- intersect(x, c(names(values), NA)) %||% character()
      if (length(x) < 1) {
        cli::cli_warn(paste0("No shared levels found between {.code names(values)} of the manual ",
                             "scale and the data's {.field {aesthetic}} values."))
      }
      x
    }
  }
  if (is.vector(values) && is.null(names(values)) && !is.waive(breaks) &&
      !is.null(breaks) && !is.function(breaks)) {
    if (length(breaks) <= length(values)) {
      names(values) <- breaks
    }
    else {
      names(values) <- breaks[1:length(values)]
    }
  }
  pal <- function(n) {
    if (n > length(values)) {
      cli::cli_abort("Insufficient values in manual scale. {n} needed but only {length(values)} provided.")
    }
    values
  }
  discrete_scale(aesthetic, name = name, palette = pal, breaks = breaks,
                 limits = limits, call = call, ...)
}


is.waive <- function(x) inherits(x, "waiver")

uniquecols <- function(df) {
  df <- df[1, sapply(df, function(x) length(unique(x)) == 1), drop = FALSE]
  rownames(df) <- 1:nrow(df)
  df
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


dapply <- function(df, by, fun, ..., drop = TRUE) {
  grouping_cols <- .subset(df, by)
  fallback_order <- unique(c(by, names(df)))
  apply_fun <- function(x) {
    res <- fun(x, ...)
    if (is.null(res)) return(res)
    if (length(res) == 0) return(new_data_frame())
    vars <- lapply(setNames(by, by), function(col) .subset2(x, col)[1])
    if (is.matrix(res)) res <- split_matrix(res)
    if (is.null(names(res))) names(res) <- paste0("V", seq_along(res))
    if (all(by %in% names(res))) return(new_data_frame(unclass(res)))
    res <- modify_list(unclass(vars), unclass(res))
    new_data_frame(res[intersect(c(fallback_order, names(res)), names(res))])
  }

  # Shortcut when only one group
  if (all(vapply(grouping_cols, single_value, logical(1)))) {
    return(apply_fun(df))
  }

  ids <- id(grouping_cols, drop = drop)
  group_rows <- split_with_index(seq_len(nrow(df)), ids)
  vec_rbind(!!!lapply(seq_along(group_rows), function(i) {
    cur_data <- df_rows(df, group_rows[[i]])
    apply_fun(cur_data)
  }))
}

new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) {
    abort("Elements must be named")
  }
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) {
      abort("Elements must equal the number of rows or 1")
    }
    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- "data.frame"

  attr(x, "row.names") <- .set_row_names(n)
  x
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


str_extr <- function(string, pattern){
  matches <- regexec(pattern, text = string)
  unlist(Map(function(x,y){
    start <- y[1]
    end <- start + attr(y, "match.length")[1] - 1L
    if(start==-1L) return(NA_character_)
    substr(x, start, end)}, x = string, y = matches))
}

do_by <- function(data, by, fun, ...){
  order_cache <- do.call('order', lapply(by, function(x){data[[x]]}))
  data <- data[order_cache,]
  split_by <- interaction(data[,by, drop = F], drop = T, lex.order = T)
  data <- vec_rbind(!!!lapply(split(data, split_by), FUN = fun, ...))
  data <- data[order(order_cache),]
  rownames(data) <- seq_len(nrow(data))
  data
}

anti_join <- function(x, y, by) {
  keys <- join_keys(x, y, by)
  x[!keys$x%in%keys$y,]
}
semi_join <- function(x, y, by) {
  keys <- join_keys(x, y, by)
  x[keys$x%in%keys$y,]
}



simplify <- function (x)
{
  if (length(x) == 2 && is_symbol(x[[1]], "~")) {
    return(simplify(x[[2]]))
  }
  if (length(x) < 3) {
    return(list(x))
  }
  op <- x[[1]]
  a <- x[[2]]
  b <- x[[3]]
  if (is_symbol(op, c("+", "*", "~"))) {
    c(simplify(a), simplify(b))
  }
  else if (is_symbol(op, "-")) {
    c(simplify(a), expr(-!!simplify(b)))
  }
  else {
    list(x)
  }
}


default_stat_aes <- function(mapping, stat, orientation = "x"){
  if(is.null(mapping)){
    mapping <- aes()
  }
  stat <- check_subclass(stat, "Stat", env = parent.frame())
  computed_var <- setdiff(c("x","y"), orientation)
  #if value assigned to computed_var isn't defined by user,
  #grab the default used by stat$default_aes if named aes exists.
  defaults <- stat$default_aes
  if(!computed_var%in%names(mapping)&&computed_var%in%names(defaults)) {
    mapping[[computed_var]] <- stat$default_aes[[computed_var]]
  }
  return(mapping)
}
