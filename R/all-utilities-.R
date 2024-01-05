



# Temporarily changes any `xside` or `yside` prefix to be removed
# from scale$aesthetic. Values are returned when exiting the frame this
# function was called in.
# @param scale a ggproto Scale object
# local_vanilla_scale_aes <- function(scale, frame = parent.frame()) {
#   s_quo <- enexpr(scale)
#   aes_ <- expr((!!s_quo)$aesthetics)
#   old <- eval_bare(expr(!!aes_), frame)
#   eval_bare(expr( `<-`(!!aes_, (!!aes_)[!grepl('(x|y)side', !!old)])) , frame)
#   eval_bare(expr(on.exit(`<-`(!!aes_ ,!!old), add = T)), frame)
# }

# Temporarily changes the first element of a list to contain the
# union aesthetics of all other elements
# @param scales a list containing a ggproto Scale object
# local_union_scale_aes <- function(scales, frame = parent.frame()) {
#   s_quo <- enexpr(scales)
#   aes_ <- expr((!!s_quo)[[1]]$aesthetics)
#   old <- eval_bare(expr(!!aes_), frame)
#   new <- eval_bare(expr(unique(unlist(lapply(!!s_quo, `[[`, "aesthetics")))), frame)
#   eval_bare(expr(`<-`(!!aes_, !!new)), frame)
#   expr <- expr(on.exit(`<-`(!!aes_, !!old) , add = TRUE))
#   eval_bare(expr, frame)
# }


# Standardize the panel params such that any `xside` or `yside` prefixes
# are removed from the object
# @param panel_params the panel_params element of the Layout ggproto object
# @param layout the layout element of the Layout ggproto object
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


modify_body <- function(call_body, from, to) {
  for (i in seq_along(call_body)) {
    call <- call_body[[i]]
    if(!rlang::is_missing(call)) {
      if(identical(call, from)) {
        call_body[[i]] <- to
      } else if (length(call)>1) {
        call_body[[i]] <- modify_body(call, from, to)
      }
    }
  }
  call_body
}


