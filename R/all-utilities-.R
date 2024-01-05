


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


