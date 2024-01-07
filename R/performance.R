### INCLUDE BEGIN
#' @include utils-ggplot2-reimpl-.R
NULL
### INCLUDE END
# Taken from ggplot2
# Fast data.frame constructor and indexing
# No checking, recycling etc. unless asked for




split_matrix <- function(x, col_names = colnames(x)) {
  force(col_names)
  x <- lapply(seq_len(ncol(x)), function(i) x[, i])
  if (!is.null(col_names)) names(x) <- col_names
  x
}

mat_2_df <- function(x, col_names = colnames(x)) {
  data_frame0(!!!split_matrix(x, col_names))
}

df_col <- function(x, name) .subset2(x, name)

df_rows <- function(x, i) {
  data_frame0(!!!lapply(x, `[`, i = i))
}

# More performant modifyList without recursion
modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}
modifyList <- function(...) {
  abort(glue("
    Please use `modify_list()` instead of `modifyList()` for better performance.
    See the vignette 'ggplot2 internal programming guidelines' for details.
  "))
}
