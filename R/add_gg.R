
plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()

  p
}


merge_element <- function(new, old) {
  UseMethod("merge_element")
}

merge_element.default <- function(new, old) {
  if (is.null(old) || inherits(old, "element_blank")) {
    # If old is NULL or element_blank, then just return new
    return(new)
  } else if (is.null(new) || is.character(new) || is.numeric(new) || is.unit(new) ||
             is.logical(new)) {
    # If new is NULL, or a string, numeric vector, unit, or logical, just return it
    return(new)
  }

  # otherwise we can't merge
  abort(glue("No method for merging {class(new)[1]} into {class(old)[1]}"))
}

merge_element.element <- function(new, old) {
  if (is.null(old) || inherits(old, "element_blank")) {
    # If old is NULL or element_blank, then just return new
    return(new)
  }

  # actual merging can only happen if classes match
  if (!inherits(new, class(old)[1])) {
    abort("Only elements of the same class can be merged")
  }

  # Override NULL properties of new with the values in old
  # Get logical vector of NULL properties in new
  idx <- vapply(new, is.null, logical(1))
  # Get the names of TRUE items
  idx <- names(idx[idx])

  # Update non-NULL items
  new[idx] <- old[idx]

  new
}

merge_element.element_blank <- function(new, old) {
  # If new is element_blank, just return it
  new
}
