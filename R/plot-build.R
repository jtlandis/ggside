
#' @import ggplot2
guess_layer_mapping <- function(layer) {
  geom_class <- stringr::str_extract(class(layer$geom), "(X|Y)side")
  val <- if(all(is.na(geom_class))){
    "main"
  } else {
    geom_class <- geom_class[!is.na(geom_class)]
    to_lower_ascii(substr(geom_class,1,1))
  }
  return(val)
}
