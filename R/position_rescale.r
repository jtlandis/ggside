### INCLUDE BEGIN
#' @include aab-other_utils.r
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

#Very old code, will probably remove

#Position rescale

#' Rescale x or y onto new range in margin
#' @name position_rescale
#' @rdname position_rescale
#' @usage NULL
#' @export
PositionRescale <- ggplot2::ggproto("PositionRescale",
                                     ggplot2::Position,
                                     required_aes = c("x|y"),
                                     rescale = "y", #rescale y onto x
                                                     #if y exists in data, then this is used to calculate
                                                     #waivered range. Other
                                     location = NULL, #rescale y defaults location to "bottom", and
                                                          #rescale x defaults location to "left".
                                                          #location helps specify how waivered midpoint is calculated
                                     midpoint = NULL, #center point of rescaled y/x about the geom is built.
                                                          #If midpoint is specified - location is ignored
                                     range = NULL, #range about midpoint that y/x is scaled onto.
                                     instance = NULL, #if instance is used then previous
                                     setup_params = function(self, data){

                                       suggested_var <- c("x","y")
                                       env <- find_build_plotEnv()
                                       .hs <- get_variable(".build_history", envir = env)
                                       if(!is.element(self$rescale, suggested_var)){
                                         abort(glue("PositionRescale does not know how to rescale {self$rescale}\n"))
                                       }

                                       instance <- self$instance

                                       if(!is.null(instance)){#check if previous instance exists
                                         .tmp <- .hs[.hs$cvar==self$rescale&.hs$instance==instance,]
                                         if(!is.null(.tmp)&&nrow(.tmp)==1){
                                           return(
                                             list(rescale_var = self$rescale,
                                                rescale_onto = suggested_var[!suggested_var%in%self$rescale],
                                                midpoint = .hs[.hs$instance==instance,]$mid,
                                                range = .hs[.hs$instance==instance,]$max - .hs[.hs$instance==instance,]$min,
                                                location = .hs[.hs$instance==instance,]$location,
                                                instance = instance))
                                         }
                                       }
                                       .i <- .hs[.hs$cvar==self$rescale,]$instance
                                       instance <- instance %||% if(length(.i)==0) 1 else (max(.i)+1)

                                       rescale_var <- self$rescale
                                       rescale_onto <- suggested_var[!suggested_var%in%rescale_var]

                                       mainData <- grab_Main_Mapping(env = env) %||% data_frame(x = numeric(0), y = numeric(0)) #handle case when everything is empty
                                       if(!any(suggested_var%in%colnames(mainData))|nrow(mainData)==0){
                                         mainData[[rescale_var]] <- 0
                                         mainData[[rescale_onto]] <- 1
                                       }
                                       if(!all(suggested_var%in%colnames(mainData))){
                                         missing_var <- suggested_var[!suggested_var%in%colnames(mainData)]
                                         if(rescale_var==missing_var){
                                           mainData[[missing_var]] <- 0
                                         } else {
                                           mainData[[missing_var]] <- 1
                                         }
                                       }


                                       rescale_dat <- mainData[[rescale_var]]
                                       var_reso <- if(resolution(rescale_dat, FALSE)!=1) (diff(range(rescale_dat))*.05) else 1
                                       range <- self$range %||% var_reso
                                       if(!is_scalar_double(range)){
                                         abort(glue("range should be a double scalar value"))
                                       }
                                       location <- self$location %||% switch(rescale_onto, x = "bottom", y = "left")
                                       if(!is_scalar_character(location)){
                                         abort(glue("location should be a character scalar value"))
                                       }
                                       if(!location %in% switch(rescale_onto, x = c("bottom","top"), y = c("left","right"))){
                                         warn(glue("location: {location} is uncompatable with rescale variable {rescale_var}.\n",
                                                   "Coercing location to \"{switch(rescale_onto, x = \"bottom\", y = \"left\")}\""))
                                         location <- switch(rescale_onto, x = "bottom", y = "left")
                                       }
                                       if(location %in% .hs$location){
                                         logi <- .hs$location%in%location
                                         adjust.lb <- min(.hs[logi,]$mid) - (min(.hs[logi,]$max - .hs[logi,]$min))/2 - range/2
                                         adjust.ru <- max(.hs[logi,]$mid) + (max(.hs[logi,]$max - .hs[logi,]$min))/2 + range/2
                                       } else {
                                         adjust.lb <- min(rescale_dat) - (var_reso +range)/2
                                         adjust.ru <- max(rescale_dat) + (var_reso + range)/2
                                       }

                                       if(!is.null(self$midpoint)){
                                         location <- "NA"
                                       }
                                       if(!is.null(self$midpoint)){
                                         midpoint <- self$midpoint
                                       } else if(location %in% c("bottom","left")){
                                         midpoint <- adjust.lb
                                       } else {
                                         midpoint <- adjust.ru
                                       }


                                       params <- list(
                                         rescale_var = rescale_var,
                                         rescale_onto = rescale_onto,
                                         midpoint = midpoint,
                                         range = range,
                                         location = location,
                                         instance = instance
                                       )
                                       .hr <- params$range/2
                                       .hs <- rbind(.hs,
                                                    data_frame(cvar = rescale_var,
                                                               min = params$midpoint-.hr,
                                                               mid = params$midpoint,
                                                               max = params$midpoint+.hr,
                                                               instance = instance,
                                                               location = location))
                                       assign(x = ".build_history", value = .hs, envir = env)
                                       return(params)

                                     },
                                     setup_data = function(data, params){
                                       suggested_var <- c("x","y")
                                       cvar <- params$rescale_var
                                       suffix <- c("min","lower","middle","upper","max","min_final","max_final", "end", "")
                                       .cols <- base::intersect(colnames(data), c(paste0(cvar,suffix),suffix))
                                       cdata <- data[,.cols, drop = FALSE]
                                       from_range <- range(unlist(lapply(cdata, range)))
                                       rerange <- params$midpoint + (c(-1,1)*c(params$range/2))
                                       data[,.cols] <- lapply(data[,.cols, drop = FALSE],function(x){
                                         if(is.list(x)){
                                           x <- lapply(x, scales::rescale, to = rerange, from = from_range)
                                         } else {
                                           x <- scales::rescale(x, to = rerange, from = from_range)
                                         }
                                         return(x)
                                       })
                                       # data[[cvar]] <- scales::rescale(data[[cvar]], to = rerange, from = from_range)
                                       # data[[paste0(cvar,"max")]] <- scales::rescale(data[[paste0(cvar,"max")]], to = rerange, from = from_range)
                                       # data[[paste0(cvar,"min")]] <- scales::rescale(data[[paste0(cvar,"min")]], to = rerange, from = from_range)
                                       data
                                     },
                                     compute_panel = function(data, params, scales){
                                       suppressWarnings({unique(data)})
                                     })


#' @rdname position_rescale
#' @description Take the range of the specified axis and rescale it to a new range about a midpoint. By default
#' the range will be calculated from the associated main plot axis mapping. The range will either be the resolution
#' or 5% of the axis range, depending if original data is discrete or continuous respectively. Each layer called
#' with position_rescale will possess an instance value that indexes with axis rescale. By default, each
#' position_rescale will dodge the previous call unless instance is specified to a previous layer.
#' @param rescale character value of "x" or "y". specifies which mapping data will be rescaled
#' @param midpoint default set to NULL. Center point about which the rescaled x/y values will reside.
#' @param range default set to NULL and auto generates from main mapping range. Specifies the size of the rescaled range.
#' @param location specifies where position_rescale should try to place midpoint. If midpoint is specified, location
#' is ignored and placed at the specified location.
#' @param instance integer that indexes rescaled axis calls. instance may be specified and if a previous
#' layer with the same instance exists, then the same midpoint and range are used for rescaling. x and y are
#' indexed independently.
#' @return a ggproto object inheriting from 'Position' and can be added to a ggplot
#' @export
position_rescale <- function(rescale = "y", midpoint = NULL, range = NULL, location = NULL, instance = NULL){
  ggproto(NULL, PositionRescale, rescale = rescale, midpoint = midpoint, range = range, location = location, instance = instance)
}

#' @rdname position_rescale
#' @export
position_yrescale <- function(rescale = "y", midpoint = NULL,
                              range = NULL, location = NULL,
                              instance = NULL) {
  position_rescale(rescale = rescale, midpoint = midpoint, range = range, location = location, instance = instance)
  }

#' @rdname position_rescale
#' @export
position_xrescale <- function(rescale = "x", midpoint = NULL,
                              range = NULL, location = NULL,
                              instance = NULL) {
  position_rescale(rescale = rescale, midpoint = midpoint, range = range, location = location, instance = instance)
  }
