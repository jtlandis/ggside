

#Position collapse

PositionCollapse <- ggplot2::ggproto("PositionCollapse",
                                     ggplot2::Position,
                                     required_aes = c("x|y"),
                                     collapse = "y", #collapse y onto x
                                                     #if y exists in data, then this is used to calculate
                                                     #waivered scale_range. Other
                                     location = NULL, #collapse y defaults location to "bottom", and
                                                          #collapse x defaults location to "left".
                                                          #location helps specify how waivered midpoint is calculated
                                     midpoint = NULL, #center point of collapsed y/x about the geom is built.
                                                          #If midpoint is specified - location is ignored
                                     scale_range = NULL, #range about midpoint that y/x is scaled onto.
                                     instance = NULL, #if instance is used then previous
                                     setup_params = function(self, data){
                                       browser()

                                       suggested_var <- c("x","y")
                                       env <- find_build_plotEnv()
                                       .hs <- get_variable(".build_history", envir = env)
                                       if(!is.element(self$collapse, suggested_var)){
                                         abort(glue("PositionCollapse does not know how to collapse {self$collapse}\n"))
                                       }

                                       instance <- self$instance

                                       if(!is.null(instance)){#check if previous instance exists
                                         .tmp <- .hs[.hs$cvar==self$collapse&.hs$instance==instance,]
                                         if(nrow(.tmp)==1){
                                           return(
                                             list(collapse_var = self$collapse,
                                                collapse_onto = suggested_var[!suggested_var%in%self$collapse],
                                                midpoint = .hs[.hs$instance==instance,]$midpoint,
                                                scale_range = .hs[.hs$instance==instance,]$scale_range,
                                                location = .hs[.hs$instance==instance,]$location,
                                                instance = instance))
                                         }
                                       }
                                       .i <- .hs[.hs$cvar==self$collapse,]$instance
                                       instance <- instance %||% if(length(.i)==0) 1 else (max(.i)+1)

                                       collapse_var <- self$collapse
                                       collapse_onto <- suggested_var[!suggested_var%in%collapse_var]

                                       mainData <- grab_Main_Mapping(env = env) %||% data_frame(x = numeric(0), y = numeric(0)) #handle case when everything is empty
                                       if(!any(suggested_var%in%colnames(mainData))|nrow(mainData)==0){
                                         mainData[[collapse_var]] <- 0
                                         mainData[[collapse_onto]] <- 1
                                       }
                                       if(!all(suggested_var%in%colnames(mainData))){
                                         missing_var <- suggested_var[!suggested_var%in%colnames(mainData)]
                                         if(collapse_var==missing_var){
                                           mainData[[missing_var]] <- 0
                                         } else {
                                           mainData[[missing_var]] <- 1
                                         }
                                       }


                                       collapse_dat <- mainData[[collapse_var]]
                                       scale_range <- self$scale_range %||% if(resolution(collapse_dat, FALSE)!=1) (diff(range(collapse_dat))*.05) else 1
                                       if(!is_scalar_double(scale_range)){
                                         abort(glue("scale_range should be a double scalar value"))
                                       }
                                       location <- self$location %||% switch(collapse_onto, x = "bottom", y = "left")
                                       if(!is_scalar_character(location)){
                                         abort(glue("location should be a character scalar value"))
                                       }
                                       if(!location %in% switch(collapse_onto, x = c("bottom","top"), y = c("left","right"))){
                                         warn(glue("location: {location} is uncompatable with collapse variable {collapse_var}.\n",
                                                   "Coercing location to \"{switch(collapse_onto, x = \"bottom\", y = \"left\")}\""))
                                         location <- switch(collapse_onto, x = "bottom", y = "left")
                                       }
                                       if((instance-1) %in% .hs$instance & location %in% .hs$location){
                                         logi <- .hs$instance%in%instance&.hs$location%in%location
                                         adjust.lb <- .hs[logi,]$midpoint - (.hs[logi,]$max - .hs[logi,]$min)/2 - scale_range/2
                                         adjust.ru <- .hs[logi,]$midpoint + (.hs[logi,]$max - .hs[logi,]$min)/2 + scale_range/2
                                       } else {
                                         adjust.lb <- min(collapse_dat) - scale_range
                                         adjust.ru <- max(collapse_dat) + scale_range
                                       }

                                       midpoint <- self$midpoint %||% case_when(location%in%c("bottom","left") ~ adjust.lb,
                                                                                location%in%c("top",  "right") ~ adjust.ru)


                                       params <- list(
                                         collapse_var = collapse_var,
                                         collapse_onto = collapse_onto,
                                         midpoint = midpoint,
                                         scale_range = scale_range,
                                         location = location,
                                         instance = instance
                                       )
                                       .hr <- params$scale_range/2
                                       .hs <- rbind(.hs,
                                                    data_frame(cvar = collapse_var,
                                                               min = params$midpoint-.hr,
                                                               mid = params$midpoint,
                                                               max = params$midpoint+.hr,
                                                               instance = instance,
                                                               location = location))
                                       assign(x = ".build_history", value = .hs, envir = env)
                                       return(params)

                                     },
                                     setup_data = function(data, params){
                                       #browser()
                                       suggested_var <- c("x","y")
                                       cvar <- params$collapse_var
                                       if(!all(suggested_var%in%colnames(data))){
                                         missing_var <- suggested_var[!suggested_var%in%colnames(data)]
                                         if(missing_var==cvar){
                                           warn(glue("collapse variable \"{missing_var}\" is missing from layer data.",
                                                     " Coercing value to 0"))
                                           data[[missing_var]] <- 0
                                           data[[paste0(missing_var,"max")]] <- 0.5
                                           data[[paste0(missing_var,"min")]] <- -.5
                                         } else {
                                           warn(glue("Collapsing \"{cvar}\" onto \"{missing_var}\" is not well defined ",
                                                     "because \"{missing_var}\" is missing in layer data.\n Setting \"{missing_var}\" to 1."))
                                           data[[missing_var]] <- 1
                                           data[[paste0(missing_var,"max")]] <- 1.5
                                           data[[paste0(missing_var,"min")]] <- 0.5
                                         }
                                       }
                                       varmin <- data[[paste0(cvar,"min")]] %||% min(data[[cvar]])
                                       varmax <- data[[paste0(cvar,"max")]] %||% max(data[[cvar]])
                                       from_range <- c(varmin, varmax)
                                       collapse_range <- params$midpoint + (c(-1,1)*c(params$scale_range/2))
                                       data[[cvar]] <- scales::rescale(data[[cvar]], to = collapse_range, from = from_range)
                                       data[[paste0(cvar,"max")]] <- scales::rescale(data[[paste0(cvar,"max")]], to = collapse_range, from = from_range)
                                       data[[paste0(cvar,"min")]] <- scales::rescale(data[[paste0(cvar,"min")]], to = collapse_range, from = from_range)
                                       data
                                     },
                                     compute_panel = function(data, params, scales){
                                       distinct_all(data)
                                     })



position_collapse <- function(collapse = "y", midpoint = NULL, scale_range = NULL, location = NULL){
  ggproto(NULL, PositionCollapse, collapse = collapse, midpoint = midpoint, scale_range = scale_range, location = location)
}
