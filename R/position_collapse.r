

#Position rescale

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
                                       browser()

                                       suggested_var <- c("x","y")
                                       env <- find_build_plotEnv()
                                       .hs <- get_variable(".build_history", envir = env)
                                       if(!is.element(self$rescale, suggested_var)){
                                         abort(glue("PositionRescale does not know how to rescale {self$rescale}\n"))
                                       }

                                       instance <- self$instance

                                       if(!is.null(instance)){#check if previous instance exists
                                         .tmp <- .hs[.hs$cvar==self$rescale&.hs$instance==instance,]
                                         if(nrow(.tmp)==1){
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
                                       if((instance-1) %in% .hs$instance & location %in% .hs$location){
                                         logi <- .hs$instance%in%(instance-1)&.hs$location%in%location
                                         adjust.lb <- .hs[logi,]$mid - (.hs[logi,]$max - .hs[logi,]$min)/2 - range/2
                                         adjust.ru <- .hs[logi,]$mid + (.hs[logi,]$max - .hs[logi,]$min)/2 + range/2
                                       } else {
                                         adjust.lb <- min(rescale_dat) - (var_reso +range)/2
                                         adjust.ru <- max(rescale_dat) + (var_reso + range)/2
                                       }

                                       midpoint <- self$midpoint %||% case_when(location%in%c("bottom","left") ~ adjust.lb,
                                                                                location%in%c("top",  "right") ~ adjust.ru)


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
                                       #browser()
                                       suggested_var <- c("x","y")
                                       cvar <- params$rescale_var
                                       if(!all(suggested_var%in%colnames(data))){
                                         missing_var <- suggested_var[!suggested_var%in%colnames(data)]
                                         if(missing_var==cvar){
                                           warn(glue("rescale variable \"{missing_var}\" is missing from layer data.",
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
                                       rerange <- params$midpoint + (c(-1,1)*c(params$range/2))
                                       data[[cvar]] <- scales::rescale(data[[cvar]], to = rerange, from = from_range)
                                       data[[paste0(cvar,"max")]] <- scales::rescale(data[[paste0(cvar,"max")]], to = rerange, from = from_range)
                                       data[[paste0(cvar,"min")]] <- scales::rescale(data[[paste0(cvar,"min")]], to = rerange, from = from_range)
                                       data
                                     },
                                     compute_panel = function(data, params, scales){
                                       distinct_all(data)
                                     })



position_rescale <- function(rescale = "y", midpoint = NULL, range = NULL, location = NULL, instance = NULL){
  ggproto(NULL, PositionRescale, rescale = rescale, midpoint = midpoint, range = range, location = location, instance = instance)
}
