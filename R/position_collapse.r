

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
                                       if(!is.element(self$collapse, suggested_var)){
                                         abort(glue("PositionCollapse does not know how to collapse {self$collapse}\n"))
                                       }
                                       if(!all(suggested_var%in%colnames(data))){
                                         missing_var <- suggested_var[!suggested_var%in%colnames(data)]
                                         if(missing_var==self$collapse){
                                           # warn(glue("collapse variable \"{missing_var}\" is missing from layer data.",
                                           #           " Coercing value to 0"))
                                           data[[missing_var]] <- 0
                                         } else {
                                           # warn(glue("Collapsing \"{self$collapse}\" onto \"{missing_var}\" is not well defined ",
                                           #           "because \"{missing_var}\" is missing in layer data.\n Setting \"{missing_var}\" to 1."))
                                           data[[missing_var]] <- 1
                                         }
                                       }

                                       collapse_var <- self$collapse
                                       collapse_dat <- data[[collapse_var]]
                                       collapse_onto <- suggested_var[!suggested_var%in%collapse_var]
                                       scale_range <- self$scale_range %||% if(resolution(collapse_dat, FALSE)!=1) (diff(range(collapse_dat))*.05) else 1
                                       location <- self$location %||% switch(collapse_onto, x = "bottom", y = "left")
                                       if(!location %in% switch(collapse_onto, x = c("bottom","top"), y = c("left","right"))){
                                         warn(glue("location: {location} is uncompatable with collapse variable {collapse_var}.\n",
                                                   "Coercing location to \"{switch(collapse_onto, x = \"bottom\", y = \"left\")}\""))
                                         location <- switch(collapse_onto, x = "bottom", y = "left")
                                       }
                                       midpoint <- self$midpoint %||% case_when(location%in%c("bottom","left") ~ (min(collapse_dat) - scale_range),
                                                                                location%in%c("top",  "right") ~ (max(collapse_dat) + scale_range))
                                       #do something with instance
                                       instance <- self$instance %||% 1 #1 is placeholder??

                                       list(
                                         collapse_var = collapse_var,
                                         collapse_onto = collapse_onto,
                                         midpoint = midpoint,
                                         scale_range = scale_range,
                                         location = location,
                                         instance = instance
                                       )

                                     },
                                     setup_data = function(data, params){
                                       browser()
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
                                       from_range <- range(data[[paste0(cvar,"max")]], data[[paste0(cvar,"min")]])
                                       collapse_range <- params$midpoint + (c(-1,1)*c(params$scale_range/2))
                                       data[[cvar]] <- scales::rescale(data[[cvar]], to = collapse_range, from = from_range)
                                       data[[paste0(cvar,"max")]] <- scales::rescale(data[[paste(cvar,"max")]], to = collapse_range, from = from_range)
                                       data[[paste0(cvar,"min")]] <- scales::rescale(data[[paste(cvar,"min")]], to = collapse_range, from = from_range)
                                       data
                                     })



position_collapse <- function(collapse = "y", midpoint = NULL, scale_range = NULL, location = NULL){
  ggproto(NULL, PositionCollapse, collapse = collapse, midpoint = midpoint, scale_range = scale_range, location = location)
}
