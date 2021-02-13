


XLayer <- ggplot2::ggproto("XLayer",
                           ggplot2:::Layer,
                           setup_layer = function(self, data, plot){
                             data$PANEL_TYPE <- "x"
                             data
                           })

YLayer <- ggplot2::ggproto("YLayer",
                           ggplot2:::Layer,
                           setup_layer = function(self, data, plot){
                             data$PANEL_TYPE <- "y"
                             data
                           })
