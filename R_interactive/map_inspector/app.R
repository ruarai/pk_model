library(shiny)
library(leaflet)
library(raster)
library(sp)

library(tidyr)

library(ggplot2)
library(dplyr)

base_wd <- "/data/gpfs/projects/punim1449/knowlesi_ruarai"
risk_map_file <- "output/update/rasters/with_new_covs_naive_risk"

risk_map <- raster(file.path(base_wd,risk_map_file))
cols <- colorRampPalette(c("#55843b", "#a4be79","#ffffbf", "#921d67", "#6c0043"))(100)

covs_file <- "data/clean/raster/SEAsia_covs_v2"

covs_sea <- brick(file.path(base_wd,covs_file))
covs_vals <- getValues(covs_sea)

min_vals <- sapply(1:ncol(covs_vals), function(i) min(covs_vals[,i], na.rm = TRUE))
max_vals <- sapply(1:ncol(covs_vals), function(i) max(covs_vals[,i], na.rm = TRUE))

effect_data <- readRDS(file.path(base_wd,"output/update/with_new_covs_naive_effect_plot.rds"))

ui <- fluidPage(
    tags$head(tags$style(HTML(".leaflet-container {cursor: auto !important;}"))),
    sidebarLayout(
        sidebarPanel(
            plotOutput("data_plot"),
            plotOutput("effect_plot")
        ),
        mainPanel(leafletOutput("mymap",
                                height=900))
    )
)

server <- function(input, output, session) {
    
    points <- eventReactive(input$recalc, {
    }, ignoreNULL = FALSE)
    
    observe({
        click = input$mymap_click
        if(is.null(click))
            return()
        
        # Display our text
        text <- paste0("Latitude: ", round(click$lat,2), ", Longitude: ", round(click$lng,2))
        
        map1_proxy = leafletProxy("mymap") %>%
            clearPopups() %>%
            addPopups(click$lng, click$lat, text)
        
        point <- SpatialPoints(data.frame(Longitude = click$lng, Latitude = click$lat),
                               proj4string = crs(risk_map))
        
        print(point)
        
        vals_raw <- raster::extract(covs_sea, point)[1,]
        
        vals_adj <- (vals_raw - min_vals) / (max_vals - min_vals)
        
        values <- vals_adj %>%
            data.frame(value = .,
                       name = names(vals_adj)) %>%
            tibble()
        
        p <- ggplot(values) + 
            geom_col(aes(y=name, x = value)) +
            xlab("Quantile") + ylab("Covariate")
        
        inv_logit <- function(x) exp(x)/(1+exp(x))
        
        values_effect <- values %>%
            mutate(x = round(value,2)) %>%
            select(name, x) %>%
            left_join(effect_data, by=c('name' = 'var_name', 'x' = 'x')) %>%
            mutate(prob = inv_logit(value))
        
        
        p_effect <- ggplot(values_effect) + 
            geom_point(aes(y=name, x = prob)) +
            xlab("Probability") + ylab("Covariate") +
            xlim(0, 1)
        
        output$data_plot <- renderPlot({ p })
        output$effect_plot <- renderPlot({ p_effect })
        
        print(inv_logit(mean(values_effect$value, na.rm=TRUE)))
        
    })
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addRasterImage(risk_map, 
                           opacity = 0.8,
                           colors = cols)
    })
}

shinyApp(ui, server)