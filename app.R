rm(list=ls())
#install.packages(c("shiny", "leaflet", "rgdal", "mapview"))

library(shiny)
library(leaflet)
library(rgdal)
library(mapview)

load("./NUTS0.rData") 
NUTS0 <- data
load("./NUTS2.rData") 
NUTS2 <- data
load("./NUTS3.rData") 
NUTS3 <- data
load("./GEMEINDEN.rData") 
GEMEINDEN <- data

levels <- list("NUTS0" = list(shapeFile = NUTS0, long=13.1, lat=47.5, zoom=7),
               "NUTS2" = list(shapeFile = NUTS2, long=13.1, lat=47.5, zoom=7), 
               "NUTS3" = list(shapeFile = NUTS3, long=13.1, lat=47.5, zoom=7),
               "GEMEINDEN" = list(shapeFile = GEMEINDEN, long=13.1, lat=47.5, zoom=7),
               "GEMEINDEN - BURGENLAND" = list(shapeFile = GEMEINDEN[GEMEINDEN@data$NUTS2_ID == "AT11",], long=17.1, lat=47.5, zoom=8),
               "GEMEINDEN - NIEDERÖSTERREICH" = list(shapeFile = GEMEINDEN[GEMEINDEN@data$NUTS2_ID == "AT12",], long=15.8, lat=48.3, zoom=8),
               "GEMEINDEN - WIEN" = list(shapeFile = GEMEINDEN[GEMEINDEN@data$NUTS2_ID == "AT13",], long=16.4, lat=48.2, zoom=11),
               "GEMEINDEN - KÄRNTEN" = list(shapeFile = GEMEINDEN[GEMEINDEN@data$NUTS2_ID == "AT21",], long=13.9, lat=46.7, zoom=9),
               "GEMEINDEN - STEIERMARK" = list(shapeFile = GEMEINDEN[GEMEINDEN@data$NUTS2_ID == "AT22",], long=15.0, lat=47.3, zoom=8),
               "GEMEINDEN - OBERÖSTERREICH" = list(shapeFile = GEMEINDEN[GEMEINDEN@data$NUTS2_ID == "AT31",], long=14.0, lat=48.2, zoom=8),
               "GEMEINDEN - SALZBURG" = list(shapeFile = GEMEINDEN[GEMEINDEN@data$NUTS2_ID == "AT32",], long=13.1, lat=47.5, zoom=9),
               "GEMEINDEN - TIROL" = list(shapeFile = GEMEINDEN[GEMEINDEN@data$NUTS2_ID == "AT33",], long=11.4, lat=47.2, zoom=9),
               "GEMEINDEN - VORARLBERG" = list(shapeFile = GEMEINDEN[GEMEINDEN@data$NUTS2_ID == "AT34",], long=9.9, lat=47.2, zoom=9))

varNames = names(NUTS2@data)[3:ncol(NUTS2@data)]

ui <- fluidPage(
    fluidRow(
        column(1),
        column(4,
               selectInput("variable", "Variable:",
                           varNames)),
        column(4,
               selectInput("level", "Level:",
                           names(levels)))
    ),
    leafletOutput("map", width = "100%", height = 600),
    fluidRow(
        column(10,
               downloadButton('exported_map.png'))
    )

    
)


server <- function(input, output, session) {
    
    colorInput <- reactive({
        switch(input$variable,
               "RandNum1" = "Blues",
               "RandNum2" = "Oranges"
               )
    })
    
    pal <- reactive({
        colorNumeric(palette = colorInput(), domain = levels[[input$level]]$shapeFile@data[[input$variable]])
    }) 
    
    selected.df <- reactive({
        data <- levels[[input$level]]$shapeFile@data[, c(1, 2)]
        data[[input$variable]] <- levels[[input$level]]$shapeFile@data[[input$variable]]
        data
    })

    mymap <- reactive({
        # here I have specified a tile from openstreetmap
        leaflet() %>%
            addProviderTiles(providers$OpenStreetMap, group="OSM") %>% 
            addProviderTiles("Esri.WorldImagery", group="ESRI World Imagery") %>%
            addProviderTiles(providers$OpenStreetMap, group="OSM")
    })
    
    output$map <- renderLeaflet({
        mymap()
    })
    
    myfun <- function(map){
        pal_map = pal()
        selectedData = selected.df()
        req(input$variable)
        setView(map, levels[[input$level]]$long, levels[[input$level]]$lat, levels[[input$level]]$zoom) %>%
        clearShapes() %>%
        addPolygons(
            data = levels[[input$level]]$shapeFile,  # LAD polygon data from geojson
            weight = 1,  # line thickness
            opacity = 1,  # line transparency
            color = "gray",  # line colour
            fillOpacity = 0.7,
            fillColor =  ~pal_map(levels[[input$level]]$shapeFile@data[[input$variable]]),
            group="Polygons",
            label = ~paste0(NAME, ": ", format(round(selectedData[[input$variable]]*100, 2), nsmall = 2), "%"),  # LAD name as a hover label
            highlightOptions = highlightOptions(color = "black", weight = 1, 
                                                bringToFront = TRUE)
        ) %>%
        addPolylines(
            data = levels[["NUTS2"]]$shapeFile,  # LAD polygon data from geojson
            weight = 1,  # line thickness
            opacity = 1,  # line transparency
            color = "black",  # line colour
            group="Polygons"
        ) %>%
        addLegend(pal = pal_map, values = selectedData[[input$variable]], opacity = 0.7, title = input$variable,
                  position = "bottomright")
    }
    

    observe({
        pal_map = pal()
        selectedData = selected.df()
        leafletProxy("map") %>% myfun() %>% 
        clearControls() %>% 
        addLayersControl(overlayGroups = c("Polygons"),baseGroups = c("OSM", "ESRI World Imagery"), options = layersControlOptions(collapsed = FALSE)) %>%
        addLegend(pal = pal_map, values = selectedData[[input$variable]], opacity = 0.7, title = input$variable,
                      position = "bottomright")
        
    })
    
    # map that will be downloaded
    mapdown <- reactive({
        # we need to specify coordinates (and zoom level) that we are currently viewing
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        mymap() %>% myfun() %>% setView(lng = (lngRng[1]+lngRng[2])/2, lat = (latRng[1]+latRng[2])/2, zoom = input$map_zoom)
    })
    
    output$`exported_map.png` <- downloadHandler(
        filename = 'exported_map.png',
        
        content = function(file) {
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            
            # using mapshot we can substitute the above two lines of code
            mapshot(mapdown(), file = file, cliprect = "viewport")
        }
    )
    
}


shinyApp(ui=ui, server=server)
