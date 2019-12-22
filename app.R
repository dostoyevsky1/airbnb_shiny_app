
source('global.R')



vars = c('Price' = 'price',
         'Borough' = 'neighbourhood_group',
         'Neighborhood' = 'neighbourhood',
         'Monthly Reviews' = 'reviews_per_month',
         'Total Reviews' = 'number_of_reviews',
         'Minimum Nights' = 'minimum_nights',
         'Room Type' = 'room_type')

vars2 = c('None' = 'none',
          'Price' = 'price',
          'Monthly Reviews' = 'reviews_per_month',
          'Total Reviews' = 'number_of_reviews')

ui = navbarPage("NYC AirBnB", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                            # Include our custom CSS
                            includeCSS("./styles.css")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"))),
           
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, top = 60, left = "auto", right = 1220, bottom = "auto",
                         width = 800, height = "auto",
                         
                         h4("NYC AirBnB Explorer"),
                         
                         selectInput("color", "Color", vars, selected = 'price', width = 'auto'),
                         selectInput("size", "Size", vars2, selected = 'price', width = 'auto'),
                         plotOutput("p_hist", height = 200, width = 750)
                         ))
           
                        

# Define server logic required to draw a histogram
server = function(input, output, session) {
    
    map = leaflet() %>% 
        addProviderTiles('MapBox',
                         options = providerTileOptions(id = 'mapbox.dark',noWrap=FALSE,
                                                       accessToken = 'pk.eyJ1IjoibWRyaXp6eSIsImEiOiJjazRnN3R6N2Qwd2tlM2xtdzl4Z3J5ajFxIn0.aC1WFSuBLtK6aWQdvnIRsg'
                         )) %>% 
        setView(lng = -73.9855, lat = 40.7000, zoom = 12)
    
    observe({
        
        colorBy = input$color
        sizeBy = input$size
    
        if(colorBy == 'price'){
            bnbdat = bnb
            pal = colorFactor('YlGn', bnbdat[,colorBy])
        } else if(colorBy == 'number_of_reviews' |
                  colorBy == 'minimum_nights') {
            bnbdat = bnb
            pal = colorFactor('Reds', bnbdat[,colorBy])
        } else if(colorBy == 'reviews_per_month'){
            bnbdat = na.omit(bnb)
            pal = colorFactor('Reds', bnbdat[,colorBy])
        } else {
            bnbdat = bnb
            pal = colorFactor('Accent', bnbdat[,colorBy])
        }
        
        
        if(sizeBy == 'price'){
            
            radius = bnbdat$p_radius   
            
        } else if(sizeBy == 'reviews_per_month') {
            
            radius = bnbdat$r_radius
        
        } else if(sizeBy == 'number_of_reviews'){
            
            radius = bnbdat$b_radius
            
        } else if(sizeBy == 'none') {
            radius = 1
        }
        
        
        leafletProxy("map", data = bnbdat) %>%
            clearMarkers() %>%
            addCircleMarkers(~longitude, ~latitude, radius=radius,fillOpacity = 0.65, 
                             fillColor = ~pal(bnbdat[,colorBy]), stroke = FALSE,
                             label = ~paste(paste0('Price: ','$',as.character(price)),
                                             paste('Neighborhood: ', as.character(neighbourhood)),
                                             paste('Minimum Nights: ', as.character(minimum_nights)),
                                             paste('Room Type: ', as.character(room_type)), sep = ' / '))
    
        if(colorBy != 'neighbourhood'){
            p_hist = ggplot() + geom_density(aes(x=bnbdat[,colorBy]), fill = '#00DD00') + ggtitle(paste0('Distribution of ',colorBy)) + xlab(colorBy) + ylab('Density')
        } else {
            p_hist = ggplot(data=bnbdat) + geom_density(aes(x=neighbourhood), fill = '#00DD00') + facet_wrap(~neighbourhood_group) + 
                ylim(0,0.3) +
                ggtitle(paste0('Density per ',colorBy,'/borough')) + xlab(colorBy) + ylab('Density') + theme(axis.text.x = element_blank())
        }
            
            output$p_hist = renderPlot({p_hist})
        
    })
    
    output$map = renderLeaflet({map})
    
        
}

# Run the application 
shinyApp(ui = ui, server = server)
