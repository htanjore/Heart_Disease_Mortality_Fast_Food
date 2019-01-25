

server <- function(input, output) {
  
  output$FFR_tmap = renderLeaflet({

    data <- FFR_shape
    var = input$var
    
  

    tm <- tm_shape(data, projection = 2163) +
      tm_fill(input$var,
              midpoint = 0,
              n=9,
              palette = mycols2,
              border.col = "black",
              title= input$var,
              border.alpha = .5,
              id = "NAME",
              textNA = "Unreliable",
              colorNA = "grey",
              popup.vars = c(input$var,"Age Adjusted Mortality rate: " = "Age Adjusted Rate",
                             "Percent Change in FFR from 09 to 14 per 1000 residents: " = "FFR percent change 09 to 14",
                             "Number of FastFood Resturants 09: " = "FFR09",
                             "Number of FastFood Resturants 14: " = "FFR14"))+
      tm_polygons(col = var, alpha = 0.5, border.col = 'white')+
      tm_bubbles(size="FFR percent change 09 to 14", col = "FFR percent change 09 to 14",
                 midpoint = NA,
                 title.size="FFR percent change 09 to 14",
                 title.col = "FFR percent change 09 to 14",
                 palette = mycols,
                 popup.vars = c("County: " = "NAME", "Age Adjusted Mortality rate: " = "Age Adjusted Rate",
                                "Percent Change in FFR from 09 to 14 per 1000 residents: " = "FFR percent change 09 to 14",
                                "Number of FastFood Resturants 09: " = "FFR09",
                                "Number of FastFood Resturants 14: " = "FFR14"))+
      tm_shape(US_states) +
      tm_borders(lwd=2, col = "black", alpha = .25) +
      tm_style("natural")+
      tm_layout(title = "",
                title.size = 1.1,
                title.position = c("center", "top"),bg.color = "gray95")+
      tm_legend(legend.position = c("right", "bottom"))

    tmap_leaflet(tm)



  })
  
  output$FFR_TN = renderLeaflet({
    
    map_data <- FFR_shape %>% 
      filter(State_name == input$state)
    var = input$var
  
tm1 <- tm_shape(map_data, projection = 2163)+
  
    tm_fill(input$var,
            midpoint = 0,
            n=9,
            palette = mycols2,
            border.col = "black",
            title=input$var,
            border.alpha = 0.5,
            id = "NAME",
            textNA = 'Unreliable',
            colorNA = "darkgrey",
            popup.vars = c(input$var,"Age Adjusted Mortality rate: " = "Age Adjusted Rate",
                           "Percent Change in FFR from 09 to 14 per 1000 residents: " = "FFR percent change 09 to 14",
                           "Number of FastFood Resturants 09: " = "FFR09",
                           "Number of FastFood Resturants 14: " = "FFR14"))+
      tm_polygons(col = var, alpha = 0.5, border.col = 'black',title=input$var)+
      tm_text("County", size = 1, xmod = .5, ymod = 1.5)+
      tm_bubbles(size="FFR percent change 09 to 14", col = "FFR percent change 09 to 14",
             midpoint = NA,
             title.size="FFR percent change 09 to 14",
             title.col = "FFR percent change 09 to 14",
             palette = mycols,
             alpha = 1,
             popup.vars = c("County: " = "NAME", "Age Adjusted Mortality rate: " = "Age Adjusted Rate",
                            "Percent Change in FFR from 09 to 14 per 1000 residents: " = "FFR percent change 09 to 14",
                            "Number of FastFood Resturants 09: " = "FFR09",
                            "Number of FastFood Resturants 14: " = "FFR14"))+
      tmap_options(max.categories = 14)+
      tm_shape(US_states) +
      tm_borders(lwd=2, col = "black", alpha = .25) +
      tm_style("natural")+
      tm_layout(title = "",
                title.size = 1.1,
                title.position = c("center", "top"))+
      tm_legend(legend.position = c("right", "bottom"))
 
    tmap_leaflet(tm1)
      
    
      
  })
}


