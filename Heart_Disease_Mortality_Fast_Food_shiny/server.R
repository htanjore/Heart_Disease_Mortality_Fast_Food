

server <- function(input, output) {
  
  output$FFR_tmap = renderLeaflet({
    
    data <- FFR_shape
    var = input$var
  
  
    
    tm_style= input$style
    
    tm <- tm_shape(data, projection = 2163) +
      tm_fill(input$var,
              midpoint = 0,
              n=9,
              palette = "Set1",
              border.col = "black",
              title= input$var,
              border.alpha = .5,
              id = "NAME",
              textNA = "Unreliable",
              colorNA = "grey",
              popup.vars = c(input$var, "Percent Change in FFR from 09 to 14 per 1000 residents: " = "Percent_Change_FFR_2009_2014",
                                "Number of FastFood Resturants 09: " = "FFR09",
                                "Number of FastFood Resturants 14: " = "FFR14"))+
      tm_polygons(col = var, alpha = 0.5, border.col = 'white')+
      tm_bubbles(size="Percent_Change_FFR_2009_2014", col = "Percent_Change_FFR_2009_2014",
                 palette = mycols,
                  midpoint = NA,
                 title.size="FFR % change 09 to 14",
                 title.col = "FFR % change 09 to 14",
                 popup.vars = c("County: " = "NAME", "Heart Disease Age Adjusted Mortality rate: " = "Age_Adjusted_Rate",
                                "Percent Change in FFR from 09 to 14 per 1000 residents: " = "Percent_Change_FFR_2009_2014",
                                "Number of FastFood Resturants 09: " = "FFR09",
                                "Number of FastFood Resturants 14: " = "FFR14"))+
      tm_shape(US_states) +
      tm_borders(lwd=2, col = "black", alpha = .25) +
      tm_style(input$style)+
      tm_layout(title = "Heart Disease Mortality and Fast Food Resturants",
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
            palette = "Set1",
            border.col = "white",
            title=input$var,
            border.alpha = 0.5,
            id = "NAME",
            textNA = 'Unreliable',
            colorNA = "grey",
            popup.vars = c(input$var, "Percent Change in FFR from 09 to 14 per 1000 residents: " = "Percent_Change_FFR_2009_2014",
                           "Number of FastFood Resturants 09: " = "FFR09",
                           "Number of FastFood Resturants 14: " = "FFR14"))+
      tm_polygons(col = var, alpha = 0.5, border.col = 'white',title=input$var)+
      tm_bubbles(size ="Percent_Change_FFR_2009_2014", col = "Percent_Change_FFR_2009_2014",
                 palette = mycols,
                 midpoint = NA,
                 title.size=input$var,
                 title.col = "FFR % change 09 to 14",
                 popup.vars = c("County: " = "NAME", "Heart Disease Age Adjusted Mortality rate: " = "Age_Adjusted_Rate",
                                              "Percent Change in FFR from 09 to 14 per 1000 residents: " = "Percent_Change_FFR_2009_2014",
                                              "Number of FastFood Resturants 09: " = "FFR09",
                                              "Number of FastFood Resturants 14: " = "FFR14"))+
      tmap_options(max.categories = 14)+
      tm_shape(US_states) +
      tm_borders(lwd=2, col = "black", alpha = .25) +
      tm_style(input$style)+
      tm_layout(title = "Heart Disease Mortality and Fast Food Resturants",
                title.size = 1.1,
                title.position = c("center", "top"))+
      tm_legend(legend.position = c("right", "bottom"))
 
    tmap_leaflet(tm1) 
  })
}


