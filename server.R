

shinyServer(function(input, output,session){
  
## rendering geospatial plot with circle markers and clusters  
  
  output$map <- renderLeaflet({
    leaflet()  %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      setView(lat = 40.75898, lng = -73.99394,zoom = 11)   #set default view
  })
      
  observe({   #observe the input from user
  
      proxy <- leafletProxy("map") %>%    #leaflet proxy will measure the input and change according to input
      clearMarkers() %>%   
      clearMarkerClusters() %>% 
      addCircleMarkers(data = moto1 %>% filter(borough == input$borough1) %>%
      filter_(input$times1) %>% filter_(ifelse(input$vehicle1 == 'ALL',TRUE,"`vehicle type code 1` == input$vehicle1")),
      lng = ~longitude,lat = ~latitude,color = 'Red',radius =1,group = 'CIRCLE') %>% 
      addCircleMarkers(data = moto1 %>% filter(borough == input$borough1) %>%
            filter_(input$times1) %>% filter_(ifelse(input$vehicle1 == 'ALL',TRUE,"`vehicle type code 1` == input$vehicle1")) ,
            lng = ~longitude,lat = ~latitude,color = 'Red',radius =1,clusterOptions = markerClusterOptions(),group = 'CLUSTER') %>% 
      addLayersControl(                        #creating group for layers
        baseGroups = c("CIRCLE","CLUSTER"),
        options = layersControlOptions(collapsed = FALSE)
      )  

    })
  
  #heatmap of collision
  
  output$heatmap <- renderLeaflet({
    leaflet() %>%  
    addProviderTiles(providers$CartoDB.DarkMatter) %>%  #importing provider tiles
      setView(lat = 40.75898, lng = -73.99394,zoom = 11) %>% 
      addLegend("topright", pal = colorNumeric(c("Red","Green","Blue"), domain = NULL, reverse = TRUE),
                values = moto1$latitude, title = "Collision",opacity = 1.5)    #adding a legend for color gradient
    
    })
  
  observe({      #observing the heatmap filters from the user
    
    
       proxy <- leafletProxy("heatmap") %>% 
        removeWebGLHeatmap(layerId = 'a') %>%
        addWebGLHeatmap(layerId = 'a', data = moto1 %>% 
                        filter(borough == input$borough) %>%
                        filter_(input$times) %>% filter_(ifelse(input$vehicle == 'ALL',TRUE,"`vehicle type code 1` == input$vehicle")) %>% 
                      filter(date1 > input$dateRange[1] & date1 < input$dateRange[2]) %>% 
                        filter_(ifelse(input$injure == 'ALL',TRUE,input$injure)) %>% 
                        filter_(ifelse(input$killed == 'ALL',TRUE,input$killed)),
                      lng= ~longitude,lat= ~latitude,size = input$heatslide,
                      alphaRange = .01, opacity = .45)
                      
    })
  
  output$plot1 <- renderPlot({   #line chart & point chart -> number of accident vs year
    
    moto1 %>% group_by(year) %>% arrange(year) %>%
      summarise(number_of_accident = n()) %>% 
      ggplot(aes(x = year,y = number_of_accident )) +  
      geom_line(aes(colour = "Accidents")) + 
      geom_point(size = 2, color = 'red') + 
      geom_text(aes(label = number_of_accident),vjust =-1) + 
      theme_classic() + 
      labs(title = 'NYC Motor Vehicle Collisions 2012 - 2017',x = 'Year', y = 'Number of Accidents') +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  
  output$plot2 <- renderPlot({    #bar plot of day of week has most number of collision -> day vs year(borough wise)
    
    moto1 %>% group_by(day,borough) %>% 
      arrange(day) %>% summarise(n = n()) %>% 
      ggplot(aes(x = day,y= n)) +
      geom_col(aes(fill = day)) + 
      facet_grid(~borough) + 
      ggtitle("A day of week has most number of collision") + 
      theme(plot.title = element_text(hjust = 0.5)) +
      ylab("Number of collision") + xlab("Day") +
      theme(axis.text.x=element_text(angle =75,hjust=1))
  })
  
  
  output$plot3 <- renderPlot({  # Heatmap of hour of day has most number of collision
    
    moto1 %>% mutate(hour = hour(hms(as.character(time)))) %>%
      group_by(hour,borough) %>% summarise(Collisions = n()) %>%
      ggplot(aes(x = borough,y = hour)) + 
      geom_raster(aes(fill = Collisions)) + 
      scale_fill_gradient(low = "darkgreen",high = "yellow") +   #setting color gradient for low and high frequency
      ggtitle("Heatmap of hour of day has most number of collision") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plot4 <- renderPlot({       #Heatmap of contributing factor vs Year 
    
    moto1 %>% group_by(year,`contributing factor vehicle 1`) %>% 
      summarise(Collision = n()) %>% 
      filter(`contributing factor vehicle 1` != 'Unspecified' & `contributing factor vehicle 1`!= 'Traffic Control Device Improper/Non-Working' & `contributing factor vehicle 1`!= 'Driverless/Runaway Vehicle' & `contributing factor vehicle 1`!= 'Pedestrian/Bicyclist/Other Pedestrian Error/Confusion') %>%
      ggplot(aes(y = `contributing factor vehicle 1`,x = year)) +
      geom_raster(aes(fill = Collision)) + 
      scale_x_continuous(breaks = seq(2012,2017)) + 
      scale_fill_gradient(low = "darkgreen",high = "yellow") + 
      ylab("Contributing Factor") + 
      ggtitle("Heatmap of contributing factor vs Year") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  output$table <- DT::renderDataTable({         #rendering dataframe on shiny dashboard using DT
    datatable(head(moto1,100),options = list(scrollX = TRUE), rownames=FALSE) %>% 
      formatStyle(input$selected, background="skyblue", fontWeight='bold')
  })
  

})

  



                                             
                            

  
    












