

shinyServer(function(input, output,session){
  
  
  
  output$map <- renderLeaflet({
    leaflet()  %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      setView(lat = 40.75898, lng = -73.99394,zoom = 11)
  })
      
  observe({
  
      proxy <- leafletProxy("map") %>%
      clearMarkers() %>%   
      clearMarkerClusters() %>% 
      addCircleMarkers(data = moto1 %>% filter(borough == input$borough1) %>%
      filter_(input$times1) %>% filter_(ifelse(input$vehicle1 == 'ALL',TRUE,"`vehicle type code 1` == input$vehicle1")) %>% 
        filter_(ifelse(input$injure1 == 'ALL',TRUE,input$injure1)) %>% filter(date1 > input$dateRange[1] & date1 < input$dateRange[2]),
      lng = ~longitude,lat = ~latitude,color = 'Red',radius =1,clusterOptions = markerClusterOptions(),group = 'CLUSTER') %>% 
      addCircleMarkers(data = moto1 %>% filter(borough == input$borough1) %>%
      filter_(input$times1) %>% filter_(ifelse(input$vehicle1 == 'ALL',TRUE,"`vehicle type code 1` == input$vehicle1")) %>% 
        filter_(ifelse(input$injure1 == 'ALL',TRUE,input$injure1)) %>% filter(date1 > input$dateRange[1] & date1 < input$dateRange[2]),
      lng = ~longitude,lat = ~latitude,color = 'Red',radius =1,group = 'CIRCLE') %>% 
        addLayersControl(
        baseGroups = c("CLUSTER","CIRCLE"),
        options = layersControlOptions(collapsed = FALSE)
      )  

    })
  
  
  # observeEvent(input$add, {
  #   proxy <- leafletProxy('map') %>% addMarkers(
  #     data = data1,
  #     clusterOptions = markerClusterOptions(), clusterId = 'cluster1'
  #   )
  # })
  
  output$heatmap <- renderLeaflet({
    leaflet() %>%  
    addProviderTiles(providers$CartoDB.DarkMatter) %>% 
      setView(lat = 40.75898, lng = -73.99394,zoom = 11) %>% 
      addLegend("topright", pal = colorNumeric(c("Red","Green","Blue"), domain = NULL, reverse = TRUE),
                values = moto1$latitude, title = "Collision",opacity = 1.5)
    
    })
  
  observe({
    
    
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
  
  output$plot1 <- renderPlot({
    
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
  
  
  output$plot2 <- renderPlot({
    
    moto1 %>% group_by(day,borough) %>% 
      arrange(day) %>% summarise(n = n()) %>% 
      ggplot(aes(x = day,y= n)) +
      geom_col(aes(fill = day)) + 
      facet_grid(~borough) + 
      ggtitle("WHO LOVES FRIDAY...UGHHHH?") + 
      theme(plot.title = element_text(hjust = 0.5)) +
      ylab("Number of collision") + xlab("Day") +
      theme(axis.text.x=element_text(angle =75,hjust=1))
  })
  
  
  output$plot3 <- renderPlot({
    
    moto1 %>% mutate(hour = hour(hms(as.character(time)))) %>%
      group_by(hour,borough) %>% summarise(Collisions = n()) %>%
      ggplot(aes(x = borough,y = hour)) + 
      geom_raster(aes(fill = Collisions)) + 
      scale_fill_gradient(low = "darkgreen",high = "yellow") +
      ggtitle("Heatmap of hour of day has most number of collision") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plot4 <- renderPlot({
    
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
  
  
  output$plot5 <- renderPlot({
    
    moto1 %>% group_by(borough,year) %>% 
      summarise(person_injured = sum(`number of pedestrians injured`)/n(),injured = sum(`number of pedestrians injured`),n()) %>%
      ggplot(aes(x =year,y = person_injured )) + 
      geom_line(aes(color = borough)) + 
      geom_point(aes(color = borough)) + 
      geom_text(aes(label = injured),vjust =-1,size = 4,check_overlap = FALSE) + 
      scale_x_continuous(breaks = seq(2012,2017)) + 
      ggtitle("Pedestrian Injured vs Year(Boroughwise)") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plot6 <- renderPlot({
    
    moto1 %>% group_by(year,`contributing factor vehicle 1`) %>% 
      summarise(Pedestrian_injured = sum(`number of pedestrians injured`))  %>% 
      filter(`contributing factor vehicle 1` != 'Unspecified' & `contributing factor vehicle 1`!= 'Traffic Control Device Improper/Non-Working' & `contributing factor vehicle 1`!= 'Driverless/Runaway Vehicle') %>% 
      ggplot(aes(y = `contributing factor vehicle 1`,x = year)) +
      geom_raster(aes(fill = Pedestrian_injured)) + 
      scale_x_continuous(breaks = seq(2012,2017)) + 
      scale_fill_gradient(low = "darkgreen",high = "yellow") + 
      ylab("Contributing Factor") +
      ggtitle("Heatmap of Contributing Factor vs Pedestrian Injured")
  
  })
  
  output$plot7 <- renderPlot({
    moto1 %>% group_by(borough,year) %>% 
      summarise(n = n()) %>% ggplot(aes(x = year,y= n)) + 
      geom_col(aes(fill = borough)) + 
      facet_grid(~borough) + 
      ggtitle("Number of Collision by Borough and Year ") + 
      ylab("Number of collision") +
      theme(plot.title = element_text(hjust = 0.5)) + 
      scale_x_continuous(breaks = seq(2012,2017)) + 
      theme(axis.text.x=element_text(angle =45,hjust=1))
  })
  
  output$table <- DT::renderDataTable({
    datatable(head(moto1,100),options = list(scrollX = TRUE), rownames=FALSE) %>% 
      formatStyle(input$selected, background="skyblue", fontWeight='bold')
  })
  

})

  



                                             
                            

  
    












