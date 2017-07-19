shinyUI(dashboardPage(
    dashboardHeader(title = 'MOTOR COLLISION'),
    dashboardSidebar(
      
      sidebarUserPanel("0xiNach",image = "http://www.zrox.org/0x.png"),
      sidebarMenu(id = 'sideBarMenu',
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Heatmap", tabName = "heatmap1", icon = icon("map")),
            menuItem("Chart",tabName = "chart", icon = icon("bar-chart"),
                    menuSubItem("Collision by Year",tabName = "plot1", icon = icon("line-chart")),
                    menuSubItem("Collision by Borough",tabName = "plot7", icon = icon("bar-chart")),
                    menuSubItem("Collision by Day",tabName = "plot2", icon = icon("bar-chart")),
                    menuSubItem("Collision by Hour",tabName = "plot3", icon = icon("area-chart")),
                    menuSubItem("Contributing Factor", tabName = "plot4", icon = icon("area-chart")),
                    menuSubItem("Are pedestrians safe?", tabName = "plot5", icon = icon("line-chart")),
                    menuSubItem("Factor affecting Injury", tabName = "plot6", icon = icon("line-chart"))),
            menuItem("Data",tabName = "data", icon = icon("database"))),
                    
                    
      
      
        conditionalPanel("input.sideBarMenu == 'map'",

                        selectizeInput('borough1',
                                        'BOROUGH',
                                        choice1, selected = "MANHATTAN"),
                        
                        selectizeInput('vehicle1',
                                       'VEHICLE TYPE',
                                       c(ALL = 'ALL', choice)),
                        
                        selectizeInput('injure1','INJURED',c(ALL = "ALL",
                                                            "PERSON"="`number of persons injured` > 0",
                                                            "PEDESTRIAN"="`number of pedestrians injured` > 0",
                                                            "CYCLIST"="`number of cyclist injured` > 0",
                                                            "MOTORIST"="`number of motorist injured` > 0")),
                        dateRangeInput('dateRange',
                                       label = 'Date range input: yyyy/mm/dd',
                                       start = '2013-01-01', end = '2017-12-31',
                                       min = '2013-01-01', max = '2017-12-31'),
                        

                        radioButtons("times1", "Select time: ",
                                     c('All' = "time >= '00:00:01' & time <= '23:59:59'",
                                       "12:00 am - 6:00 am" = "time >= '00:00:01' & time <= '06:00:00'",
                                       "6:00 am - 12:00 pm" = "time >= '06:00:00' & time <= '12:00:00'",
                                       "12:00 pm - 6:00 pm" = "time >= '12:00:00' & time <= '18:00:00'",
                                       "6:00 pm - 12:00 am" = "time >= '18:00:00' & time <= '23:59:59'"))

                       
                    #actionButton('add', 'Add marker cluster')
                    #actionButton('clear', 'Clear marker cluster')
      
      
        ),
      
      conditionalPanel("input.sideBarMenu =='data'",
                       
                       selectizeInput("selected",
                                      "Select Item to Display",
                                      choice2)
                       
                       
                       ),
      
      conditionalPanel("input.sideBarMenu =='chart'"
                       
                      
                       ),
      
      
      conditionalPanel("input.sideBarMenu == 'heatmap1'",
                      
                    selectizeInput('borough',
                                   'BOROUGH',
                                    choice1, selected = "MANHATTAN" ),

                    
                    selectizeInput('vehicle',
                                   'VEHICLE TYPE',
                                    c(ALL = 'ALL', choice) ),
                    
                    selectizeInput('injure','INJURED',c(ALL = "ALL",
                                                       "PERSON"="`number of persons injured` > 0",
                                                       "PEDESTRIAN"="`number of pedestrians injured` > 0",
                                                       "CYCLIST"="`number of cyclist injured` > 0",
                                                       "MOTORIST"="`number of motorist injured` > 0")),
                    
                    
                    selectizeInput('killed','KILLED',c(ALL = "ALL",
                                                       "PERSON"="`number of persons killed` > 0",
                                                       "PEDESTRIAN"="`number of pedestrians killed` > 0",
                                                       "CYCLIST"="`number of cyclist killed` > 0",
                                                       "MOTORIST"="`number of motorist killed` > 0" )),
                    
                    sliderInput('heatslide', label = 'Set size of radius (in meters): ', 
                                min = 50, max = 500, value = 150, step = 50),
                    dateRangeInput('dateRange',
                                   label = 'Date range input: yyyy/mm/dd',
                                   start = '2013-01-01', end = '2017-12-31',
                                   min = '2013-01-01', max = '2017-12-31'),
                    
                    radioButtons("times", "Select time: ",
                                 c('All' = "time >= '00:00:00' & time <= '23:59:59'",
                                   "12:00 am - 6:00 am" = "time >= '00:00:01' & time <= '06:00:00'",
                                   "6:00 am - 12:00 pm" = "time >= '06:00:00' & time <= '12:00:00'",
                                   "12:00 pm - 6:00 pm" = "time >= '12:00:00' & time <= '18:00:00'",
                                   "6:00 pm - 12:00 am" = "time >= '18:00:00' & time <= '23:59:59'"))
                    
                    
                    
                    
    )),
       
      dashboardBody(
          tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
          ),
          tabItems(
              tabItem(tabName = "map",
                      fluidRow(box(
                      leafletOutput("map",
                                    height = 650),
                      width = 12))),
            tabItem(tabName = "heatmap1",
                    fluidRow(box(
                      leafletOutput("heatmap",height = 800),
                      width = 12))),
            tabItem(tabName = "data",
                    fluidRow(box(DT::dataTableOutput("table"), width = 16))),
            tabItem(tabName = "plot1",
                    fluidRow(box(
                      plotOutput("plot1",height = 650),
                      width = 12))),
            tabItem(tabName = "plot2",
                    fluidRow(box(
                      plotOutput("plot2",height = 650),
                      width = 12))),
            tabItem(tabName = "plot3",
                    fluidRow(box(
                      plotOutput("plot3",height = 650),
                      width = 12))),
            tabItem(tabName = "plot4",
                    fluidRow(box(
                      plotOutput("plot4",height = 650),
                      width = 12))),
            tabItem(tabName = "plot5",
                    fluidRow(box(
                      plotOutput("plot5",height = 650),
                      width = 12))),
            tabItem(tabName = "plot6",
                    fluidRow(box(
                      plotOutput("plot6",height = 650),
                      width = 12))),
            tabItem(tabName = "plot7",
                    fluidRow(box(
                      plotOutput("plot7",height = 650),
                      width = 12)))
    )
  )
))