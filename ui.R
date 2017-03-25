# Author : SeeRs team 
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# Purpose: The purpose of this code is to serve as the user interface to display the graphs
# and help user to give inputs for ggplot2 and googleVis
#**********************************************************************************************************
# Creation of the dashboard using the shiny-dashboard commands 

ui <- dashboardPage(
  dashboardHeader(title = "SeeRs dashboard"),
  # Sidebar content
  dashboardSidebar(
    sidebarMenu(
  # each menu item corresponds to a tab in sidebar
  # first arg is the name that will be visible in webpage, 
  # second arg is the shiny internal ID that will be used to link the code
  # third is the icon to be displayed, this can be customized to give better presentation to the tabs
  # more ways of customizing icons can be found here. https://rstudio.github.io/shinydashboard/appearance.html#icons
      menuItem("GGplot2 - I",         tabName = "GGPlot2",     icon = icon("dashboard")),
      menuItem('GGplot2 - II',        tabName = 'ggplot21',    icon = icon("dashboard")),
      menuItem("GGplot2 - III",       tabName = "GGplot22",    icon = icon("dashboard")),
      menuItem("googleVis - WorldMap",tabName = "googleVisI",  icon = icon("map-marker")), 
      menuItem("googleVis - Bubble Chart",tabName = "googleVisII", icon = icon("dashboard")),
      menuItem("googleVis - Motion Chart",tabName = "googleVisIII",icon = icon("dashboard"))
    )
  ),
  # Body content
  dashboardBody(
    tabItems(
  # First tab content
  # the tab is created inorder to capture the rendered ggplot graphs from the shiny server
  # The main functions of shiny used to create the client UI are tabitem(), fluidRow(), box(), plotOutput()
  # each of the functions are used to arrange the graphs on the dashboard
    tabItem(tabName = "GGPlot2",
              fluidRow(
               # Every plotoutput() statement below sends an input to the server to create a plot and 
               # displays the rendered graph
                column (6, box(title = "Per capita CO2 emission(Point Chart)",solidHeader = TRUE,
                               background = "navy",width = 12, plotOutput("pointplot", height = 500))),
                column( 6, box(title = "GDP Growth(Jitter Plot)",solidHeader = TRUE, background = "navy",
                               width = 12,plotOutput("jitterplot", height =200))),
                column(6, box(title = "GDP/Population rate(Path Chart)",solidHeader = TRUE, background = "navy",
                              width = 12,plotOutput("pathplot", height = 220))),
                column(6, box(title ="Co2 emission by India(Density Chart)",solidHeader = TRUE, background = "navy",
                              width = 12, plotOutput("plotIndia",height = 300))),
                # the below box is created to enter help manually select the year for which the user wants to see the 
                # co2 emission for India. 
                box(
                  title = "Control",
                  solidHeader = TRUE,
                  background = "black",
                  sliderInput("co2year", "Year of observation", 
                              min(co2$Year),           # min value that the slider should take
                              max(co2$Year),           # max value that the slider should take
                              c(min(co2$Year), 1912),  # if slider not moved 1912 is position as default
                              animate=T,               # animate =true makes it playable
                              step = 5,                # step=5 meaans it will increase by 5years
                              sep ="")                 # sep="" for removing commas in year
                )
              )
      ),
# ************************   Second Tab for ggplot**********************************  
    # This is the second tab 
      tabItem(tabName = "ggplot21", 
              fluidRow(
                # box in shinydashboard places visualization1 at a particular place in the output 
                box(title = "Population growth(Bar Chart)",solidHeader = TRUE, background = "navy",
                              width = 12, plotOutput("barplot", height = 300))),
              fluidRow(  
                box(title = " GDP vs CO2 Emission (Point Chart)",solidHeader = TRUE, background = "navy",
                              width = 6, plotOutput("plotworld",height = 300)),
                #sliderinput takes input as the name it will appear in the input and the second as the name of call in the server 
                box(
                  title = "Control",
                  solidHeader = TRUE,
                  background = "black",
                  sliderInput("yearInpop", "Year of Observation:",    # SliderInput() is an input the server
                              min(co2$Year),                          # min value that the slider should take
                              max(co2$Year),                          # max value that the slider should take
                              #median(co2$Year)                       # default value of the slider to start the control with
                              c(median(co2$Year),median(co2$Year)+1), # As the two slider for years is required
                              animate=T,                              # gives the option to play the selection of years 
                              step = 5,                               # the motion if auto steps at 5 years interval
                              sep =""
                              
                  )    
               )  
            )
              
      ),
# ************************   Third Tab for ggplot**********************************        
   # This is the third tab item used for displaying the bar charts and Combo Chart
      tabItem(tabName = "GGplot22",
              # Creates the controls for providing input to filter data for plots
              fluidRow(
                box(
              # Control for year
                  title = "Control", solidHeader = TRUE,background = "black",height = 150,
                  sliderInput("yearInpgdp", "Year of Observation:", 
                              min(co2$Year),     # min value that the slider should take
                              max(co2$Year),     # max value that the slider should take
                              median(co2$Year),  # single default year input goes to server 
                              animate=T,         # gives the option to play the selection of years 
                              step = 5,          # the motion if auto steps at 5 years interval
                              sep =""
                              )
                ),
                box(
              # Control for Region
                  title = "Control",background = "black",height = 150,
                  selectInput("region", "Region",
                              choices=c("Africa","Asia","Australia","Europe","North America","South America"))
                              )
              ),
              # Creates boxes for all plots with specified title,color and dimensions
             fluidRow(
                box(title = "GDP per Capita (Bar Chart)", background = "navy",solidHeader = TRUE,
                    plotOutput("barchart", height = 400)),
                # The htmlOutput() is required for the googlevis() graphs to be rendered.  
                box(title = "Population vs GDP (Combo Chart using googleVis)",background = "navy",solidHeader = TRUE,
                    htmlOutput("combochart")),
                box(title= "Contributors of CO2 Emission (Bar Chart)",background = "navy",solidHeader = TRUE, 
                    plotOutput("Emisplot"),height = 450, width = 12))
          ),

# ************************   Fourth Tab using googleVis**********************************              
      #  This is the fourth tab
      #  This tab displays how the CO2 emissions, GDP and Population has changed for all countries over the years
      #  slidercontrol determines the year for which the data needs to be presented
      #  it can also  be animated to  produce a time variation of the three values over the years.
      tabItem(tabName = "googleVisI", #The tabID that is internal to shiny and used to link the panel with sidebar
              #the layout is done in two rows with fixed cols marked by separate boxes
              fluidRow(
                box(title= "World Map of CO2 Emissions by Country", 
                    htmlOutput("co2map"), #googleVis outputs html thus we need htmlOutput holder
                    background = "navy",  #background of the box
                    solidHeader = TRUE 
                ),
                box(
                  title = "Year Control for Map Visual", 
                  solidHeader = TRUE,
                  background = "black",
                  sliderInput("mapyear", "Year of Observation:", 
                              min(co2$Year),    # min value that the slider should take
                              max(co2$Year),    # max value that the slider should take
                              median(co2$Year), # default value of the slider to start the control with
                              animate=T,        # gives the option to play the selection of years 
                              step = 5,         # the motion of slider is stepped by 5 years at a time
                              sep =""
                  )
                ) 
              ),
              fluidRow(
                box(title= "World Map of GDP by Country", 
                    htmlOutput("gdpmap"), #googleVis outputs html thus we need htmlOutput holder
                    background = "navy",  #background of the box
                    solidHeader = TRUE 
                ) ,
                box(title= "World Map of Population by Country", 
                    htmlOutput("popmap"), #googleVis outputs html thus we need htmlOutput holder
                    background = "navy",  #background of the box
                    solidHeader = TRUE 
                ) 
              )
      ),

# ************************   Fifth Tab using googleVis**********************************  
      # This tab displays how the CO2 emissions varies with GDP for all countries over the years
      # slidercontrol determines the year for which the data needs to be presented
      # it can also  be animated to  produce a time variation of the c02 emission over the years.  
      tabItem(tabName = "googleVisII",
              fluidRow(
                box(title = "CO2 Emission vs GDP with Population as size of Bubble (Bubble Plot)",
                    solidHeader = TRUE, width = 8,background = "navy",htmlOutput("bubbleplot")),
                
                column(4, box(
                  title = "Control", background = "black",width = 12,
                  sliderInput("yearInpbub", "Year of Observation:", 
                              min(co2$Year),     # min value that the slider should take
                              max(co2$Year),     # max value that the slider should take
                              median(co2$Year),  # default value of the slider to start the control with
                              animate=T,         # gives the option to play the selection of years 
                              step = 5,          # the motion of slider is stepped by 5 years at a time
                              sep =""
                              
                  ))
                )
              )
      ),
# ************************   Sixth Tab using googleVis**********************************  
      # This tab is for the motion chart for Region wise data
      tabItem(tabName = "googleVisIII",
              fluidRow(
                ##Create box for motion chart with specified title, color and dimensions
                box(title = "Region wise Visual (Motion Chart)", 
                    background = "navy",  width = 8,solidHeader = TRUE, 
                    htmlOutput("motionchart",height = 600)),
                
                ##Control for providing Region
                column(4, box(
                  title = "Input", background = "black", width = 12,
                  selectInput("motionreg","Region",
                              choices=c("Africa","Asia","Australia","Europe","North America","South America"))
                )          
                )
              )
      )
      
    )
  )
)

# ***************************** The End ***********************************************************
