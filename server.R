# Author : SeeRs team 
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# Purpose: The purpose of this code is to serve as the backend for the shiny dashboard to render the graphs
# and help to serve the user inputs for ggplot2 and googleVis
#**********************************************************************************************************
server <- function(input, output) {
  
  # Filter function is written inorder to capture the controls in the form of year or region  
  # We need to get the data for the set of years that are passed on from the UI control bar
  # All the Filters are clubbed together for easy access 
  
  # Filter -1 : Filter for the population and co2 emission charts in the Tab - GGplot2-I
  filter_Pop = reactive({
    co2 %>%
      filter(
        Year >= input$yearInpop[1],
        Year <= input$yearInpop[2])
  })
  
  # Filter -2 :filter for the gdp.pp plots 
  filter_GDP = reactive({
    co2 %>%
      filter(
        Year==input$yearInpgdp 
      ) 
  })
  
  # Filter -3 :filter for the motion chart 
  filter_motion = reactive({
    co2 %>%
      filter(
        Region ==input$motionreg
      )
  })
  
  # Filter -4 :filter for the Co2 emission contribution charts 
  filter_contri = reactive({
    co2 %>%
      filter(
        Year==input$yearInpgdp,
        Region == input$region
      ) 
  })
  
  # Filter -5 :filter for the bubble charts 
  filter_bubble = reactive({
    co2 %>%
      filter(
        Year==input$yearInpbub 
      ) 
  })
  
  # Filter -6 :filter for the plot depicting Co2 emission by India 
  filter_Ind = reactive ({
    co2 %>%
    filter(
      Year >= input$co2year #input$year inputs the first input from user
    )
  })
  
  # Filter -7 :filter used for the googleVis charts for the map 
  filtered_map = reactive({
    co2 %>%
      filter(
        Year == input$mapyear
      )
  })
#*******************************************************************************************************  
  
  #########################
  #ggplot() and qplot Code
  #########################
  # Below qplot() command helps to create graph for the per.capita c02 emission from the data co2. The geom helps
  # to create the identification based on the Region in the form of different shapes
  # renderPlot() helps to capture the plot inorder to be returned back to the client.  
  output$pointplot <- renderPlot({
    qplot(Year, Per.capita.CO2.emissions,data = co2, colour = Region , shape = Region,geom = c("point"))
  })  
  
  # The jitter geom command helps to see the points distinctly. It helps when there are more than one point 
  # of the same value. The alpha value of 0.25 ensure that the points are not fully opaque and one can observe the 
  # underlying points too. The colour and legend in the qplot is decided by R and it can also be manually set using
  # scale options. 
  output$jitterplot <- renderPlot({
    qplot(Region, Gdp.Ppp, data = co2, geom = 'jitter',alpha = I(1/4),colour = Region)
  }) 
  
  # The geom_path() command plots the data points and connects them in an order. In this case we plotted  region wise
  # rate of GDP with respect to population as a layer. 
  output$pathplot <- renderPlot({
    ggplot(data = co2, aes(x = Gdp.Ppp/Population, y = Region)) +
      geom_path(aes(color= Year, size = Population)) 
  })
  
  # The below plot is for depicting the density of the Co2 emission by India over the years 
  # qplot plots Co2 emission which is first input of dataset defined which is the subset of india
  # geom defines what type of output it should be, in this case it is density plot
  #xlab defines x label nd ylab as y label, alpha is for the opacity of points    
  output$plotIndia <- renderPlot({ 
    qplot(CO2.Emissions, data=subset(filter_Ind(),Nation =='INDIA'), geom="density",fill="red", alpha=I(.5), 
          xlab="Co2 Emission",ylab="Density")
  })
  
  # In order to capture the population region wise for a particular timeframe we can use the geom_bar chart 
  # The filter_pop() will filter the data based on the year which comes as an input from the UI
  # The aesthetic inputs of x is given as year and y as the population growth. The color is filled based on the region
  # the stat = identity will help to keep the scale according to the population. The year is set discrete by the
  # scale command
  output$barplot <- renderPlot({
  # grouping the data region and year wise so that the plot has combined population for region a particular year  
    pop_region = group_by(filter_Pop(),Region,Year)             
  # mean of the population grouped by Region. The sum of population is not considered because the default integer
  # package cannot handle the data especially for Asia. The enhanced bit64() package is another option 
  # but we wanted to minimize the packages loaded for the project
    pop_input = summarise(pop_region, meanpop = mean(Population)) 
    ggplot(pop_input, aes(x = factor(Year), y = meanpop, fill=Region)) + 
      geom_bar(stat = "identity") +
      scale_x_discrete("Year")
  })
  
  # The below ggplots uses the geom-point which is the dot visualization
  # geomtext() helps put labels for each of the points what it represents. It is crude version of hover
  output$plotworld <- renderPlot({
    ggplot(data=filter_Pop(), aes(x=Gdp.Ppp,y=CO2.Emissions,col=Region,size=Population,label=Nation)) +
      geom_point()+geom_text(aes(label=Nation2),hjust=0, vjust=0)
  
  })

  # The below code is to plot a Bar Chart to plot the gdp with respect to the region 
  # The filtered data will be used to plot the information year-wise 
  # geom_bar() helps to create the plot. ggplot2 sets the colors to the regions on its own
  # stat option helps to set the scales, labs() helps the assign the label names 
  output$barchart <- renderPlot({
    ggplot( filter_GDP(), aes( x= Region, y=Gdp.Ppp))+
      geom_bar(stat= "identity", aes(fill = Region)) +
      labs(x = "Region", y = "GDP per Capita")
  })
  
  #####################
  #Bar Chart Code
  #####################
  # Bar Chart to give visualization for all contributors of CO2 emissions
  # This Chart gives visualization for the mean of all CO2 contributors along with Total CO2 emission
  # Region wise at any particular year from 1912 to 2011
  output$Emisplot <- renderPlot({
  # filter_contri() gives Region wise data at a particular year
    filterco2  = filter_contri()
    
  # We concentrate only on emission field so we subset our dataframe
    co2_new = filterco2[, c("Region", "Year", "CO2.Emissions", 
                          "Emissions.from.bunker.fuels",
                          "Emissions.from.cement.production" ,                          
                          "Emissions.from.gas.flaring",
                          "Emissions.from.gas.fuel.consumption" ,                       
                          "Emissions.from.solid.fuel.consumption",                    
                          "Emissions.from.liquid.fuel.consumption")]
    
  # Rename the column names for clear visualization
    names(co2_new) = c("Region"= "Region","Year"= "Year","CO2.Emissions" = "Net.CO2.Emissions", "Emissions.from.bunker.fuels" = "CO2.from.Bunker.Fuel",
                       "Emissions.from.cement.production" ="CO2.from.Cement.Prod", "Emissions.from.gas.flaring" ="CO2.from.Gas.Flaring" ,
                       "Emissions.from.gas.fuel.consumption"="CO2.from.Gas.Fuel.Consm", "Emissions.from.solid.fuel.consumption"="CO2.from.Solid.Fuel.Consm",
                       "Emissions.from.liquid.fuel.consumption" ="CO2.from.Liquid.Fuel.Consm")
    
  # Use melt() from reshape2() package to change the individual emission columns into two columns
  # one for type and other for value . This is getting the data ready for plotting
    melted  = melt(co2_new, id= c("Region", "Year"), value.name = "Emission")
    
  # Now group the data as per Region and the type of emission
    group_meltco2 = group_by(melted,Region,variable)
    
  # Calculate the mean of the emission for each type of emission
    outco2 = summarise(group_meltco2,mean_Emis = mean(Emission))
    
  # Plot this data using ggplot and geom_bar() layer. reorder() option will display the data on the graph 
  # in ascending order for clear visualization. The ordering is done based on the mean values 
    ggplot(outco2, aes(reorder(variable,mean_Emis),mean_Emis)) +
      geom_bar(aes(col = Region ),stat="identity") +
      labs(x = "Contributors", y = "Mean CO2 Emission")
    
  }) 
  
  #####################
  #ComboChart Code
  #####################
  # Plotting of the combochart using gvisComboChart()
  # It helps to plot two different types of data using two different types of graphs 
  output$combochart<-renderGvis({
    gr_temp = group_by(filter_GDP(), Region) 
  # creating the data based on the summarised outbased based on the mean of population and Gdp.ppp  
    output_pop = summarise( gr_temp, mean_pop = mean(Population), mean_gdp = mean(Gdp.Ppp ))
  # Combined graph to be returned to the UI. There are various options for combochart which are combined into
  # different subgroups of titleTextStyle : for the graph text, Curvetype : for the plot type 
  # series : for setting the data information on the axis, vaxes : gives option for setting of the vertical
  # axes and haxes : gives option for setting the horizontal axes. 
  # all the options are given in a form of a list to the combochart and help in customizations 
    combch    =  gvisComboChart(output_pop, xvar="Region", yvar=c("mean_pop", "mean_gdp"),
                          options=list(title="Population vs GDP",
                                       titleTextStyle="{color:'black',
                                       fontName:'Courier',
                                       fontSize:16}",
                                       curveType="function", 
                                       pointSize=3,
                                       seriesType="bars",
                                       series="[{type:'line', 
                                       targetAxisIndex:0,
                                       color:'black'}, 
                                       {type:'bars', 
                                       targetAxisIndex:1,
                                       color:'blue'}]",
                                       vAxes="[{title:'POPULATION',
                                       format:'short',
                                       titleTextStyle: {fontName:'Times-Roman',fontSize:18,Italics:'False',color: 'black'},
                                       textStyle:{color: 'black'},
                                       textPosition: 'out'}, 
                                       {title:'GDP',
                                       format:'short',
                                       titleTextStyle: {fontName:'Times-Roman',fontSize:18,Italics:'False',color: 'black'},
                                       textStyle:{color: 'grey'},
                                       textPosition: 'out',
                                       minValue:0}]",
                                       hAxes="[{title:'Region',
                                       textPosition: 'out'}]",
                                       width=500, height=400)
                          )
  # Return the plot to the UI. This is rendering of the graph 
    return(combch) 
  })
  
  #####################
  #World Map Code
  #####################
  # based on the year input filter out the whole dataset to just show the relevant year
  # this is reactivity at display. everytime the inpur changes this gets called and 
  # adjusts the dataset on display and then call all other plots that depends on the 
  # filtered_map
  
  # Produce the world map using googleVis
  # One of the benefits of the googleVis over leaflet is that it can automaticaly detect the nations and plot them
  # while leaflet requites to specify the coordinates of a nation's borders ( which can be quite tedious)
  #   gvisGeoChart has following syntax:
  #   gvisGeoChart(data, locationvar = "", colorvar = "", sizevar = "", hovervar = "", options = list(), chartid)
  #         location var can be either markers or region names
  #         colorvar determins the scale of the gradient used to color regions
  #         options is used to control the look  and feel of the whole htmloutput and can be customized using javascript
  output$co2map <- renderGvis({
    map = gvisGeoChart(filtered_map(), locationvar="Nation2", colorvar="CO2.Emissions", 
                       options = list(width=500, height=300))  
    return(map)
  })
  output$gdpmap <- renderGvis({
    map = gvisGeoChart(filtered_map(), locationvar="Nation2", colorvar="Gdp.Ppp", 
                       options = list(width=500, height=300) ) 
    return(map)
  })
  output$popmap <- renderGvis({
    map = gvisGeoChart(filtered_map(), locationvar="Nation2", colorvar="Population", 
                       options = list(width=500, height=300) )  
    return(map)
  })
  
  #####################
  #Bubble Plot Code
  #####################
  # Below is the code for creating Bubble Plot. The function gvisBubbleChart() is used 
  # The bubble plot is created for the C02.Emissions with respect to the Gdp.Ppp. The color of the bubbles 
  # is based on the Region. The size of the bubble is decided by the population
  # options in the functions is given in a form of list and helps to control the look of the graph along 
  # with the labeling. The various options have been used to show the flexibility of the function 
  output$bubbleplot <- renderGvis({
    bubblepl= gvisBubbleChart(filter_bubble(), idvar="Nation2",xvar="Gdp.Ppp", yvar="CO2.Emissions",
                              colorvar="Region", sizevar="Population",options=list(
                                top = '50', left ='75',
                                height = "600", width = "700",
                                hAxis="{format:'short',title:'GDP'}",vAxis="{format:'short',title:'Co2 Emissions'}"
                              )
    )
  # Renders the graph back to the UI to be displayed 
    return(bubblepl)
  })
  
  #####################
  #Motion Chart Code
  #####################
  # For Region wise visualization of all data present in the dataset we have used Motion Chart
  # Takes Region wise filtered data as input to give dynamic visualization for all variables available
  # in the dataframe in form of bubbleplot, bar charts and line charts.
  # one of the easiest to implement and one of the best features provided by googleVis.
  # it picks up all the data in the dataframe based on the input given as filter_motion() and 
  # creates all the possible combination of charts 
  output$motionchart <- renderGvis({
    motionch= gvisMotionChart(filter_motion(),idvar="Nation2",timevar="Year")
    return(motionch)
  })
  
}
#******************************** The END ***************************************************************