##NEON Mammal History App##
#Will look into NEON database and find tagID if it exists on the selected site in the app
#Will print out what tag selected, history of captures on NEON database, and plot when it
#was caught (collection date) vs where on the plot it was captured (trap coordinate)
#If MAM history and plot has no info = could not find that tagID at selected site on NEON database
#Must enter domain # and tagID in correct format, leading w/ domain followed by tag
#identification number [D##.tagID#] {Ex: 'club foot' at JORN - R2861}

#install.packages('neonUtilities')  ##Loading Libraries##
#load.pkg('dplyr')
#install.packages('tidyverse')
#install.packages('readr')
#install.packages('tidyr')
#install.packages('plotly')
#install.packages('shiny')
#install.packages('shinycssloaders')
#install.packages('reshape')
#install.packages('RColorBrewer')
#install.packages('grid')
#update.packages(ask = FALSE, checkBuilt = TRUE)
#devtools::check()

#setwd('C:/Users/tgilbert/Documents/GitHub/fs-learn-R/2020/TimGilbert/MAM_history')

#a<- .libPaths()
#setwd(a)
#setwd('C:\Users\tgilbert\Documents\R\win-library\3.6')


library(neonUtilities)  ##Loading Libraries##
library(dplyr)
library(tidyverse)
library(readr)
library(tidyr)
library(shiny)
library(shinycssloaders)
library(reshape)
library(RColorBrewer)
library(grid)
library(leaflet)
library(ggmap)
library(sf)
library(mapview)
library(plotly)
#(devtools)
#install.packages('phantomjs.exe')
#install_github("wch/webshot") # first install phantomjs.exe in your directory
library(htmlwidgets)
library(webshot)


##debug_mode=0

#getwd()
#runApp()
#shiny::runApp(system.file('appdir', package='MamHistory'))

#---user interface--------------------------------------------------  


ui <- fluidPage(
  titlePanel("NEON Mammal History App", windowTitle = 'Heyo'),
  sidebarLayout(
    sidebarPanel(
      #selection area for each site- can type in site and app will find from list below
      selectInput("Select", "Please select your site:",
                  choices = c("SRER", "JORN", "BART", "HARV", "BLAN", "SCBI", "SERC", "DSNY", "JERC", "OSBS", "GUAN", "LAJA","STEI", "TREE", "UNDE","KONA", "KONZ", "UKFS", "GRSM", "MLBS", "ORNL","DELA", "LENO", "TALL", "DCFS", "NOGP", "WOOD", "CPER", "RMNP", "STER", "CLBJ", "OAES", "YELL", "MOAB", "NIWO", "JORN", "SRER", "ONAQ", "ABBY", "WREF", "SJER", "SOAP", "TEAK", "BARR", "TOOL", "BONA", "DEJU", "HEAL"), selected = F, multiple = T),
     
      selectInput('view', "Select map view:",
                  choices = c("Esri.WorldImagery","TomTom.Hybrid","Stamen.TopOSMRelief")),
      submitButton("Process Selection", icon('globe-americas'), width = '200px'), 
    #  checkboxInput('yes', 'download map?', FALSE), 
     # conditionalPanel(
      #  condition = "input.yes == true",
      #  downloadButton('map_down')
    #  ),
      #type in tagID looking for and will save as variable
      textInput("SelectID", "Type in tag of individual species:", value= ""),
      
      
      submitButton("Process Tag", icon('tag'), width = '200px'),
    ),
    
    mainPanel("Please wait for app to access NEON data portal",
              tabsetPanel(
                
                tabPanel("Map View of Capture by Species",
                         withSpinner(leafletOutput("map", width = '100%', height = 700))),
                         #downloadButton('downloadmap', label = "Save Map")),
                
                tabPanel("Top Captured Individuals",
                         withSpinner(dataTableOutput("FitSelect"))),
                
                tabPanel("Captures Per Plot",
                         withSpinner(plotlyOutput("DataSet1"))),
                
                tabPanel("MAM Capture Histroy",
                         withSpinner(dataTableOutput("NEONDataSet1"))),
                
                tabPanel("Raw Data",
                         withSpinner(dataTableOutput("data1"))),
                
                tabPanel("Trap Coordinate Heat Map",
                         withSpinner(plotlyOutput("plot2"))),
                
                tabPanel("Experimental Heat Map View",
                         withSpinner(plotlyOutput("plot3")))
              )
    )
  )
)
