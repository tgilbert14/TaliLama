
#--Server----------------------------------------------------------------

server <- function(input, output, session) {
  
  site_select1 <- reactive({
    site.pick1<- input$Select1 #saving site selection
    req(input$Select1)
    
    D<- ''
    if (nchar(D) == 0) { D<- getwd() }
    unlink(paste0(D,"/WaterWorld"), recursive = T)
    ifelse(!dir.exists(file.path(D,'WaterWorld')), dir.create(file.path(D,'WaterWorld')), FALSE)
    setwd(file.path(D,'WaterWorld'))
    
    y2017 <- c("2017-01","2017-02","2017-03","2017-04","2017-05","2017-06","2017-07","2017-08","2017-09","2017-10","2017-11","2017-12")
    y2018 <- c("2018-01","2018-02","2018-03","2018-04","2018-05","2018-06","2018-07","2018-08","2018-09","2018-10","2018-11","2018-12")
    y2019 <- c("2019-01","2019-02","2019-03","2019-04","2019-05","2019-06","2019-07","2019-08","2019-09","2019-10","2019-11","2019-12")
    y2020 <- c("2020-01","2020-02","2020-03","2020-04")
    y<- c(y2017,y2018)
    y2<- c(y,y2019)
    years<- c(y2,y2020)
    dpid <- "DP1.20093.001"   #Surface Water Chemistry DPID
    
    i=1
    result = vector("list", length(years))
    for(i in seq_along(years)){   #sequencing along dates 2017-01 to present for NEON portal data
      result[[i]] = tryCatch(getPackage(dpid, site_code = site.pick1 ,year_month = years[i], package = "basic")(years[[i]]), 
                             error = function(e) paste("No data"))
      zipF <- list.files(pattern=site.pick1, full.names = T); sapply(zipF, unzip, exdir = paste0(site.pick1,"-unzip"))
    }
    
    #putting unzipped files together by type as list
    DomainLab <- list.files(path= paste0(site.pick1,"-unzip"),pattern= "domain")
    ExternalLab <- list.files(path= paste0(site.pick1,"-unzip"),pattern= "external")
    FieldData <- list.files(path= paste0(site.pick1,"-unzip"),pattern= "fieldData")
    ParentData <- list.files(path= paste0(site.pick1,"-unzip"),pattern= "Parent")
    
    Domain<-list()      #empty lists to store file data below
    Field<- list()
    Parent<- list()
    External<- list()
    #Reading all CSV's and saving to vector
    i=1
    result = vector("list", length(years))
    for(i in seq_along(years)) {
      while(i< length(years)+1){
        result[[i]] = tryCatch(External[[i]]<- read_csv(paste0(site.pick1,"-unzip/",ExternalLab[i])),
                               error = function(e) paste("No data"))
        result[[i]] = tryCatch(Domain[[i]]<- read_csv(paste0(site.pick1,"-unzip/",DomainLab[i])),
                               error = function(e) paste("No data"))
        result[[i]] = tryCatch(Field[[i]]<- read_csv(paste0(site.pick1,"-unzip/",FieldData[i])),
                               error = function(e) paste("No data"))
        result[[i]] = tryCatch(Parent[[i]]<- read_csv(paste0(site.pick1,"-unzip/",ParentData[i])),
                               error = function(e) paste("No data"))
        #print(paste("Successfully Read",years[i]))
        i=i+1
      }
    }
    #Merging data by type/origin [ExternalLab/DomainLab/Field/Parent Data]
    External.data1<- External[[1]]       #saving data for first bout
    i=2
    while (i < length(External)+1) {    #Merging the rest of data
      External.data1<- merge(External.data1, External[[i]], all.x="T",all.y = "T")
      i=i+1
    }
    Domain.data1<- Domain[[1]]       #saving data for first bout
    i=2
    while (i < length(Domain)+1) {    #Merging the rest of data
      Domain.data1<- merge(Domain.data1, Domain[[i]], all.x="T",all.y = "T")
      i=i+1
    }
    Field.data1<- Field[[1]]       #saving data for first bout
    i=2
    while (i < length(Field)+1) {    #Merging the rest of data
      Field.data1<- merge(Field.data1, Field[[i]], all.x="T",all.y = "T")
      i=i+1
    }
    Parent.data1<- Parent[[1]]       #saving data for first bout
    i=2
    while (i < length(Parent)+1) {    #Merging the rest of data
      Parent.data1<- merge(Parent.data1, Parent[[i]], all.x="T",all.y = "T")
      i=i+1
    }
    
    write.table(Parent.data1, file = paste0(site.pick1,"Parent_data.csv"), sep = ",")
    write.table(Field.data1, file = paste0(site.pick1,"Field_data.csv"), sep = ",")
    write.table(Domain.data1, file = paste0(site.pick1,"Domain_data.csv"), sep = ",")
    ###done with first site###
    
    setwd(D)
    External.data1
  })
  
  ##Analyte selection
  Analyte_select <- reactive({
    select.analyte<- input$Select_A #saving site selection
    req(input$Select_A)
    
  })
  
  #site_select2<- 'SYCA'
  site_select2 <- reactive({
    site.pick2<- input$Select2 #saving site selection
    req(input$Select2)
    
    D<- ''
    if (nchar(D) == 0) { D<- getwd() }
    unlink(paste0(D,"/WaterWorld"), recursive = T)
    ifelse(!dir.exists(file.path(D,'WaterWorld')), dir.create(file.path(D,'WaterWorld')), FALSE)
    setwd(file.path(D,'WaterWorld'))
    
    y2017 <- c("2017-01","2017-02","2017-03","2017-04","2017-05","2017-06","2017-07","2017-08","2017-09","2017-10","2017-11","2017-12")
    y2018 <- c("2018-01","2018-02","2018-03","2018-04","2018-05","2018-06","2018-07","2018-08","2018-09","2018-10","2018-11","2018-12")
    y2019 <- c("2019-01","2019-02","2019-03","2019-04","2019-05","2019-06","2019-07","2019-08","2019-09","2019-10","2019-11","2019-12")
    y2020 <- c("2020-01","2020-02","2020-03","2020-04")
    y<- c(y2017,y2018)
    y2<- c(y,y2019)
    years<- c(y2,y2020)
    dpid <- "DP1.20093.001"   #Surface Water Chemistry DPID
    
    i=1
    result = vector("list", length(years))
    for(i in seq_along(years)){   #sequencing along dates 2017-01 to present for NEON portal data
      result[[i]] = tryCatch(getPackage(dpid, site_code = site.pick2 ,year_month = years[i], package = "basic")(years[[i]]), 
                             error = function(e) paste("No data"))
      zipF <- list.files(pattern=site.pick2, full.names = T); sapply(zipF, unzip, exdir = paste0(site.pick2,"-unzip"))
    }
    
    #putting unzipped files together by type as list
    DomainLab <- list.files(path= paste0(site.pick2,"-unzip"),pattern= "domain")
    ExternalLab <- list.files(path= paste0(site.pick2,"-unzip"),pattern= "external")
    FieldData <- list.files(path= paste0(site.pick2,"-unzip"),pattern= "fieldData")
    ParentData <- list.files(path= paste0(site.pick2,"-unzip"),pattern= "Parent")
    
    Domain<-list()      #empty lists to store file data below
    Field<- list()
    Parent<- list()
    External<- list()
    #Reading all CSV's and saving to vector
    i=1
    result = vector("list", length(years))
    for(i in seq_along(years)) {
      while(i< length(years)+1){
        result[[i]] = tryCatch(External[[i]]<- read_csv(paste0(site.pick2,"-unzip/",ExternalLab[i])),
                               error = function(e) paste("No data"))
        result[[i]] = tryCatch(Domain[[i]]<- read_csv(paste0(site.pick2,"-unzip/",DomainLab[i])),
                               error = function(e) paste("No data"))
        result[[i]] = tryCatch(Field[[i]]<- read_csv(paste0(site.pick2,"-unzip/",FieldData[i])),
                               error = function(e) paste("No data"))
        result[[i]] = tryCatch(Parent[[i]]<- read_csv(paste0(site.pick2,"-unzip/",ParentData[i])),
                               error = function(e) paste("No data"))
        #print(paste("Successfully Read",years[i]))
        i=i+1
      }
    }
    #Merging data by type/origin [ExternalLab/DomainLab/Field/Parent Data]
    External.data2<- External[[1]]       #saving data for first bout
    i=2
    while (i < length(External)+1) {    #Merging the rest of data
      External.data2<- merge(External.data2, External[[i]], all.x="T",all.y = "T")
      i=i+1
    }
    Domain.data2<- Domain[[1]]       #saving data for first bout
    i=2
    while (i < length(Domain)+1) {    #Merging the rest of data
      Domain.data2<- merge(Domain.data2, Domain[[i]], all.x="T",all.y = "T")
      i=i+1
    }
    Field.data2<- Field[[1]]       #saving data for first bout
    i=2
    while (i < length(Field)+1) {    #Merging the rest of data
      Field.data2<- merge(Field.data2, Field[[i]], all.x="T",all.y = "T")
      i=i+1
    }
    Parent.data2<- Parent[[1]]       #saving data for first bout
    i=2
    while (i < length(Parent)+1) {    #Merging the rest of data
      Parent.data2<- merge(Parent.data2, Parent[[i]], all.x="T",all.y = "T")
      i=i+1
    }
    
    write.table(Parent.data2, file = paste0(site.pick2,"Parent_data.csv"), sep = ",")
    write.table(Field.data2, file = paste0(site.pick2,"Field_data.csv"), sep = ",")
    write.table(Domain.data2, file = paste0(site.pick2,"Domain_data.csv"), sep = ",")
    ###done with second site###
    setwd(D)
    External.data2
  })
  #---------------------------------------------------------------------
  
  output$Elab <- renderPlotly({
    
    External.data1 <- site_select1()     #pulling from NEON site selection
    External.data2 <- site_select2()
    select.analyte <- Analyte_select()
    site.pick1<- site_select1()
    site.pick2<- site_select2()
    ####  select.analyte<- 'ANC'
    analyte_data1<- External.data1 %>%
      filter(analyte == select.analyte) %>%
      filter(!is.na(analyteConcentration)) %>% 
      select(analyte, analyteConcentration, analyteUnits, collectDate) %>% 
      arrange(collectDate)
    
    #colourCount = length(unique(2))
    #getPalette = colorRampPalette(brewer.pal(9, "Set3"))
    
    #d.plot<- d.plot %>% add_markers(data=analyte_data1, type='scatter', mode='markers+lines', x=~collectDate, y=~analyteConcentration, name=~site.pick1, color=I("yellow"))
    d.plot<- plot_ly(data=analyte_data1,
                     type='scatter',
                     mode='markers+lines',
                     x=~collectDate,
                     y=~analyteConcentration,
                     name =~site.pick1,
                     color=I("yellow"),
                     alpha = .9,
                     #create custom hovertext
                     text=~paste0("Analyte: ",analyte, '\n',"Analyte Concentration: ",analyteConcentration,' ',analyteUnits, '\n'," Collect Date: ",collectDate, '\n', "From Extrernal Lab Analysis"), 
                     hoverinfo='text') %>%
      layout(title=paste0(select.analyte,' Analyte Concentrations'),yaxis=list(title='Concentration'),xaxis=list(title='Collect Date'))
    
    #------------------------------------------
    analyte_data2<- External.data2 %>%
      filter(analyte == select.analyte) %>%
      filter(!is.na(analyteConcentration)) %>% 
      select(analyte, analyteConcentration, analyteUnits, collectDate) %>% 
      arrange(collectDate)
    
    d.plot<- d.plot %>% add_markers(data=analyte_data2, type='scatter', mode='markers+lines', x=~collectDate, y=~analyteConcentration, name =~site_select2, color=I("blue"), alpha = .9, text=~paste0("Analyte: ",analyte, '\n',"Analyte Concentration: ",analyteConcentration,' ',analyteUnits, '\n'," Collect Date: ",collectDate, '\n', "From Extrernal Lab Analysis"),hoverinfo='text')
    
    
    D<- ''
    if (nchar(D) == 0) { D<- getwd() }
    unlink(paste0(D,"/WaterWorld"), recursive = T)
    
    d.plot
  })
  
  
}