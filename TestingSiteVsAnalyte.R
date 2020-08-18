

#App to look at Analtye Data from external lab?
rm(list=ls())  #clear variables
graphics.off()  #close figs

#Loading Libraries
load.pkg <- function(p) {  #load packages with require(), install any that are not installed
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  suppressMessages(require(p, character.only = TRUE))
}
load.pkg('neonUtilities')  ##Loading Libraries##
#load.pkg('dplyr')
load.pkg('tidyverse')
load.pkg('readr')
load.pkg('tidyr')
load.pkg('plotly')
load.pkg('shiny')
load.pkg('shinycssloaders')
load.pkg('RColorBrewer')

##List of analytes that will be evaluated
type.analyte.here<- c('ANC','Br','Ca','Cl','CO3','conductivity','DIC','DOC','F','Fe','HCO3','K','Mg','Mn','Na','NH4 - N','NO2 - N','NO3+NO2 - N','Ortho - P','pH','Si','SO4','TDN','TDP','TDS','TN','TOC','TP','TPC','TPN','TSS','TSS - Dry Mass','UV Absorbance (250 nm)','UV Absorbance (280 nm)')

s=1 ##variable to move through 'site.pick1' below

#List of sites-
site.pick1<- c("SYCA", "ARIK","BARC","BIGC","BLDE","BLUE","BLWA","CARI","COMO","CRAM","CUPE","FLNT","GUIL","HOPB","KING","LECO","LEWI","LIRO","MART","MAYF","MCDI","MCRA","OKSR","POSE","PRIN","PRLA","PRPO","REDB","SUGG","TECR","TOMB","TOOK","WALK","WLOU")
#-----------
      D<- ''
      if (nchar(D) == 0) { D<- getwd() }
      unlink(paste0(D,"/WaterWorld"), recursive = T)
      ifelse(!dir.exists(file.path(D,'WaterWorld')), dir.create(file.path(D,'WaterWorld')), FALSE)
      setwd(file.path(D,'WaterWorld'))
      
      #loadByProduct won't work for large data pull for Water Chemistry-
      #variable for running getPackage()- will run through all dates below
      y2017 <- c("2017-01","2017-02","2017-03","2017-04","2017-05","2017-06","2017-07","2017-08","2017-09","2017-10","2017-11","2017-12")
      y2018 <- c("2018-01","2018-02","2018-03","2018-04","2018-05","2018-06","2018-07","2018-08","2018-09","2018-10","2018-11","2018-12")
      y2019 <- c("2019-01","2019-02","2019-03","2019-04","2019-05","2019-06","2019-07","2019-08","2019-09","2019-10","2019-11","2019-12")
      y2020 <- c("2020-01","2020-02","2020-03","2020-04")
      y<- c(y2017,y2018)
      y2<- c(y,y2019)
      years<- c(y2,y2020)
      
      dpid <- "DP1.20093.001"   #Surface Water Chemistry DPID
      
      #s=23 ##stopped at s=23 because problem with rbind()
      while (s < length(site.pick1)+1) {
        
        if (s == 1) {   ##if first site 'SYCA' will run code
        i=1
        ##will sequence along dates from variable 'years' and download zip files- does it for every site
      result = vector("list", length(years))
      for(i in seq_along(years)){   #sequencing along dates 2017-01 to present for NEON portal data
        result[[i]] = tryCatch(getPackage(dpid, site_code = site.pick1[s] ,year_month = years[i], package = "basic")(years[[i]]), 
                               error = function(e) paste("No data"))
        zipF <- list.files(pattern= site.pick1[s] , full.names = F); sapply(zipF, unzip, exdir = paste0(site.pick1[s],"-unzip"))
      }
      
      #putting unzipped files together by type as list
      DomainLab <- list.files(path= paste0(site.pick1[s],"-unzip"),pattern= "domain")
      ExternalLab <- list.files(path= paste0(site.pick1[s],"-unzip"),pattern= "external")
      FieldData <- list.files(path= paste0(site.pick1[s],"-unzip"),pattern= "fieldData")
      ParentData <- list.files(path= paste0(site.pick1[s],"-unzip"),pattern= "Parent")
      Domain<-list()      #empty lists to store file data below
      Field<- list()
      Parent<- list()
      External<- list()
      
      #Reading all CSV's and saving to vector
      i=1
      result = vector("list", length(years))
      for(i in seq_along(years)) {
        while(i< length(years)+1){
          result[[i]] = tryCatch(External[[i]]<- read_csv(paste0(site.pick1[s],"-unzip/",ExternalLab[i])),
                                 error = function(e) paste("No data"))
          result[[i]] = tryCatch(Domain[[i]]<- read_csv(paste0(site.pick1[s],"-unzip/",DomainLab[i])),
                                 error = function(e) paste("No data"))
          result[[i]] = tryCatch(Field[[i]]<- read_csv(paste0(site.pick1[s],"-unzip/",FieldData[i])),
                                 error = function(e) paste("No data"))
          result[[i]] = tryCatch(Parent[[i]]<- read_csv(paste0(site.pick1[s],"-unzip/",ParentData[i])),
                                 error = function(e) paste("No data"))
          #print(paste("Successfully Read",years[i]))
          i=i+1
        }
      }
      #Merging data by type/origin [ExternalLab/DomainLab/Field/Parent Data]
      External.data<- External[[1]]       #saving data for first bout
      i=2
      while (i < length(External)+1) {    #Merging the rest of data
        External.data<- merge(External.data, External[[i]], all.x="T",all.y = "T")
        i=i+1
      }
      Domain.data<- Domain[[1]]       #saving data for first bout
      i=2
      while (i < length(Domain)+1) {    #Merging the rest of data
        Domain.data<- merge(Domain.data, Domain[[i]], all.x="T",all.y = "T")
        i=i+1
      }
      Field.data<- Field[[1]]       #saving data for first bout
      i=2
      while (i < length(Field)+1) {    #Merging the rest of data
        Field.data<- merge(Field.data, Field[[i]], all.x="T",all.y = "T")
        i=i+1
      }
      Parent.data<- Parent[[1]]       #saving data for first bout
      i=2
      while (i < length(Parent)+1) {    #Merging the rest of data
        Parent.data<- merge(Parent.data, Parent[[i]], all.x="T",all.y = "T")
        i=i+1
      }
      } ##end if statement for 's == 1'
      
      if (s != 1) {   ##if anything but site.pick1[1] 'SYCA' will run code
        #Every other site besides 'SYCA' will go through this code
        i=1
        result = vector("list", length(years))
        for(i in seq_along(years)){   #sequencing along dates 2017-01 to present for NEON portal data
          result[[i]] = tryCatch(getPackage(dpid, site_code = site.pick1[s] ,year_month = years[i], package = "basic")(years[[i]]), 
                                 error = function(e) paste("No data"))
          zipF <- list.files(pattern= site.pick1[s] , full.names = F); sapply(zipF, unzip, exdir = paste0(site.pick1[s],"-unzip"))
        }
        
        #putting unzipped files together by type as list
        DomainLab.1 <- list.files(path= paste0(site.pick1[s],"-unzip"),pattern= "domain")
        ExternalLab.1 <- list.files(path= paste0(site.pick1[s],"-unzip"),pattern= "external")
        FieldData.1 <- list.files(path= paste0(site.pick1[s],"-unzip"),pattern= "fieldData")
        ParentData.1 <- list.files(path= paste0(site.pick1[s],"-unzip"),pattern= "Parent")
        Domain.1<-list()      #empty lists to store file data below
        Field.1<- list()
        Parent.1<- list()
        External.1<- list()
        
        i=1
        result = vector("list", length(years))
        for(i in seq_along(years)) {
          while(i< length(years)+1){
            result[[i]] = tryCatch(External.1[[i]]<- read_csv(paste0(site.pick1[s],"-unzip/",ExternalLab.1[i])),
                                   error = function(e) paste("No data"))
            result[[i]] = tryCatch(Domain.1[[i]]<- read_csv(paste0(site.pick1[s],"-unzip/",DomainLab.1[i])),
                                   error = function(e) paste("No data"))
            result[[i]] = tryCatch(Field.1[[i]]<- read_csv(paste0(site.pick1[s],"-unzip/",FieldData.1[i])),
                                   error = function(e) paste("No data"))
            result[[i]] = tryCatch(Parent.1[[i]]<- read_csv(paste0(site.pick1[s],"-unzip/",ParentData.1[i])),
                                   error = function(e) paste("No data"))
            #print(paste("Successfully Read",years[i]))
            i=i+1
          }
        }
        #Merging data by type/origin [ExternalLab/DomainLab/Field/Parent Data]
        External.data1<- External.1[[1]]       #saving data for first bout
        i=2
        while (i < length(External.1)+1) {    #Merging the rest of data
          External.data1<- merge(External.data1, External.1[[i]], all.x="T",all.y = "T")
          i=i+1
        }
        Domain.data1<- Domain.1[[1]]       #saving data for first bout
        i=2
        while (i < length(Domain.1)+1) {    #Merging the rest of data
          Domain.data1<- merge(Domain.data1, Domain.1[[i]], all.x="T",all.y = "T")
          i=i+1
        }
        Field.data1<- Field.1[[1]]       #saving data for first bout
        i=2
        while (i < length(Field.1)+1) {    #Merging the rest of data
          Field.data1<- merge(Field.data1, Field.1[[i]], all.x="T",all.y = "T")
          i=i+1
        }
        Parent.data1<- Parent.1[[1]]       #saving data for first bout
        i=2
        while (i < length(Parent.1)+1) {    #Merging the rest of data
          Parent.data1<- merge(Parent.data1, Parent.1[[i]], all.x="T",all.y = "T")
          i=i+1
        }

      ##merging all data together for all sites- rbind() does not work for everysite, must use merge() here
      External.data<- merge(External.data,External.data1, all.x="T",all.y = "T")
      Domain.data<- merge(Domain.data,Domain.data1, all.x="T",all.y = "T")
      Field.data<- merge(Field.data,Field.data1, all.x="T",all.y = "T")
      Parent.data<- merge(Parent.data,Parent.data1, all.x="T",all.y = "T")
      } ##end if statement for 's != 1'
        
        s=s+1   ##moves loop to next site
        
    } ##end of while loop for all sites
        
      ##optional statements to dave data
      #setwd(D)
      #write.table(External.data, file = paste0(site.pick1[s],"External_data.csv"), sep = ",")
      #write.table(Parent.data, file = paste0(site.pick1[s],"Parent_data.csv"), sep = ",")
      #write.table(Field.data, file = paste0(site.pick1[s],"Field_data.csv"), sep = ",")
      #write.table(Domain.data, file = paste0(site.pick1[s],"Domain_data.csv"), sep = ",")

      #External ANC data
      analyte_data<- External.data %>%
        select(analyte, analyteConcentration, collectDate, siteID) %>%
        filter(!is.na(analyteConcentration)) %>% 
        arrange(collectDate)
      #View(analyte_data)
      nb.cols <- 34
      mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)
      
      #colourCount = length(unique(analyte_data$analyte))
      #getPalette = colorRampPalette(brewer.pal(9, "Set1"))

      d.plot<- ggplot(analyte_data, aes(collectDate, analyteConcentration, group = (siteID), fill = (siteID)))+
        geom_point(aes(color=siteID), size=.7)+
        geom_line(aes(color=siteID)) +
        facet_wrap(facets = vars(analyte), scales = 'free')+
          scale_alpha_continuous()+
        
        scale_fill_manual(values = mycolors) +
        
        #scale_fill_brewer(values = mycolors)+
          ggtitle("Analyte Comparison by Site")+
          xlab("Collect Date")+
          ylab("Analyte Concentration")    
        
      d.plot<- ggplotly(d.plot, width = 2400, height = 1400)
      
      ggsave('AnalyteCompare_bySite',limitsize = F, device = htm)
      
      htmlwidgets::saveWidget(d.plot, 'AnalyteCompare_bySite')
      
      d.plot
      
      #resetting directiry to original
      setwd(D)
      unlink(paste0(D,"/WaterWorld"), recursive = T)
      

 

