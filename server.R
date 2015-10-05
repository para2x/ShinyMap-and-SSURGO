library(leaflet)
require(soilDB)
library(RCurl)
library(xml2)
library(ggplot2)
library(curl)
library(jsonlite)
#setwd("C:/Users/hamzed/Dropbox/New folder/mine")
  multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}

  soildataret<-function(lat,long){
    
    f <-  paste("&lon=", long, "&lat=", lat,sep = "")
    the.url <- paste("http://casoilresource.lawr.ucdavis.edu/soil_web/api/ssurgo.php?what=mapunit", 
                     f, sep = "")
    try(res <- jsonlite::fromJSON(the.url), silent = TRUE)
    mukey<-res[[4]]
    ######### Reteiv soil 
    headerFields =
      c(Accept = "text/xml",
        Accept = "multipart/*",
        'Content-Type' = "text/xml; charset=utf-8",
        SOAPAction = "http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx/RunQuery")
    
    body = paste('<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
                 <soap:Body>
                 <RunQuery xmlns="http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx">
                 <Query>
                 SELECT muaggatt.mukey, mapunit.mukey, mapunit.muname,muaggatt.aws0100wta,component.*,chorizon.* from mapunit join component on mapunit.mukey=component.mukey
                 join chorizon on component.cokey=chorizon.cokey
                 join muaggatt on mapunit.mukey=muaggatt.mukey
                 where mapunit.mukey=', mukey,';
                 </Query>
                 </RunQuery>
                 </soap:Body>
                 </soap:Envelope>')
    reader = basicTextGatherer()
    out<-curlPerform(url = "http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx",
                     httpheader = headerFields,  postfields = body,
                     writefunction = reader$update
    )
    
    xml_data <-read_xml(reader$value())
    ##### All datatables below newdataset
    tables<-xml_children(xml_find_all(xml_data, ".//NewDataSet"))
    
    ll<-length(xml_text(xml_find_all(tables, ".//compname")))
    
    data<-data.frame(Hname=xml_text(xml_find_all(tables, ".//compname")),
                     Comp_Share=xml_text(xml_find_all(tables, ".//comppct_r")),
                     Depth=xml_text(xml_find_all(tables, ".//hzdept_r")),
                     OM=xml_text(xml_find_all(tables, ".//om_r")),
                     SAND=ifelse(length(xml_text(xml_find_all(tables, ".//sandtotal_r")))==ll,xml_text(xml_find_all(tables, ".//sandtotal_r")),rep(NA,ll)),
                     SILT=ifelse(length(xml_text(xml_find_all(tables, ".//silttotal_r")))==ll,xml_text(xml_find_all(tables, ".//silttotal_r")),rep(NA,ll)),
                     CLAY=ifelse(length(xml_text(xml_find_all(tables, ".//claytotal_r")))==ll,xml_text(xml_find_all(tables, ".//claytotal_r")),rep(NA,ll)),
                     AWC= ifelse(length(xml_text(xml_find_all(tables, ".//awc_r")))==ll,xml_text(xml_find_all(tables, ".//awc_r")),rep(NA,ll))

    )
    
    data[,] <- lapply(data[,], as.character)
    data[,2:4] <- lapply(data[,2:4], as.numeric)
    return(list("data"=data,"Metadat"=list("Name"=unique(xml_text(xml_find_all(tables, ".//muname"))),
                                           "AWC"=unique(xml_text(xml_find_all(tables, ".//aws0100wta"))),
                                           "MapUnit"=mukey,
                                           "Crop.Pro"=unique(xml_text(xml_find_all(tables, ".//cropprodindex")))
                                           )))
  }
  
 function(input, output) { 


    ########################################## 
   ######### Reactive data
   #####################
   values <- reactiveValues()
   observe({
     values$inf <- input$map_click
     if (!is.null(values$inf)) {
       values$infmu<-soildataret(values$inf$lat,values$inf$lng)}
   })
   ##################### Map
   output$map <- renderLeaflet({
     map<-leaflet() %>% setView(lng = -93.5, lat = 42.03601, zoom = 12) %>%
     addProviderTiles("Esri.WorldImagery") %>%
      addProviderTiles("Thunderforest.Transport",
                      options = providerTileOptions(opacity = 0.25))
                         

  return(map)
  })
  ###################### status and info
   output$stateInfo <-renderUI({
     if (is.null(values$inf)) {
       return("")
     } else {
       
       infmu<-values$infmu
      # infmu<-soildataret(values$inf$lat,values$inf$lng)
       HTML(paste("<b>Lat=</b>",round(values$inf$lat,2),
                  "</br> <b>Long=</b>",round(values$inf$lng,2),
                  "</br> <b>MuName=</b>",(infmu["Metadat"]$Metadat$Name),
                  "</br> <b>Mapunit=</b>",(infmu["Metadat"]$Metadat$MapUnit)
                  ))

     }
   })
   ######### Set Plot height
   myheight <- function(){
     if (!is.null(values$inf)) {
     250
       }else{
         0
       }
   }
   ######### Soil Analysis Plot
   output$anaplot<-renderPlot({
     if (!is.null(values$inf)) {
       ##### Subsetting the dataset based on combobox
       data <-values$infmu[[1]]
       tmp.data<-data.frame(Hname=data[,"Hname"],Depth=data[,"Depth"],var=data[,input$field])
       tmp.data[,] <- lapply(tmp.data[,], as.character)
       tmp.data[,2:3] <- lapply(tmp.data[,2:3], as.numeric)
       #####graphing
          p1<- ggplot(tmp.data) +geom_line(aes(x=(var), y=Depth,color=Hname),size=2)+
                   scale_y_reverse()+theme_gray(base_size = 15)
           
           p2<-ggplot(data, aes(x="", y=Comp_Share, fill=Hname))+
             geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
             theme_gray(base_size = 15)
           
           multiplot(p1,p2,cols=2)
           
           output$table <- renderDataTable(data)
       
     }else{
  
     }
   }, height=myheight)
   ####### Download
    datasetdown<-function(){
      if (!is.null(values$inf)) {
        values$infmu[[1]]
      }
    }
    
   output$downloadData <- downloadHandler(
     filename = 
       paste('SoilDB.csv', sep='') ,
     content = function(file) {
       write.csv(datasetdown(), file)
     }
 
   )

  ########### Weather Analysis
   
   
   
  }
