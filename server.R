library(shiny)
library(rgdal)
library(leaflet)
library(raster)


shinyServer(function(input, output) {
  
  ###Zona de definiciones###
  
  withProgress(message="Cargando base de datos", value = 0, {
    accidentalidad2015 <- shapefile("Accidentalidad_2015/Accidentalidad_2015.shp",encoding="UTF-8",use_iconv=TRUE)
    incProgress(25)
    accidentalidad2016 <- shapefile("Accidentalidad_2016/Accidentalidad_2016.shp",encoding="UTF-8",use_iconv=TRUE)
    incProgress(25)
    accidentalidad2017 <- shapefile("Accidentalidad_2017/Accidentalidad_2017.shp",encoding="UTF-8",use_iconv=TRUE)
    incProgress(25)
    accidentalidad <-accidentalidad2015
    incProgress(25)
  })
  
  ### FIN Zona de definiciones###
  
  cargarBaseDeDatos <- reactive({
    withProgress
    switch(input$year,
           "2015" = accidentalidad2015,
           "2016" = accidentalidad2016,
           "2017" = accidentalidad2017
    )  
  })
  
  #cargarBaseDeDatos()
  
  limpiezaHora <- function(x){
    
    franja <- substr(x, 7,8)
    hora <- as.integer(substr(x, 0, 2))
    minutos <- as.integer(substr(x, 4, 5))
    
    if(franja =='AM' && hora!= 12){
      x <- hora + (minutos/60)
    }else if(franja =='PM' && hora!= 12){
      x <- (hora+12) + (minutos/60)
    }else if(franja =='AM'){
      x <- (minutos/60)
    }else{
      x <- 12 + (minutos/60)
    }
    
    x <- as.double(x)
    return(x)
  }
  
  output$zonas <- renderUI({
    
    accidentalidad <- cargarBaseDeDatos()
    
    
    # if (is.null(input$filtro)){
    #   return()  
    #}
    
    if(input$filtro == 'Zona'){
      selectInput("x1", "Zonas",
                  choices = c(unique(accidentalidad@data$COMUNA))
      )
    }
  })
  
  output$tipoAccidente <- renderUI({
    
    accidentalidad <- cargarBaseDeDatos()
    # if (is.null(input$filtro)){
    #   return()  
    # }
    
    if(input$filtro == 'Tipo de Accidente'){
      selectInput("x2", "Accidentes",
                  choices = c(unique(accidentalidad@data$CLASE))
      )
    }
  })
  
  output$map <- renderLeaflet({
    
    pal <-colorFactor(palette=rainbow(8),levels=unique(accidentalidad@data$CLASE),ordered=F)
    popup<-paste(accidentalidad@data$CLASE,accidentalidad@data$BARRIO,sep="<br/>")
    m<-leaflet()
    m<-fitBounds(m, lng1=min(accidentalidad@coords[,1]),
                  lat1=min(accidentalidad@coords[,2]),
                  lng2=max(accidentalidad@coords[,1]),
                  lat2=max(accidentalidad@coords[,2]))
    m<-addProviderTiles(m,provider="OpenStreetMap.Mapnik")
    # m<-addCircleMarkers(m,
    #                     lng = accidentalidad@coords[,1],
    #                     lat = accidentalidad@coords[,2],
    #                     popup = popup,
    #                     radius = 2,
    #                     stroke = FALSE,
    #                     color=pal(accidentalidad@data$CLASE),
    #                     fillOpacity = 0.75
    # )
    # m<-addLegend(m,"topright",pal=pal,values=accidentalidad@data$CLASE,
    #             title="Tipo de accidente",
    #            labels = accidentalidad@data$CLASE,opacity = 1)
  })
})