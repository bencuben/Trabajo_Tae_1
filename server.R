library(shiny)
library(rgdal)
library(leaflet)
library(raster)
library(rgdal)
library(car)
library(lubridate)

shinyServer(function(input, output) {
  
  ###Zona de definiciones###
  
  withProgress(message="Cargando base de datos", value = 0, {
    # Lectura base de datos
    accidentalidad.15 <- shapefile("Accidentalidad_2015/Accidentalidad_2015.shp",encoding="UTF-8",use_iconv=TRUE)
    incProgress(25)
    accidentalidad.16 <- shapefile("Accidentalidad_2016/Accidentalidad_2016.shp",encoding="UTF-8",use_iconv=TRUE)
    incProgress(25)
    accidentalidad.17 <- shapefile("Accidentalidad_2017/Accidentalidad_2017.shp",encoding="UTF-8",use_iconv=TRUE)
    incProgress(25)
    #levels(accidentalidad.15@data$clase)
    
    
    accidentalidad.15@data$CLASE<-as.factor(accidentalidad.15@data$CLASE) # La convertimos en factor
    
    # Recodificamos los niveles de la clase de accidente 2015
    
    accidentalidad.15@data$CLASE<- Recode(accidentalidad.15@data$CLASE,'"Atropello"="Atropello";"Caída Ocupante"="Caída de Ocupante";"Caida Ocupante"="Caída de Ocupante";
                                          "Choque"="Choque";"Incendio"="Incendio";
                                          "Otro"="Otro";
                                          "Volcamiento"="Volcamiento";
                                          "Choque y Atropello"="Choque y Atropello"',as.factor.result=T)#Verificamos que sea factor
    
    #------------------------------------------------------------------------------------------------------------
    # Recodificación de la variable clase 2016
    
    accidentalidad.16@data$CLASE<-as.factor(accidentalidad.16@data$CLASE)
    
    accidentalidad.16@data<-na.omit(accidentalidad.16@data)
    
    accidentalidad.16@data$CLASE<- Recode(accidentalidad.16@data$CLASE,'"Atropello"="Atropello";"Caída de Ocupante"="Caída Ocupante";"Caida Ocupante"="Caída Ocupante";"Choque"="Choque";
                                          "Choque "="Choque";"Incendio"="Incendio";
                                          "Otro"="Otro";
                                          "Volcamiento"="Volcamiento"',as.factor.result=T)

#------------------------------------------------------------------------------------------------------------
# Recodificación de la variable clase 2017


  accidentalidad.17@data$CLASE<-as.factor(accidentalidad.17@data$CLASE)
  
  accidentalidad.17@data$CLASE<- Recode(accidentalidad.17@data$CLASE,'"Atropello"="Atropello";"Caída Ocupante"="Caída Ocupante";"Caida Ocupante"="Caída Ocupante";"Choque"="Choque";
                                        "Choque "="Choque";"Incendio"="Incendio";
                                        "Otro"="Otro";
                                        "Volcamiento"="Volcamiento";
                                        "Choque y Atropello"="C y A"',as.factor.result=T)

### recodificacion de las horas

  accidentalidad.15@data$HORA<-parse_date_time(accidentalidad.15@data$HORA, '%I:%M %p')
  accidentalidad.16@data$HORA<-parse_date_time(accidentalidad.16@data$HORA, '%I:%M %p')
  accidentalidad.17@data$HORA<-parse_date_time(accidentalidad.17@data$HORA, '%I:%M %p')
  accidentalidad <- accidentalidad.15
  incProgress(25)
})
  
  ### FIN Zona de definiciones###
  
  cargarBaseDeDatos <- reactive({
    switch(input$year,
           "2015" = accidentalidad.15,
           "2016" = accidentalidad.16,
           "2017" = accidentalidad.17
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
      selectInput("nombreZona", "Zonas",
                  choices = c(unique(accidentalidad@data$COMUNA))
      )
    }
  })
  
  output$tipoAccidente <- renderUI({
    
    accidentalidad <- cargarBaseDeDatos()

    if(input$filtro == 'Tipo de Accidente'){
      selectInput("nombreAccidente", "Accidentes",
                  choices = c(as.character(unique(accidentalidad@data$CLASE)))
      )
    }
  })
  
  output$map <- renderLeaflet({
    
    accidentalidad <- cargarBaseDeDatos()
    
    switch(input$filtro,
           "Zona" = {if(is.null(input$nombreZona)){
             
                    }else if(input$nombreZona == "NA"){
                      accidentalidad <- subset(accidentalidad, is.na(accidentalidad@data$COMUNA))
                    }else if(input$nombreZona == "0"){
                      accidentalidad <- subset(accidentalidad, accidentalidad@data$COMUNA == 0)
                    }else{
                        accidentalidad <- subset(accidentalidad, accidentalidad@data$COMUNA == input$nombreZona)
                    }
                    select <- input$nombreZona},
           "Tipo de Accidente" = {if(is.null(input$nombreAccidente)){
             
                                  }else if(input$nombreAccidente == "NA"){
                                    accidentalidad <- subset(accidentalidad, is.na(accidentalidad@data$COMUNA))
                                  }else {
                                    accidentalidad <- subset(accidentalidad, accidentalidad@data$CLASE == input$nombreAccidente)
                                    
                                  }
                                  select <- input$nombreAccidente}
    )
    
    if((input$filtro == 'Zona' || input$filtro == 'Tipo de Accidente') && !is.null(select)){
      popup<-paste(accidentalidad@data$BARRIO)
       
      m<-leaflet()
      m<-fitBounds(m,
                   lng1=min(accidentalidad@coords[,1]),
                   lat1=min(accidentalidad@coords[,2]),
                   lng2=max(accidentalidad@coords[,1]),
                   lat2=max(accidentalidad@coords[,2])
                  )
      m<-addProviderTiles(m,provider="OpenStreetMap.Mapnik")
      m<-addCircleMarkers(m,
                           lng = accidentalidad@coords[,1],
                           lat = accidentalidad@coords[,2],
                           popup = popup,
                           radius = 2,
                           stroke = FALSE,
                           fillOpacity = 0.75
       )
      m <- setView(m, mean(accidentalidad@coords[,1]), mean(accidentalidad@coords[,2]), zoom = 14)
      m
    }
  })
})