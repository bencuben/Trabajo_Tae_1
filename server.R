library(shiny)
library(rgdal)
library(leaflet)
library(raster)
library(rgdal)
library(car)
library(lubridate)

shinyServer(function(input, output) {
  
  ###Zona de definiciones###
  
  # Mientras se cargan las bases de datos se muestran barra de progreso de carga
  withProgress(message="Cargando base de datos", value = 0, {
    # Lectura base de datos
    accidentalidad.15 <- shapefile("Accidentalidad_2015/Accidentalidad_2015.shp",encoding="UTF-8",use_iconv=TRUE)
    incProgress(25)
    accidentalidad.16 <- shapefile("Accidentalidad_2016/Accidentalidad_2016.shp",encoding="UTF-8",use_iconv=TRUE)
    incProgress(25)
    accidentalidad.17 <- shapefile("Accidentalidad_2017/Accidentalidad_2017.shp",encoding="UTF-8",use_iconv=TRUE)
    incProgress(25)

  ### recodificacion de las horas
  accidentalidad.15@data$HORA<-parse_date_time(accidentalidad.15@data$HORA, '%I:%M %p')
  accidentalidad.16@data$HORA<-parse_date_time(accidentalidad.16@data$HORA, '%I:%M %p')
  accidentalidad.17@data$HORA<-parse_date_time(accidentalidad.17@data$HORA, '%I:%M %p')
  incProgress(25)
})
  
  # Función para recargar Bases de datos de acuerdo al año 
  cargarBaseDeDatos <- reactive({
    switch(input$year,
           "2015" = accidentalidad.15,
           "2016" = accidentalidad.16,
           "2017" = accidentalidad.17
    )  
  })

  ### FIN Zona de definiciones###
  
  # Carga de nombres de las Zonas 
  output$zonas <- renderUI({
    
    # Recarga de base de datos
    accidentalidad <- cargarBaseDeDatos()
    
    # Se muestran los nombres de las comunas cuando se eligen en el filtro "Zona"
    if(input$filtro == 'Zona'){
      selectInput("nombreZona", "Zonas",
                  choices = c(unique(accidentalidad@data$COMUNA))
      )
    }
  })
  
  # Carga de nombres de acidentes 
  output$tipoAccidente <- renderUI({
    
    # Recarga de base de datos
    accidentalidad <- cargarBaseDeDatos()

    # Se muestran los nombres de los accidentes cuando se eligen en el filtro "Accidente"
    if(input$filtro == 'Tipo de Accidente'){
      selectInput("nombreAccidente", "Accidentes",
                  choices = c(as.character(unique(accidentalidad@data$CLASE)))
      )
    }
  })
  
  # Carga del mapa
  output$map <- renderLeaflet({
    
    # Recarga de base de datos
    accidentalidad <- cargarBaseDeDatos()
    
    # Se hace una busqueda en la Base de datos según los filtros escogidos
    switch(input$filtro,
           "Zona" = {if(is.null(input$nombreZona)){ #Al inicio cuando se carga la página el input es nulo
             
                    }else if(input$nombreZona == "NA"){ 
                      accidentalidad <- subset(accidentalidad, is.na(accidentalidad@data$COMUNA))
                    }else if(input$nombreZona == "0"){
                      accidentalidad <- subset(accidentalidad, accidentalidad@data$COMUNA == 0)
                    }else{
                        accidentalidad <- subset(accidentalidad, accidentalidad@data$COMUNA == input$nombreZona)
                    }
                    select <- input$nombreZona},
           "Tipo de Accidente" = {if(is.null(input$nombreAccidente)){ #Al inicio cuando se carga la página el input es nulo
             
                                  }else if(input$nombreAccidente == "NA"){
                                    accidentalidad <- subset(accidentalidad, is.na(accidentalidad@data$COMUNA))
                                  }else {
                                    accidentalidad <- subset(accidentalidad, accidentalidad@data$CLASE == input$nombreAccidente)
                                  }
                                  select <- input$nombreAccidente},
           "Hora" = { # Se obtienen las dos horas del slider hourRange
                      Inicio <- input$hoursRange[1] 
                      Fin <- input$hoursRange[2]  
                     
                      # Como las horas no están como numéricas, se convierten a una unidad de tiempo (Horas)
                      # En la posición 9 a 10 está la hora, en la posición 12 a 13 están los minutos,
                      accidentalidad@data$HORA <- as.double(substr(accidentalidad@data$HORA,9,10)) + as.double(substr(accidentalidad@data$HORA,12,13))/60
                      
                      # Se hace una busqueda en la Base de datos según el rango de horas escogido
                      accidentalidad <- subset(accidentalidad, accidentalidad@data$HORA >= Inicio & accidentalidad@data$HORA < Fin)}
    )
    
    # Cuando no hay nada seleccionado no hay que cargar mapa aún, cuando se coge filtro Hora no se carga ningun select
    # de algún input
    if(!is.null(select)){
  
      show(head(accidentalidad))
      popup<-paste(accidentalidad@data$BARRIO)
      
      m<-leaflet()
      m<-fitBounds(m,
                   lng1=min(accidentalidad@coords[,1]), 
                   lat1=min(accidentalidad@coords[,2]), 
                   lng2=max(accidentalidad@coords[,1]),
                   lat2=max(accidentalidad@coords[,2]))
      m<-addProviderTiles(m,provider="OpenStreetMap.Mapnik")
      m<-addCircleMarkers(m,
                          lng = accidentalidad@coords[,1],
                          lat = accidentalidad@coords[,2],
                          popup = popup, 
                          radius = 2, 
                          stroke = FALSE,
                          fillOpacity = 0.75
      )
      m
    }
  })
})