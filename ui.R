##Incio del UI

library(shiny)
library(leaflet)
library(shinyTime)

shinyUI(
  
  navbarPage("MAPAS",
             tabPanel(title= "Estadistica Descriptiva",
                      div(class="outer",
                          tags$head(
                            
                            # Include our custom CSS
                            includeCSS("styles.css")
                          ),
                          leafletOutput("map", width="100%", height="100%"),
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        h3("Controles", class="text-center"),
                                        selectInput(inputId="year", label="Año", choices= c("2015", "2016", "2017")),
                                        selectInput(inputId="filtro", label="Filtro", choices= c( "Zona", "Hora", "Tipo de Accidente")),
                                        conditionalPanel("input.filtro == 'Hora'",
                                                          sliderInput("hoursRange", "Rango de horas ",
                                                                      min = 0, max = 24, value = c(10,11)),
                                                         hr(),
                                                         fluidRow(column(3, verbatimTextOutput("value")))
                                                         
                                                         
                                        ),
                                        conditionalPanel("input.filtro == 'Zona'",
                                                         uiOutput("zonas")
                                        ),
                                        conditionalPanel("input.filtro == 'Tipo de Accidente'",
                                                         uiOutput("tipoAccidente")
                                        )
                                        
                                        
                          )
                      )
             ),
             tabPanel(title= "Inferencia")
  )
)
