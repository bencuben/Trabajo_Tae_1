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
                                        selectInput(inputId="filtro", label="Filtro", choices= c("Hora", "Zona", "Tipo de Accidente")),
                                        conditionalPanel("input.filtro == 'Hora'",
                                                         # sliderInput("hoursRange", "Rango de horas ",
                                                         #             min = 0, max = 24, value = c(10,11))
                                                         selectInput(inputId="hoursRange", label = "Franja horaria",
                                                                     choices = list("12:00 AM - 01:00 AM" = 1, "01:00 AM - 02:00 AM" = 2, 
                                                                                    "02:00 AM - 03:00 AM" = 3, "03:00 AM - 04:00 AM" = 4,
                                                                                    "04:00 AM - 05:00 AM" = 5, "05:00 AM - 06:00 AM" = 6,
                                                                                    "06:00 AM - 07:00 AM" = 7, "07:00 AM - 08:00 AM" = 8,
                                                                                    "08:00 AM - 09:00 AM" = 9, "09:00 AM - 10:00 AM" = 10,
                                                                                    "10:00 AM - 11:00 AM" = 11, "11:00 AM - 12:00 PM" = 12,
                                                                                    "12:00 PM - 01:00 PM" = 13, "01:00 PM - 14:00 PM" = 14,
                                                                                    "02:00 PM - 03:00 PM" = 15, "03:00 AM - 16:00 PM" = 16,
                                                                                    "04:00 PM - 05:00 PM" = 17, "05:00 AM - 18:00 PM" = 18,
                                                                                    "06:00 PM - 07:00 PM" = 19, "07:00 AM - 20:00 PM" = 20,
                                                                                    "08:00 PM - 09:00 PM" = 21, "09:00 AM - 22:00 PM" = 22,
                                                                                    "10:00 PM - 11:00 PM" = 23, "11:00 PM - 12:00 AM" = 24), 
                                                                     selected = 1),
                                                         
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




