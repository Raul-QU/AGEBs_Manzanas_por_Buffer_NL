library(shiny)
library(leaflet)
library(DT)

shinyUI(fluidPage(
  titlePanel("Mapa interactivo con información de AGEBs y Manzanas de Nuevo León"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("base_datos", "Selecciona la base de datos:",
                  choices = c("AGEB" = "ageb", "Manzanas" = "manzanas")),
      numericInput("radio", "Radio buffer (metros):", value = 500, min = 0, max = 5000, step = 50),
      actionButton("generar", "Generar buffer y resumen"),
      hr(),
      tags$h4("Resumen de variables dentro del buffer"),
      DT::DTOutput("tabla", width = "100%")
    ),
    
    mainPanel(
      leafletOutput("mapa", height = 700),
      tags$div(
        style = "margin-top: 10px; font-size: 12px;",
        "Fuente: Elaboración del IEPAM con información del Censo de Población y Vivienda 2020, INEGI",
        tags$br(),
        tags$span(style = "font-size:10px; color: #555;",
                  "Contacto: ana.cardenas@nuevoleon.gob.mx / raul.quezada@nuevoleon.gob.mx")
      )
    )
  )
))