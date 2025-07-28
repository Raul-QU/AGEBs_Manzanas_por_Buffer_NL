library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(DT)
library(tidyr)

shinyServer(function(input, output, session) {
  
  punto_click <- reactiveVal(NULL)
  
  observeEvent(input$mapa_click, {
    punto_click(st_sfc(st_point(c(input$mapa_click$lng, input$mapa_click$lat)), crs = 4326))
  })
  
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = mun, fill = FALSE, color = "black", weight = 1) %>%
      setView(lng = -100.3, lat = 25.6, zoom = 9)
  })
  
  observe({
    req(punto_click())
    leafletProxy("mapa") %>%
      clearGroup("punto") %>%
      addCircleMarkers(data = punto_click(), color = "blue", radius = 6,
                       stroke = TRUE, fillOpacity = 0.8, group = "punto")
  })
  
  observeEvent(input$generar, {
    req(punto_click())
    
    leafletProxy("mapa") %>%
      addPopups(lng = st_coordinates(punto_click())[1],
                lat = st_coordinates(punto_click())[2],
                popup = "Calculando resultados...", layerId = "popup_temp")
    
    isolate({
      punto_utm <- st_transform(punto_click(), 32614)
      buffer_utm <- st_buffer(punto_utm, dist = input$radio)
      buffer <- st_transform(buffer_utm, 4326)
      
      base <- if (input$base_datos == "ageb") ageb else manzanas
      
      base_utm <- st_transform(base, 32614)
      base_utm$area_total <- as.numeric(st_area(base_utm))
      base_utm$id_poly <- seq_len(nrow(base_utm))
      
      inter <- suppressWarnings(st_intersection(base_utm, buffer_utm))
      inter$area_inter <- as.numeric(st_area(inter))
      inter$id_poly <- sapply(st_within(inter, base_utm), function(x) if(length(x)) x[1] else NA)
      
      inter_suma <- inter %>%
        st_drop_geometry() %>%
        group_by(id_poly) %>%
        summarise(area_inter = sum(area_inter, na.rm = TRUE)) %>%
        ungroup()
      
      base_area <- base_utm %>%
        st_drop_geometry() %>%
        select(id_poly, area_total)
      
      df_prop <- left_join(base_area, inter_suma, by = "id_poly") %>%
        mutate(area_inter = ifelse(is.na(area_inter), 0, area_inter),
               prop = area_inter / area_total)
      
      ids_seleccion <- df_prop %>% filter(prop >= 0.5) %>% pull(id_poly)
      seleccion <- base_utm %>% filter(id_poly %in% ids_seleccion) %>% st_transform(4326)
      
      vars_sum <- c("POB_TOT", "PAM_TOT", "PAM_HOM", "PAM_MUN",
                    "PAM_DISC", "PAM_PRIM_I", "PAM_PEA", "PAM_NOAFME")
      
      resumen <- seleccion %>%
        st_drop_geometry() %>%
        select(all_of(vars_sum)) %>%
        summarise(across(everything(), sum, na.rm = TRUE)) 
      
      etiquetas <- c(
        POB_TOT = "Población total",
        PAM_TOT = "Población Adulta Mayor",
        PAM_HOM = "Población Adulta Mayor Hombre",
        PAM_MUN = "Población Adulta Mayor Mujer",
        PAM_DISC = "Población Adulta Mayor con Discapacidad",
        PAM_PRIM_I = "Población Adulta Mayor con Rezago Educativo",
        PAM_PEA = "Población Adulta Mayor Económicamente Activa",
        PAM_NOAFME = "Población Adulta Mayor sin Afiliación médica"
      )
      
      resumen <- resumen %>%
        pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor") %>%
        mutate(Variable = etiquetas[Variable])
      
      output$tabla <- DT::renderDT({
        DT::datatable(resumen, rownames = FALSE, options = list(dom = 't')) %>%
          DT::formatStyle("Variable", fontWeight = "bold")
      })
      
      leafletProxy("mapa") %>%
        clearGroup("buffer") %>%
        clearGroup("seleccionados") %>%
        addPolygons(data = buffer, color = "red", fillColor = "#FFAAAA", fillOpacity = 0.3,
                    weight = 2, group = "buffer") %>%
        addPolygons(data = seleccion, color = "blue", fillColor = "#74c476", fillOpacity = 0.5,
                    weight = 2, group = "seleccionados") %>%
        removePopup("popup_temp")
    })
  })
  
})