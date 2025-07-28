library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(DT)

# Cargar capas
mun <- st_read("Data/19mun.shp", options = "ENCODING=WINDOWS-1252") %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"))

ageb <- st_read("Data/19a.shp", options = "ENCODING=WINDOWS-1252") %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"))

manzanas <- st_read("Data/19m.shp", options = "ENCODING=WINDOWS-1252") %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"))
