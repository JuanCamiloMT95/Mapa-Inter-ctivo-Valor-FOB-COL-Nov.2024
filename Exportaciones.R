install.packages("sf")
install.packages("leaflet")
install.packages("string")
install.packages("rmapshaper")
install.packages("leaflet.extras")
install.packages("mapview")
install.packages("webshot")

library(stringr)
library(sf)
library(leaflet)
library(tidyverse)
library(classInt)
library(RColorBrewer)
library(rmapshaper)
library(leaflet.extras)
library(mapview)
library(webshot)

#Cargar Datos
dptos <- st_read("Servicio-199 (1)/GDB_Lineas_Limitrofes_ET_Diciembre_2024_V2_shp/Departamento.shp")
dptos <- st_transform(dptos, crs = 4326)

expo<-read_csv2("Expo_2024/Noviembre 2024/Noviembre.csv")

# clean data frames

dptos <- dptos %>%
  mutate(DeCodigo = as.numeric(DeCodigo)) %>% 
  filter(DeCodigo != 00)


totalExpo <- expo %>% group_by(DPTO1) %>% summarise(sum(FOBDOL))
totalExpo <- totalExpo %>%
  rename(DeCodigo = DPTO1) %>% 
  mutate(DeCodigo = as.numeric(DeCodigo)) %>% 
  mutate(`sum(FOBDOL)` = if_else(DeCodigo ==25, `sum(FOBDOL)`+`sum(FOBDOL)`[DeCodigo == 11],`sum(FOBDOL)`)) %>% 
  filter(DeCodigo != c(0, 11))

#Clases e intervalos

brks <- classIntervals(totalExpo$`sum(FOBDOL)`, 6, "quantile")
#brks <- 
brks <- round(brks$brks, 2)
clss <- findInterval(totalExpo$`sum(FOBDOL)`, brks, TRUE)
inter <- tibble(class = 1:6, min = brks[1:6], max = brks[2:7], interval = paste0(min, ' - ', max))

#Unir dataframes

totalExpo <- totalExpo %>% 
  mutate(class = clss)
  
totalExpo <- inner_join(totalExpo, inter, by = "class")

dptosExpo <- full_join(dptos, totalExpo, by = "DeCodigo")

dptosExpo <- dptosExpo %>% 
  mutate(`sum(FOBDOL)` = if_else(`sum(FOBDOL)`== 'NA', 0, `sum(FOBDOL)`))

class(dptosExpo$interval)
dptosExpo <- dptosExpo %>% mutate(interval = factor(interval, levels = inter$interval))

# Paleta de Colores

pal <- colorNumeric("viridis", NULL)
paln <- brewer.pal(n = 6, name = 'YlOrRd')
clrs <- tibble(interval = inter$interval, paln)
clrs <- mutate(clrs, interval = factor(interval, levels = inter$interval))

dptosExpo <- full_join(dptosExpo, clrs, by = 'interval')

dptosExpo <- rmapshaper::ms_simplify(dptosExpo)

# Mapa (Poligono)

leaflet() %>%
  addTiles() %>%
  setView(lng = -74.297333, lat = 4.570868, zoom = 5.5) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB") %>%
  addPolygons(data = dptosExpo, stroke = TRUE, color = 'white', weight = 0.8, fillColor = dptosExpo$paln,
              fillOpacity = 0.5, smoothFactor = 0.5,
              popup = paste0('Departamento: ', dptosExpo$DeNombre, '<br>',
                             "Total Valor FOB: ","US$ ", 
                             if_else(is.na(dptosExpo$`sum(FOBDOL)`) == TRUE, "No reporta",
                             format(dptosExpo$`sum(FOBDOL)`,big.mark = ".", decimal.mark = ",", scientific =  FALSE)),
                             '<br>')) %>% 
  addLegend('bottomright', colors = clrs$paln, labels = clrs$interval, title = "Intervalo del Valor FOB (US$)") %>% 
  addScaleBar(position = 'bottomright', options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE, updateWhenIdle = TRUE)) %>% 
  addDrawToolbar(targetGroup = "Graficos", editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>% 
  addMeasure(position = "topleft", primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters", activeColor = "#3D535D", completedColor = "#7D4479") %>% 
  addSearchOSM() %>% 
  addControlGPS() %>% 
  addMiniMap(tiles = "CartoDB.DarkMatter", position = 'topleft') %>% 
  addControl(html = "<p style = 'font-family:sans-serif; font-weight:Helvetica Neue'><strong>Total Valor FOB, noviembre 2024 (DANE - 2024)</strong></p>",
             position = 'topright') %>% 
  addControl(html = "<p style = 'font-family:sans-serif; font-weight:Helvetica Neue'><strong>Por: Juan Camilo Montealegre Tamayo </strong></p>",
             position = 'topright') %>% 
  addResetMapButton() %>% 
  mapshot(url = "./html/index.html", title = 'Valor Total FOB por Departamentos en Colombia, noviembre de 2024')
  
             
               

