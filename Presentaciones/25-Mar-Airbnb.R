#Limpiador de Airbnb
#library(readr)
#airbnb <- read_csv("puerto-rico_Property_Match_2020-07-17.csv")
#View(airbnb)
#library(dplyr)
#airbnb<-airbnb|>select(Latitude, Longitude,City,`Metropolitan Statistical Area`,
#                       `Average Daily Rate (USD)`,`Annual Revenue LTM (USD)`,`Airbnb Superhost`,
#                       `Occupancy Rate LTM`,`Airbnb Host ID`,`Property ID`,`Property Type`,)
#ver si hay datos faltantes
#write_csv(airbnb,"airbnb.csv")

library(readr)
library(dplyr)

read_csv("airbnb.csv") -> airbnb #noten que al revés funciona igual :O

#ver cantidad de propiedades agrupadas por airbnb host id
datosconcentrados<-airbnb|>group_by(`Airbnb Host ID`)|>summarise(n=n(),
                                              ingresos=sum(`Annual Revenue LTM (USD)`))
#ver concentración de ingreso y propiedades, ¿se relacionan?
#quitar notación científica
options(scipen=999)

#exploremos los datos
library(ggplot2)
datosconcentrados|>ggplot(aes(x=n,y=ingresos))+
  geom_point()+ 
  geom_smooth(method = "loess", se =T)+
  scale_x_continuous(trans = "log10")+
  labs(title="Concentración de ingresos y propiedades por Airbnb Host ID",
       x="Cantidad de propiedades",
       y="log(Ingresos)")

#ver si hay relación entre el tipo de propiedad y el ingreso
airbnb|>group_by(`Property Type`)|>summarise(ingresos=sum(`Annual Revenue LTM (USD)`))|>ggplot(aes(x=`Property Type`,y=ingresos))+
  geom_col()+
  coord_flip()+
  labs(title="Ingresos por tipo de propiedad",
       x="Tipo de propiedad",
       y="Ingresos")

as.factor(airbnb$`Property Type`)|>table() |>sort(decreasing = T)
library(stringr)

#quizá son muchos tipos de propiedad, podríamos agruparlos en tres o cuatro categorías
airbnb<-airbnb |>
  mutate(
    PropertyTypeClean = str_to_lower(trimws(`Property Type`)),
    `Tipo de propiedad` = case_when(
      PropertyTypeClean %in% str_to_lower(c("Apartment", "Condominium", "House", "Loft", "Townhouse")) ~ "Residencial",
      PropertyTypeClean %in% str_to_lower(c("Bed & Breakfast", "Bungalow", "Cabin", "Cottage", "Guesthouse", "Hostel", "Hotel", "Resort", "Villa")) ~ "Turismo",
      PropertyTypeClean %in% str_to_lower(c("Boat", "Camper/RV", "Dorm", "Earth House", "Farm stay", "Hut", "Island", "Lighthouse", "Tent", "Tiny house", "Treehouse")) ~ "Exótico",
      !is.na(PropertyTypeClean) ~ "Otros",
      TRUE ~ "No especificado"
    )
  )  
airbnb |>
  group_by(`Tipo de propiedad`) |>
  summarise(ingresos = sum(`Annual Revenue LTM (USD)`, na.rm = TRUE)) |>
  ggplot(aes(x = `Tipo de propiedad`, y = ingresos)) +
  geom_col() +
  coord_flip() +
  labs(title = "Ingresos por tipo de propiedad (agrupado)",
       x = "Tipo de propiedad",
       y = "Ingresos")

airbnb <- airbnb |>
  mutate(
    Multiple = if_else(
      duplicated(`Airbnb Host ID`) | duplicated(`Airbnb Host ID`, fromLast = TRUE),
      "Multiple",
      "Sencillo"
    )
  )

# Graficar ingresos según clasificación de host
airbnb |>
  group_by(Multiple) |>
  summarise(ingresos = sum(`Annual Revenue LTM (USD)`, na.rm = TRUE)) |>
  ggplot(aes(x = Multiple, y = ingresos, fill = Multiple)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Concentración de ingresos según tipo de host",
       x = "Tipo de Host",
       y = "Ingresos (USD)") +
  theme_minimal()

#usar barras stacked para ver tipo de propiedad y si tienen varias propiedades
airbnb |>
  group_by(`Tipo de propiedad`, Multiple) |>
  summarise(ingresos = sum(`Annual Revenue LTM (USD)`, na.rm = TRUE)) |>
  ggplot(aes(x = `Tipo de propiedad`, y = ingresos, fill = Multiple)) +
  geom_col(position = "stack") +
  coord_flip() +
  labs(title = "Ingresos por tipo de propiedad y tipo de host",
       x = "Tipo de propiedad",
       y = "Ingresos (USD)")

library(scales)

airbnb |>
  group_by(`Tipo de propiedad`, Multiple) |>
  summarise(ingresos = sum(`Annual Revenue LTM (USD)`, na.rm = TRUE)) |>
  mutate(
    pct = ingresos / sum(ingresos),
    label = percent(pct, accuracy = 0.1)
  ) |>
  ggplot(aes(x = `Tipo de propiedad`, y = ingresos, fill = Multiple)) +
  geom_col(position = "stack") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3.5) +
  coord_flip() +
  labs(title = "Ingresos por tipo de propiedad y tipo de host",
       x = "Tipo de propiedad",
       y = "Ingresos (USD)") +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal()

#usemos ahora sf para crear un mapa
library(sf)
library(tigris)
library(leaflet) 


df <- na.omit(airbnb)
?st_as_sf
df_sf <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326) # Convertir a objeto sf
library(tidyverse)
?crs

# Configurar tu API key de Census (si aún no lo has hecho)
#census_api_key("aquivaelkey", install = TRUE)

# Descargar los límites de municipios de Puerto Rico
pr_municipios <- tigris::counties(state = "72", cb = TRUE, class = "sf") # 72 es el código FIPS de PR

#Un mapa de Airbnb en Puerto Rico
# Crear el mapa
ggplot() +
  geom_sf(data = pr_municipios, fill = "gray90", color = "white") +  # Mapa base de PR
  geom_sf(data = df_sf, aes(color = factor(`Airbnb Superhost`)), alpha = 0.7) +  # Airbnb con colores según Superhost
  scale_color_manual(values = c("red", "blue"), labels = c("No", "Sí")) +  
  theme_minimal() +
  labs(
    title = "Distribución de Airbnb Superhosts en Puerto Rico",
    color = "Superhost"
  )

# ahora hagamos un mapa más interactivo

leaflet() |>                                      # Paso 1
  addTiles() |>                                   # Paso 2
  addPolygons(data = pr_municipios,               # Paso 3
              color = "black",                    
              weight = 1,                         
              fillOpacity = 0.3,                  
              popup = ~geometry) |>               
  addCircleMarkers(data = df_sf,                  # Paso 4
                   radius = 1,                    
                   color = "red",                 
                   fillOpacity = 0.8,             
                   popup = ~as.character(geometry))

#paso 1: crear un mapa base interactivo vacío con leaflet()
#paso 2: agregar un mapa base con 'baldosas' o 'losas' (tiles) con addTiles(), 
# típicamente OpenStreetMap. Esta capa da el fondo geográfico del mapa.
#paso 3: agregar los límites de los municipios de Puerto Rico con addPolygons().
# los municipios se bajaron con tigris::counties() y se convirtieron a un objeto sf.
# se les dio un color negro, mas se le puso transparencia para poder ver capa posterior.
# el weight es el grosor de la línea separadora de municipios, mientras que popup
# hace que al cliquear un municipio salga una etiqueta de la geometría (si aprieto
# en Caguas, saldrá Caguas).
#paso 4: addCircleMarkers() añade puntos o marcadores sobre el mapa, circulares.
# los datos vienen del objeto que creáramos con latitud y longitud, df_sf.
# 
#Notamos el error que salió al correr esto antes:
#Warning message:
#sf layer has inconsistent datum (+proj=longlat +datum=NAD83 +no_defs).
#Need '+proj=longlat +datum=WGS84' 
# así que transfomamos los conjuntos al mismo sistema de proyección,
# en este caso World Geodetic System 1984, EPSG:4326
# https://epsg.io/4326, en R lo verán más como CRS, Co-ordinate Reference System.

# Transformar ambos conjuntos al mismo CRS WGS84
pr_municipios <- st_transform(pr_municipios, crs = 4326)
df_sf <- st_transform(df_sf, crs = 4326)

# Luego haces el mapa de nuevo, esta vez le pondremos color de una variable
# Crear una paleta de colores basada en la columna 'Multiple'
pal <- colorFactor(c("blue", "red"), domain = df_sf$Multiple)
# Crear el mapa
leaflet() |>
  addTiles() |>
  addPolygons(data = pr_municipios, 
              color = "black",
              weight = 1,
              fillOpacity = 0.3,
              popup = ~as.character(NAME)) |>
  addCircleMarkers(data = df_sf,
                   radius = 3,
                   color = ~pal(Multiple),  # aquí usas la paleta
                   fillOpacity = 0.8,
                   popup = ~as.character(`Airbnb Host ID`)) |>
  addLegend("bottomright", 
            pal = pal, 
            values = df_sf$Multiple,
            title = "Tipo de host: una o múltiples propiedades",
            opacity = 1)
