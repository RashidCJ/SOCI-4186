# Instalar y cargar los paquetes necesarios
#install.packages("tidyverse")
#install.packages("rvest")


# 1. Introducción ------------------------------------------------------------

library(tidyverse)
library(rvest)

# Definir la URL de la página de Wikipedia
url <- "https://en.wikipedia.org/wiki/Eurovision_Song_Contest_2024"

# Leer y analizar el contenido HTML de la página
pagina <- read_html(url)

# Extraer todas las tablas de la página
tablas <- pagina |> html_table(fill = TRUE)
tablas
# Identificar las tablas de interés
# Supongamos que las tablas de interés son la 15 (jurado) y la 16 (televoto)
# Es recomendable inspeccionar las tablas para confirmar sus posiciones
tabla_jurado <- tablas[[15]]
tabla_televoto <- tablas[[16]]

# Mostrar las primeras filas de las tablas para verificar
head(tabla_jurado)
head(tabla_televoto)
colnames(tabla_jurado)
# Supongamos que 'tabla' es el marco de datos después de las modificaciones
# Verifica el número de columnas
num_columnas <- ncol(tabla_jurado)
num_columnas1 <- ncol(tabla_televoto)

# Crea un vector de nombres con la misma longitud que el número de columnas
nuevos_nombres <- c(paste0("Columna", 1:num_columnas))
nuevos_nombres1 <- c(paste0("Columna", 1:num_columnas1))

# Asegúrate de que la longitud de 'nuevos_nombres' coincida con 'num_columnas'
if(length(nuevos_nombres) == num_columnas) {
  colnames(tabla_jurado) <- nuevos_nombres
} else {
  stop("La cantidad de nuevos nombres no coincide con el número de columnas en la tabla.")
}

# Asegúrate de que la longitud de 'nuevos_nombres' coincida con 'num_columnas'
if(length(nuevos_nombres1) == num_columnas1) {
  colnames(tabla_televoto) <- nuevos_nombres1
} else {
  stop("La cantidad de nuevos nombres no coincide con el número de columnas en la tabla.")
}

# 2. Limpieza y procesamiento  -----------------------------------------------

# Procesar y limpiar las tablas
# Omitir la primera fila y la primera columna, y asignar nombres de columna adecuados
# Paso 2: Convertir la primera fila en nombres de columna y eliminarla del data frame
#colnames(tabla_jurado) <- tabla_jurado[1, ]
#tabla_jurado <- tabla_jurado[-1, ]
tabla_jurado
tabla_televoto
tabla_jurado <- tabla_jurado |>
  select(-c(1, 3:5)) |>
  slice(2:n())
tabla_televoto <- tabla_televoto |>
  select(-c(1, 3:5)) |>
  slice(2:n())
tabla_televoto
tabla_televoto<-tabla_televoto |>
  (\(df) {
    nuevos_nombres <- df |> slice(1) |> unlist() |> as.character()
    set_names(df, nuevos_nombres)
  })() |>
  slice(-1)
tabla_televoto

tabla_jurado<-tabla_jurado |>
  (\(df) {
    nuevos_nombres <- df |> slice(1) |> unlist() |> as.character()
    set_names(df, nuevos_nombres)
  })() |>
  slice(-1)


sociomatriz_jurado<-tabla_jurado<-tabla_jurado |>
  slice(-1) |>  # Elimina la primera fila
  mutate(across(2:38, as.numeric)) |>
  mutate(across(2:38, ~replace_na(.,0)))
sociomatriz_jurado
tabla_televoto
sociomatriz_televoto<-tabla_televoto<-tabla_televoto |>
  slice(-1) |>  # Elimina la primera fila
  mutate(across(2:39, as.numeric)) |>
  mutate(across(2:39, ~replace_na(.,0)))
#View(sociomatriz_jurado)

#quiero que imprima todo en sociomatriz
print(sociomatriz_jurado, n=25)
print(sociomatriz_televoto, n=25)
#delete the last row
sociomatriz_jurado<-sociomatriz_jurado|>
  filter(row_number() <= n()-1)
sociomatriz_televoto<-sociomatriz_televoto|>
  filter(row_number() <= n()-1)
sociomatriz_jurado<-sociomatriz_jurado|>rename(to=`Voting procedure used:  100% Televoting  100% Jury vote`)
sociomatriz_televoto<-sociomatriz_televoto|>rename(to=`Voting procedure used:  100% Televoting  100% Jury vote`)
# Convertir en tabla de aristas (de formato wide a long)
aristas <- sociomatriz_jurado |>
  pivot_longer(-to, names_to = "from", values_to = "weight") |>
  filter(weight > 0)  # Opcional: solo conexiones con votos positivos
aristas_televoto<- sociomatriz_televoto |>
  pivot_longer(-to, names_to = "from", values_to = "weight") |>
  filter(weight > 0)  # Opcional: solo conexiones con votos positivos
library(stringi)

aristas <- aristas |>
  mutate(
    from = str_trim(stri_trans_general(from, "Latin-ASCII")),
    to = str_trim(stri_trans_general(to, "Latin-ASCII"))
  )
aristas_televoto <- aristas_televoto|>
  mutate(
    from = str_trim(stri_trans_general(from, "Latin-ASCII")),
    to = str_trim(stri_trans_general(to, "Latin-ASCII"))
  )
aristas
aristas_televoto
print(aristas,n=50)

# 3. Generación de aristas diádicas-------------------------------------------------------------------------


aristas_summary <- aristas |>
  group_by(from, to) |>
  summarise(votos = sum(weight), .groups = 'drop')
aristas_televoto_sum<- aristas_televoto |>
  group_by(from, to) |>
  summarise(votos = sum(weight), .groups = 'drop')

aristas_combinadas <- aristas_summary |>
  full_join(aristas_televoto_sum, by = c("from", "to"), suffix = c("_jurado", "_televoto")) |>
  mutate(
    votos_jurado = replace_na(votos_jurado, 0),
    votos_televoto = replace_na(votos_televoto, 0)
  ) |>
  mutate(peso = votos_jurado + votos_televoto) |>
  select(from, to, peso)

aristas
aristas_summary
aristas_televoto
aristas_televoto_sum
aristas_combinadas
aristas_summary
aristas_televoto_sum
#exportar a CSV para clase en sociomatriz y lista de aristas
write_csv(sociomatriz_jurado, "sociomatriz_jurado.csv")
write_csv(sociomatriz_televoto, "sociomatriz_televoto.csv")
write_csv(aristas, "aristas_jurado.csv")
write_csv(aristas_summary, "aristas_jurado_sum.csv")
write_csv(aristas_televoto, "aristas_televoto.csv")
write_csv(aristas_televoto_sum, "aristas_televoto_sum.csv")
write_csv(aristas_combinadas, "aristas_combinadas2024.csv")
aristas_combinadas
#dame un objeto de todos los países de aristas_combinadas, unique
paises_aristas_combinadas<-aristas_combinadas|>
  select(from,to) |>
  pivot_longer(cols = everything(), names_to = "tipo", values_to = "pais") |>
  distinct(pais)
print(paises_aristas_combinadas,n=39)
#creo una lista de los países de 2005 (basado en el paper) y 2024:
bloques_amistad_2005_2024 <- tibble(
  name = c("Norway", "Sweden", "Finland", "Iceland", "Denmark",  # Nórdicos
           "Serbia", "Croatia", "Slovenia", "Albania", "Bosnia", "Macedonia",      # Balcanes
           "Russia", "Ukraine", "Moldova", "Latvia", "Lithuania", #Este
           "Estonia", "Hungary", "Romania", "Belarus", "Poland",    # Este
           "Azerbaijan", "Georgia", "Armenia", "Czechia", # Este
           "Greece", "Cyprus", "Malta", "Turkey", "Bulgaria", # Mediterráneo oriental
           "France", "Spain", "Germany", "United Kingdom", "Switzerland", "Italy", #Occidental
           "Monaco", "Israel", "Ireland", "Portugal", "Andorra", "Belgium", "Netherlands", #Occidentales
           "Australia", "San Marino","Luxembourg", "Austria","Rest of the World"),   # Occidentales
  bloque = c(rep("Nórdico", 5),
             rep("Balcánico", 6),
             rep("Oriental", 14),
             rep("Mediterráneo Oriental", 5),
             rep("Occidental", 18)),
  miembro_OTAN = c("Sí", "Sí", "Sí", "Sí", "Sí",
                   "No", "Sí", "Sí", "Sí", "No","Sí",
                   "No", "No", "No", "Sí", "Sí",
                   "Sí", "Sí", "Sí", "No", "Sí",
                   "No", "No", "No", "Sí",
                   "Sí", "No", "No", "Sí", "Sí",
                   "Sí", "Sí", "Sí", "Sí", "No", "Sí",
                   "No", "No", "No", "Sí", "No", "Sí", "Sí",
                   "No", "No", "Sí", "No", "No")
)

# añadir bloque como variable en una node_list basados en el las aristas combinadas
paises_aristas_combinadas<-paises_aristas_combinadas|>
  left_join(bloques_amistad_2005_2024, by = c("pais" = "name")) 
paises_aristas_combinadas

# 4. Porción de replicación de Dekker pero en 2024 ------------------------------------------------

## Esta parte es para fines de montar la replicación de Dekker en 2024. --
total<-aristas_combinadas|>group_by(to)|>
  summarise(total = sum(peso))
aristas_y_total<-aristas_combinadas|>
  left_join(total,by=c("to"="to"))
modeloreplicatorio<-lm(peso~total,data = aristas_y_total)
summary(modeloreplicatorio)
aristas_y_total <- aristas_y_total %>%
  mutate(voto_predicho = 3.0243 + 0.020845 * total,
         friendship_raw = peso - voto_predicho)
constante = 14.3438
aristas_y_total <- aristas_y_total %>%
  mutate(amistad = friendship_raw + constante)
aristas_y_total
# 5. Generando la red: Jurados y televoto ----------------
library(igraph)

# Crear el objeto de red dirigido desde la tabla de aristas
g_red <- graph_from_data_frame(
  d = aristas_summary,
  vertices = data.frame(nombre = unique(c(aristas$from, aristas$to))),
  directed = TRUE
)
g_red

g_red_televoto <- graph_from_data_frame(
  d = aristas_televoto_sum,
  vertices = data.frame(nombre = unique(c(aristas_televoto_sum$from, aristas_televoto_sum$to))),
  directed = TRUE
)
g_red_televoto
# Asignar los votos agregados como atributo de peso

# Calcular el in-degree ponderado (suma de votos recibidos por cada país)
V(g_red)$grado_entrada <- strength(g_red, mode = "in", weights = E(g_red)$votos)
V(g_red_televoto)$grado_entrada <- strength(g_red_televoto, mode = "in", weights = E(g_red_televoto)$votos)

# Ver países ordenados por votos recibidos (mayor a menor)
sort(V(g_red)$grado_entrada, decreasing = TRUE)
sort(V(g_red_televoto)$grado_entrada, decreasing = TRUE)

# Resumen básico de la red
print(g_red)
summary(g_red)

# Visualización: tamaño del nodo según votos recibidos, ancho de la arista según peso
plot(
  g_red,
  edge.width = E(g_red)$peso,
  vertex.label.cex = 0.8,
  vertex.label.color = "black",
  vertex.size = V(g_red)$grado_entrada / 15,  # escalar si necesario
  layout = layout_with_fr, #compárese layout_in_circle, layout_with_kk, layout_as_star
  #layout_as_tree, layout_with_dh o layout_nicely
)

plot(
  g_red_televoto,
  edge.width = E(g_red_televoto)$peso,
  vertex.label.cex = 0.8,
  vertex.label.color = "black",
  vertex.size = V(g_red_televoto)$grado_entrada / 15,  # escalar si necesario
  layout = layout_with_fr, #compárese layout_in_circle, layout_with_kk, layout_as_star
  #layout_as_tree, layout_with_dh o layout_nicely
)
## 5.a. caracterización de las redes --------
# Calcular grado de entrada y salida para cada país
grado_entrada_jur  <- degree(g_red, mode = "in")
grado_salida_jur   <- degree(g_red, mode = "out")
grado_entrada_jur #algo más informativo
grado_salida_jur #no es tan informativo: sólo hay diez salidas
#Grados para televoto
grado_entrada_televoto<- degree(g_red_televoto, mode = "in")
grado_salida_televoto<- degree(g_red_televoto, mode = "out")
grado_entrada_televoto
grado_salida_televoto #no es tan informativo: sólo hay diez salidas
# Fuerza (suma ponderada) de entrada y salida
fuerza_entrada_jur  <- strength(g_red, mode = "in", weights = E(g_red)$votos)
fuerza_salida_jur   <- strength(g_red, mode = "out", weights = E(g_red)$votos)
fuerza_entrada_jur #noten que la diferencia es que se pondera vs mero conteo de relación
fuerza_salida_jur #noten que la diferencia es que se pondera vs mero conteo de relación
#fuerzas para televoto
fuerza_entrada_televoto  <- strength(g_red_televoto, mode = "in", weights = E(g_red_televoto)$votos)
fuerza_salida_televoto   <- strength(g_red_televoto, mode = "out", weights = E(g_red_televoto)$votos)
fuerza_entrada_televoto #noten que la diferencia es que se pondera vs mero conteo de relación
fuerza_salida_televoto #noten que la diferencia es que se pondera vs mero conteo de relación
# Centralidad de intermediación (betweenness)
intermediacion_jur <- betweenness(g_red, directed = TRUE, weights = E(g_red)$peso)
#intermediacion_jur
sort(intermediacion_jur,decreasing = T)
# Intermediación para televoto
intermediacion_televoto <- betweenness(g_red_televoto, directed = TRUE, weights = E(g_red_televoto)$votos)
#intermediacion_televoto
sort(intermediacion_televoto,decreasing = T)

#la intermediación elevada de Suiza es de esperar según Dekker, países neutrales o multiculturales como puentes entre bloques
#Italia Croacia Francia Suecia o Armenia también tienen altos niveles en este caso.
#no mide votos recibidos, sino el rol estructural de conexión interior.

# Cercanía (closeness) de entrada y salida
cercania_entrada_jur  <- closeness(g_red, mode = "in")
cercania_salida_jur   <- closeness(g_red, mode = "out")
sort(cercania_entrada_jur,decreasing = T)
sort(cercania_salida_jur,decreasing = T)
# Cercanía para el televoto
cercania_entrada_televoto <- closeness(g_red_televoto, mode="in")
cercania_salida_televoto <- closeness(g_red_televoto, mode="out")
sort(cercania_entrada_televoto,decreasing = T)
sort(cercania_salida_televoto,decreasing = T)

# Centralidad de autovector (eigenvector)
autovector_jur <- eigen_centrality(g_red, directed = TRUE, weights = E(g_red)$peso)$vector
sort(autovector_jur, decreasing = TRUE)
#Centralidad de autovector (eigenvector)
autovector_televoto <- eigen_centrality(g_red_televoto, directed = TRUE, weights = E(g_red_televoto)$peso)$vector
sort(autovector_televoto, decreasing = TRUE)
# PageRank, útil en redes dirigidas
pagerank_jur <- page_rank(g_red, directed = TRUE, weights = E(g_red)$peso)$vector
sort(pagerank_jur, decreasing=T)
pagerank_televoto <- page_rank(g_red_televoto, directed = TRUE, weights = E(g_red_televoto)$votos)$vector
# Densidad de la red
densidad_jur <- edge_density(g_red)
densidad_jur
densidad_televoto <-edge_density(g_red_televoto)
densidad_televoto
# Reciprocidad (proporción de aristas bidireccionales)
reciprocidad_jur <- reciprocity(g_red)
reciprocidad_jur
reciprocidad_televoto <- reciprocity(g_red_televoto)
reciprocidad_televoto
# Detección de comunidades usando el algoritmo Infomap
comunidades_jur <- cluster_infomap(g_red)
comunidades_jur
comunidades_televoto <- cluster_infomap(g_red_televoto)
comunidades_televoto
# Información resumida de las comunidades detectadas
summary(comunidades_jur) #salta error por falta de división
## 5.b Estableciendo la división por Louvain ------------------
comunidades_louvain_jur <- cluster_louvain(as_undirected(g_red))
comunidades_louvain_jur
comunidades_louvain_tele <- cluster_louvain(as_undirected(g_red_televoto))
comunidades_louvain_tele

plot(comunidades_louvain_jur, g_red)
plot(comunidades_louvain_tele, g_red_televoto)
grafojur<-plot(
  g_red,
  vertex.color = membership(comunidades_louvain_jur),
  vertex.size = 10,
  vertex.label.cex = 0.8,
  edge.width = E(g_red)$peso / 4,
  layout = layout_with_fr
)
grafotele<-plot(
  g_red_televoto,
  vertex.color = membership(comunidades_louvain_tele),
  vertex.size = 10,
  vertex.label.cex = 0.8,
  edge.width = E(g_red_televoto)$peso / 4,
  layout = layout_with_fr
)


print(membership(comunidades_louvain_jur))
print(membership(comunidades_louvain_tele))
# Número de comunidades encontradas
num_comunidades <- length(unique(membership(comunidades_jur)))
num_comunidades_L <- length(unique(membership(comunidades_louvain_jur)))

cat("Número de comunidades detectadas en cluster infomap:", num_comunidades, "\n")
cat("Número de comunidades detectadas en algoritmo Louvain:", num_comunidades_L, "\n")

# 6. Replicación de Dekker (parcial) en 2024 ---------------------
#crear columna con noxmnbres en español de países
library(countries)
library(countrycode)
aristas_y_total
bloques_amistad_2005_2024

aristas_y_total<-aristas_y_total|>
  left_join(bloques_amistad_2005_2024, by = c("to" = "name")) |>
  mutate(
    from = country_name(from, to="name_es"),
    to= country_name(to,to="name_es")
  ) 
bloques_amistad_2005_2024<-bloques_amistad_2005_2024 |> 
  mutate(name = country_name(name, to="name_es"),
         name = ifelse(is.na(name), "Resto del Mundo", name)) 
  
#cambiar el NA formado por Rest of the World que no es un país, cambiar ese NA en from a 'Resto del Mundo'
aristas_y_total<-aristas_y_total |> 
  mutate(from = ifelse(is.na(from), "Resto del Mundo", from)) |>
  mutate(to = ifelse(is.na(to), "Resto del Mundo", to))


print(aristas_y_total,n=15)

##  6.a Uso de tidygraph y ggraph ---------------- 
library(tidygraph)
library(ggraph)

#convierte aristas_y_total a grafo por tidygraph

grafo_panas <- aristas_y_total |>
  filter(amistad > 0) |>
  as_tbl_graph(directed = TRUE)

#generar centralidad para grafo y otras estadísticas útiles para gráficas y estadísticas
grafo_panas <- grafo_panas %>%
  mutate(
    centralidad = centrality_degree(mode = "all"),
    fuerza_entrada = centrality_degree(mode = "in", weights = amistad),
    fuerza_salida = centrality_degree(mode = "out", weights = amistad),
    intermediacion = centrality_betweenness(directed = TRUE, weights = amistad),
    cercania_entrada = centrality_closeness(mode = "in"),
    cercania_salida = centrality_closeness(mode = "out")
  )
# Calcular fuera del pipeline: autovector y pagerank
eig <- eigen_centrality(as.igraph(grafo_panas), directed = TRUE, weights = E(grafo_panas)$amistad)
pg <- page_rank(as.igraph(grafo_panas), directed = TRUE, weights = E(grafo_panas)$amistad)

# Añadirlos al objeto tidygraph
grafo_panas <- grafo_panas %>%
  mutate(
    autovector = eig$vector,
    pagerank = pg$vector
  )
# Visualizar solo relaciones,

ggraph(grafo_panas, layout = "fr") +
  geom_edge_link(aes(width = amistad / 30, alpha = amistad), show.legend = TRUE) +
  geom_node_point(aes(size = centralidad), color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_graph() +
  ggtitle("Red de amistades en Eurovisión (2024)")
## 6.b Componentes fuertemente conexos ---------------- 
components(grafo_panas, mode = "strong")
grafo_tbl <- as_tbl_graph(grafo_panas) %>%
  mutate(componente = components(grafo_panas, mode = "strong")$membership)

ggraph(grafo_tbl, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(color = as.factor(componente)), size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3,colour = "blue3") +
  theme_void() +
  ggtitle("Componentes Fuertemente Conexos en Red de Panas")
grafo_tbl <- grafo_tbl %>%
  activate(nodes) %>%
  left_join(bloques_amistad_2005_2024, by = "name")
ggraph(grafo_tbl, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = centralidad, color = bloque)) +
  geom_node_text(aes(label = name, color = bloque), repel = TRUE, size = 3)
  theme_void() +
  ggtitle("Componentes identificados como conexos por Dekker")
  
# Gráfica por bloque (Dekker)
p1 <- ggraph(grafo_tbl, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = centralidad, color = bloque)) +
  geom_node_text(aes(label = name, color = bloque), repel = TRUE, size = 3) +
  theme_void() +
  ggtitle("Agrupación por bloques (Dekker)")

# Gráfica por componente fuerte real
p2 <- ggraph(grafo_tbl, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = centralidad, color = as.factor(componente))) +
  geom_node_text(aes(label = name, color = as.factor(componente)), repel = TRUE, size = 3) +
  theme_void() +
  ggtitle("Componentes fuertemente conexos")
grid.arrange(p1, p2, ncol = 2)

# 7. Uso de ERGM en la red de amistad de 2024 ------------

# Si tu grafo es tidygraph
library(intergraph)
grafo_panas2 <- aristas_y_total |>
  graph_from_data_frame(directed = TRUE)
net_ergm <- asNetwork(grafo_panas2)
library(statnet)
net_ergm
list.vertex.attributes(net_ergm)
# Convertimos a lógico (TRUE para miembros, FALSE para no)
bloques_amistad_2005_2024 <- bloques_amistad_2005_2024 |>
  mutate(miembro_OTAN_log = miembro_OTAN == "Sí")
set.vertex.attribute(net_ergm, "miembro_OTAN", bloques_amistad_2005_2024$miembro_OTAN_log)
set.vertex.attribute(net_ergm, "bloques", bloques_amistad_2005_2024$bloque)
bloques_amistad_2005_2024$miembro_OTAN_log
list.vertex.attributes(net_ergm)
orden_nodos <- network.vertex.names(net_ergm)
atrib_otan <- bloques_amistad_2005_2024$miembro_OTAN_log[match(orden_nodos, bloques_amistad_2005_2024$name)]
set.vertex.attribute(net_ergm, "miembro_OTAN", atrib_otan)
#maximum pseudo likelihood
ergm2024<-ergm(net_ergm ~ edges + mutual + nodematch("miembro_OTAN")+ nodematch("bloques"))
summary(ergm2024)

mcmc.diagnostics(ergm2024)

gof_result <- gof(ergm2024)
plot(gof_result)

