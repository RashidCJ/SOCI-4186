
# Create the graph from the summarized edge list
library(igraph)

# Crear el objeto de red dirigido desde la tabla de aristas
g_red <- graph_from_data_frame(
  d = aristas_summary,
  vertices = data.frame(nombre = unique(c(aristas$from, aristas$to))),
  directed = TRUE
)
g_red
# Asignar los votos agregados como atributo de peso

# Calcular el in-degree ponderado (suma de votos recibidos por cada país)
V(g_red)$grado_entrada <- strength(g_red, mode = "in", weights = E(g_red)$votos)

# Ver países ordenados por votos recibidos (mayor a menor)
sort(V(g_red)$grado_entrada, decreasing = TRUE)

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

# (Opcional) Calcular grado de entrada y salida para cada país
grado_entrada  <- strength(g_red, mode = "in", weights = E(g_red)$peso)
grado_salida   <- strength(g_red, mode = "out", weights = E(g_red)$peso)

# Fuerza (suma ponderada) de entrada y salida
fuerza_entrada  <- strength(g_red, mode = "in", weights = E(g_red)$peso)
fuerza_salida   <- strength(g_red, mode = "out", weights = E(g_red)$peso)

# Centralidad de intermediación (betweenness)
intermediacion <- betweenness(g_red, directed = TRUE, weights = E(g_red)$peso)
intermediacion
# Cercanía (closeness) de entrada y salida
cercania_entrada  <- closeness(g_red, mode = "in")
cercania_salida   <- closeness(g_red, mode = "out")

# Centralidad de autovector (eigenvector)
autovector <- eigen_centrality(g_red, directed = TRUE, weights = E(g_red)$peso)$vector

# PageRank, útil en redes dirigidas
pagerank <- page_rank(g_red, directed = TRUE, weights = E(g_red)$peso)$vector

# Densidad de la red
densidad <- edge_density(g_red)

# Reciprocidad (proporción de aristas bidireccionales)
reciprocidad <- reciprocity(g_red)

# Detección de comunidades usando el algoritmo Infomap
comunidades <- cluster_infomap(g_red)
comunidades
# Información resumida de las comunidades detectadas
summary(comunidades)

comunidades_louvain <- cluster_louvain(as.undirected(g_red))
comunidades_louvain
plot(comunidades_louvain, g_red)
plot(
  g_red,
  vertex.color = membership(comunidades_louvain),
  vertex.size = 10,
  vertex.label.cex = 0.8,
  edge.width = E(g_red)$peso / 4,
  layout = layout_with_fr
)
print(membership(comunidades_louvain))

# Número de comunidades encontradas
num_comunidades <- length(unique(membership(comunidades)))
num_comunidades_L <- length(unique(membership(comunidades_louvain)))

cat("Número de comunidades detectadas en cluster infomap:", num_comunidades, "\n")
cat("Número de comunidades detectadas en algoritmo Louvain:", num_comunidades_L, "\n")
