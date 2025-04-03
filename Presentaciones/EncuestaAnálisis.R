library(tidyverse)
datos<-read_csv("encuesta_limpia.csv")
# Tablas de frecuencia para variables categóricas
table(datos$sexo)
table(datos$region)
table(datos$edad)
head(datos)
names(datos)
# 1. Visualización de datos
# Gráfico de barras de sexo
ggplot(datos, aes(x = sexo)) +
  geom_bar(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Distribución por Sexo", x = "Sexo", y = "Cantidad")


# 2. Tablas de frecuencias y porcentajes de las relaciones entre nuestros datos

# Relación entre 'sexo' y 'redujo_gastos'
tabla_sexo_gastos <- table(datos$sexo, datos$redujo_gastos)
print(tabla_sexo_gastos)

tabla_iupicoop_S<-table(datos$sexo, datos$iupicoop)
print(tabla_iupicoop_S)

tabla_calificaRP_negocio<-table(datos$seguridad, datos$`el boricua`)
print(tabla_calificaRP_negocio)


# Calcular porcentajes por fila
porcentaje_sexo_gastos <- prop.table(tabla_sexo_gastos, margin = 1) * 100
porcentaje_iupicoop_S<- prop.table(tabla_iupicoop_S, margin = 1) * 100
porcentaje_calificaRP_negocio <- prop.table(tabla_calificaRP_negocio, margin = 1) * 100
print(round(porcentaje_sexo_gastos, 2))
print(round(porcentaje_iupicoop_S, 3))
print(round(porcentaje_calificaRP_negocio, 3))

tabla_sexo_ingreso <- table(datos$sexo, datos$ingreso_anual)
print(tabla_sexo_ingreso)

# Gráfico de barras apiladas para visualizar la relación
ggplot(datos, aes(x = sexo, fill = redujo_gastos)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Relación entre Sexo y Reducción de Gastos",
       x = "Sexo",
       y = "Porcentaje",
       fill = "¿Redujo Gastos?") +
  theme_minimal()

# Gráfico de barras apiladas para visualizar la con notas a RP en seguridad e ir al Bori
ggplot(datos, aes(x = factor(`el boricua`, levels = c(1, 0), labels = c("Frecuenta", "No frecuenta")),
                  fill = seguridad)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Relación entre percepción de seguridad y asistir a negocio",
       x = "Visita a «El Boricua»",
       y = "Porcentaje",
       fill = "Calificación de seguridad del casco riopedrense") +
  theme_minimal()

datos |>
  mutate(asiste_bori = factor(`el boricua`, levels = c(1, 0), labels = c("Frecuenta", "No frecuenta"))) |>
  count(asiste_bori, seguridad) |>
  group_by(asiste_bori) |>
  mutate(porcentaje = n / sum(n)) |>
  ggplot(aes(x = asiste_bori, y = porcentaje, fill = seguridad)) +
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(porcentaje)),
            position = position_fill(vjust = 0.5),
            colour = "white", size = 3.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Relación entre percepción de seguridad y asistir a «El Boricua»",
       x = "Visita a «El Boricua»",
       y = "Porcentaje",
       fill = "Calificación de seguridad del casco riopedrense") +
  theme_minimal()

tabla_calificaRP_negocio<-table(datos$seguridad, datos$`el boricua`)

# Relación entre 'edad' y 'atractivo_compras'
tabla_edad_atractivo <- table(datos$edad, datos$atractivo_compras)
print(tabla_edad_atractivo)

# Porcentajes por fila
porcentaje_edad_atractivo <- prop.table(tabla_edad_atractivo, margin = 1) * 100
print(round(porcentaje_edad_atractivo, 2))

# Gráfico de barras apiladas
ggplot(datos, aes(x = edad, fill = atractivo_compras)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Relación entre Edad y atractivo de Río Piedras para ir de compras",
       x = "Grupo de Edad",
       y = "Porcentaje",
       fill = "Atractividad para compras") +
  theme_minimal()

# 12. Pruebas de Ji cuadrado para analizar relaciones entre variables
?chisq.test
# Prueba entre 'sexo' y 'redujo_gastos'
tabla_sexo_gastos
chi_sexo_gastos <- chisq.test(tabla_sexo_gastos)
print(chi_sexo_gastos)

# Verificar si se cumplen las condiciones para la prueba de Ji cuadrado
if(any(chi_sexo_gastos$expected < 5)) {
  # Si alguna frecuencia esperada es menor que 5, usar la prueba exacta de Fisher
  fisher_sexo_gastos <- fisher.test(tabla_sexo_gastos)
  print(fisher_sexo_gastos)
}

# Prueba entre 'edad' y 'atractivo_compras'
chi_edad_atractivo <- chisq.test(tabla_edad_atractivo)
print(chi_edad_atractivo)

if(any(chi_edad_atractivo$expected < 5)) {
  fisher_edad_atractivo <- fisher.test(tabla_edad_atractivo)
  print(fisher_edad_atractivo)
}

tabla_calificaRP_negocio
ji_calificaRP_negocio <- chisq.test(tabla_calificaRP_negocio)
ji_calificaRP_negocio

# Prueba entre dummies creadas y respuestas
# Por ejemplo, 'Estudiante universitario' y 'redujo_gastos'

# Asegurarse de que la variable dummy exista y esté en formato factor
if("Estudiante universitario" %in% names(datos)) {
  datos$`Estudiante universitario` <- as.factor(datos$`Estudiante universitario`)
  
  tabla_estudiante_gastos <- table(datos$`Estudiante universitario`, datos$redujo_gastos)
  print(tabla_estudiante_gastos)
  
  chi_estudiante_gastos <- chisq.test(tabla_estudiante_gastos)
  print(chi_estudiante_gastos)
  
  if(any(chi_estudiante_gastos$expected < 5)) {
    fisher_estudiante_gastos <- fisher.test(tabla_estudiante_gastos)
    print(fisher_estudiante_gastos)
  }
}

# 13. Usar stargazer para generar tablas para LaTeX
summary(datos)
library(stargazer)
# Generar una tabla resumen de estadísticas descriptivas
stargazer(datos |> select(where(is.numeric)), type = "text", summary = TRUE)
#alternativa
library(skimr)
skim(datos)
#alternativaB
library(summarytools)
view(dfSummary(datos))
# Generar tablas de contingencia en formato LaTeX
# Convertir tablas a data frames para stargazer
df_tabla_sexo_gastos <- as.data.frame.matrix(tabla_sexo_gastos)
df_tabla_edad_atractivo <- as.data.frame.matrix(tabla_edad_atractivo)

# Tabla de 'sexo' y 'redujo_gastos'
stargazer(df_tabla_sexo_gastos, type = "latex", summary = FALSE,
          title = "Tabla de Contingencia: Sexo vs Reducción de Gastos",
          label = "tab:sexo_gastos")

# Tabla de 'edad' y 'atractivo_compras'
stargazer(df_tabla_edad_atractivo, type = "latex", summary = FALSE,
          title = "Tabla de Contingencia: Edad vs Atractivo para Compras",
          label = "tab:edad_atractivo")

# Generar resultados de pruebas de Ji cuadrado en formato LaTeX
# Crear un data frame con los resultados
resultados_chi <- data.frame(
  Prueba = c("Sexo vs Reducción de Gastos", "Edad vs Atractivo para Compras"),
  Estadístico = c(chi_sexo_gastos$statistic, chi_edad_atractivo$statistic),
  Grados_de_Libertad = c(chi_sexo_gastos$parameter, chi_edad_atractivo$parameter),
  Valor_p = c(chi_sexo_gastos$p.value, chi_edad_atractivo$p.value)
)

# Generar tabla con stargazer
stargazer(resultados_chi, type = "latex", summary = FALSE,
          title = "Resultados de Pruebas de Ji Cuadrado",
          label = "tab:resultados_chi",
          digits = 4)

# Guardar tablas en archivos .tex si es necesario
# Por ejemplo:
stargazer(df_tabla_sexo_gastos, type = "latex", summary = FALSE,
          title = "Tabla de Contingencia: Sexo vs Reducción de Gastos",
          label = "tab:sexo_gastos",
          out = "tabla_sexo_gastos.tex")

# Puedes hacer lo mismo para otras tablas y resultados.

objetoreg<-glm(`el boricua` ~ seguridad + tipo_escuela + ingreso_anual, 
    data = datos, family = "binomial")
car::vif(objetoreg)
summary(objetoreg)
lm(vista_general ~ limpieza + ornato + seguridad + plaza_mercado + ingreso_anual, 
   data = datos)
glm(redujo_gastos ~ ingreso_anual + sexo + edad + plaza_mercado + opciones_compra, 
    data = datos, family = "binomial")
