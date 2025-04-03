#R y encuestas
# Instalar y cargar paquetes necesarios
# install.packages("tidyverse")  # Ejecuta esto si no tienes instalado tidyverse
library(tidyverse) # Para manipulación de datos
library(readxl)    # Para leer archivos Excel
library(stargazer) # Para generar tablas en LaTeX
library(googlesheets4) # Para trabajar con Google Sheets

# 1. Importar los datos
# Reemplaza 'Encuesta economía Río Piedras (respuestas).xlsx' con la ruta correcta a tu archivo de datos
#datos <- read_excel("Encuesta economía Río Piedras (respuestas).xlsx")
#datos <- read_excel("Encuesta Río Piedras SOCI 4186.xlsx")

#url_hoja <- "https://docs.google.com/spreadsheets/d/1LyBdyW-TTw-Gu6R5hJTcaTJdzuRc66KmjlgAqHw8ieQ/edit?resourcekey=&gid=1916206010#gid=1916206010"
#datos <- read_sheet(url_hoja) 

datos<-read_csv(dir/subdir/localización_de_hoja.csv)

summary(datos)

names(datos)
# 3. Renombrar columnas para facilitar su manejo
datos <- datos |>
  rename(
    consentimiento= `Hoja de Consentimiento Informado\n\nTítulo de la encuesta: Encuesta de economía en Río Piedras\n\nInvestigador Principal: Profesor Rashid Marcano Rivera, Catedrático Auxiliar, Departamento de Sociología y Antropología, Universidad de Puerto Rico, Recinto de Río Piedras (UPRRP)\n\nDescripción del estudio: Este estudio busca analizar las percepciones económicas de los residentes de Río Piedras a través de una encuesta digital. El propósito de la investigación es comprender las tendencias y comportamientos económicos locales.\n\nParticipación Voluntaria: Su participación es completamente voluntaria. Usted tiene el derecho de no contestar cualquier pregunta y puede retirarse del estudio en cualquier momento sin penalidades.\n\nDuración: La encuesta tomará aproximadamente 3 a 5 minutos.\n\nRiesgos y Beneficios: La participación en esta encuesta digital conlleva riesgos mínimos, como la incomodidad leve o cansancio por responder preguntas. No se anticipan riesgos mayores ni costos adicionales significativos asociados con la participación. No se prevén beneficios directos para los participantes; sin embargo, los resultados contribuirán al desarrollo de políticas y acciones económicas informadas para la región.\n\nConfidencialidad: Toda la información que usted proporcione será tratada de manera confidencial. Los datos se utilizarán únicamente para fines de investigación y serán almacenados en un sistema protegido digitalmente. Solo el investigador principal y el personal autorizado tendrán acceso a estos datos.\n\nContacto:\n\nSi tiene alguna pregunta sobre el estudio o sus derechos como participante, puede comunicarse con el Profesor Rashid Marcano Rivera a través del siguiente correo electrónico: [rashid.marcano@upr.edu] o el Departamento de Sociología y Antropología de la UPRRP. \n\nConsentimiento:\n\nAl hacer clic en “Aceptar” y apretar en Siguiente, usted consiente de manera voluntaria a participar en esta encuesta digital y acepta que ha leído y comprendido la información provista anteriormente.`,
    sexo = `¿Cuál es su sexo?`,
    edad = `¿Cuál es tu grupo de edad?`,
    region = `¿En qué región de Puerto Rico vive?`,
    ocupacion = `¿A qué se dedica? (puede escoger más de una opción)`,
    ingreso_anual = `¿Cuál es el ingreso anual promedio aproximado de su hogar?`,
    tipo_escuela = `¿Es egresado de qué tipo de sistema de educación secundario?`,
    descr_región = `En pocas palabras, ¿qué área geográfica considera usted que forma parte de Río Piedras?`,
    atractivo_compras = `¿Cree que Río Piedras se ve atractivo para ir de compras?`,
    paseo_de_diego = `¿Frecuenta el Paseo de Diego para hacer compras?`,
    plaza_mercado = `¿Ha ido a la Plaza del Mercado de Río Piedras?`,
    atractivo_jangueo = `¿Cree que Río Piedras se ve atractivo para janguear con amistades?`,
    jangueo_actividades = `¿Ha jangueado en Río Piedras de noche?`,
    negocios_jangueo = `Seleccione negocios que ha frecuentado para jangueo en el área (puede seleccionar varios).`,
    limpieza = `Calificando de la A a la F, ¿cómo entiende que es el casco de Río Piedras en los siguientes renglones? [Limpieza]`,
    ornato = `Calificando de la A a la F, ¿cómo entiende que es el casco de Río Piedras en los siguientes renglones? [Ornato]`,
    seguridad = `Calificando de la A a la F, ¿cómo entiende que es el casco de Río Piedras en los siguientes renglones? [Seguridad]`,
    opciones_compra = `Calificando de la A a la F, ¿cómo entiende que es el casco de Río Piedras en los siguientes renglones? [Opciones de compra]`,
    oferta_gastronomica = `Calificando de la A a la F, ¿cómo entiende que es el casco de Río Piedras en los siguientes renglones? [Oferta gastronómica]`,
    vida_nocturna = `Calificando de la A a la F, ¿cómo entiende que es el casco de Río Piedras en los siguientes renglones? [Vida nocturna]`,
    vista_general = `¿Cómo se ve el casco urbano de Río Piedras en conjunto?`,
    comparacion_anual = `¿Cree que Río Piedras se ve mejor, igual, o peor que hace un año?`,
    establecimientos = `¿En Río Piedras, cuál de los siguientes establecimientos gusta frecuentar?`,
    redujo_gastos = `¿Ha tenido que reducir sus gastos en los últimos 12 meses debido a la inflación?`,
    responsabilidad = `¿A quién le corresponde tener a Río Piedras en buen estado?`,
    comentarios = `Gracias por participar. Si tiene comentarios no dude en dejarme saber rellenando la casilla de abajo.`
  )
datos <- datos |>
  mutate(id_respuesta = row_number())
datos
#Eliminar fila si en Hoja de Consentimiento dijeron 'Declino'
datos <- subset(datos, consentimiento != 'Declino')

# 2. Eliminar columnas innecesarias
# Eliminamos 'Hoja de Consentimiento', 'Puntuación' y 'Marca temporal'
#datos <- datos |>
#  select(-c(`Hoja de Consentimiento`, `Puntuación`, `Marca temporal`))


# 4. Convertir variables categóricas a factores
datos <- datos |>
  mutate_at(vars(sexo, edad, region, ingreso_anual, atractivo_compras, 
                 tipo_escuela, atractivo_jangueo,paseo_de_diego, plaza_mercado,
                 limpieza, ornato, seguridad, opciones_compra, 
                 oferta_gastronomica, establecimientos, jangueo_actividades,
                 vida_nocturna, vista_general, comparacion_anual, 
                 redujo_gastos, responsabilidad,negocios_jangueo), as.factor)
datos|>select(establecimientos,negocios_jangueo)


#quitar acentos en establecimientos y negocios_jangueo para simplificar análisis
library(scraEP)
datos$establecimientos <- unaccent(datos$establecimientos)
datos$negocios_jangueo <- unaccent(datos$negocios_jangueo)
datos <- datos |>
  mutate(across(
    c(establecimientos, negocios_jangueo),
    ~ as.factor(trimws(tolower(.)))
  ))
#datos$establecimientos <- tolower(trimws(datos$establecimientos))
#datos$negocios_jangueo <- tolower(trimws(datos$negocios_jangueo))
summary(datos$negocios_jangueo)
# Ver datos con nombres normalizados
print(datos)
# 5. Manejar respuestas múltiples en 'ocupacion', 'responsabilidad' y 'establecimientos'
# Separar las respuestas múltiples en filas individuales
datos_ocupacion <- datos |>
  separate_rows(ocupacion, sep = ",\\s*")
#datos |> 
#  separate_rows(ocupacion, sep = ",\\s*") |>
#  select(ocupacion) 
datos_responsabilidad <- datos |>
  separate_rows(responsabilidad, sep = ",\\s*")

datos_jangueo_neg <- datos |>
  mutate(
    negocios_jangueo = tolower(negocios_jangueo),
    negocios_jangueo = stringi::stri_trans_general(negocios_jangueo, "Latin-ASCII"),
    negocios_jangueo = str_replace_all(negocios_jangueo, "[,\n\t\\u00A0]+", ","),
    negocios_jangueo = str_replace_all(negocios_jangueo, ",+", ","),
    negocios_jangueo = str_replace_all(negocios_jangueo, ",\\s*", ", "),
    negocios_jangueo = str_replace_all(negocios_jangueo, "cafe boriken,", "cafe boriken"),
    negocios_jangueo = str_replace_all(negocios_jangueo, " y ", ", "),
    negocios_jangueo = str_replace_all(negocios_jangueo, "scryer/el lab", "scryer, el lab"),
    negocios_jangueo = str_replace_all(negocios_jangueo, "mr mac", "mr. mac"),
    negocios_jangueo = str_replace_all(negocios_jangueo, "\\bboriken\\b", "cafe boriken"),
    negocios_jangueo = str_replace_all(negocios_jangueo, "cafe boriken plaza del mercado", "cafe boriken, plaza del mercado"),
    negocios_jangueo = str_replace_all(negocios_jangueo, "scyther", "scryer"),
    negocios_jangueo = str_replace_all(negocios_jangueo, "el 8", "el ocho"),
    negocios_jangueo = str_replace_all(negocios_jangueo, "el ocho de rio", "el ocho"),
    negocios_jangueo = str_replace_all(negocios_jangueo, "el vidy s", "el vidy's"),
    negocios_jangueo = str_replace_all(negocios_jangueo, "dipea/la lata", "dipea"),
    negocios_jangueo = str_squish(negocios_jangueo),
    negocios_jangueo = trimws(negocios_jangueo),
    negocios_jangueo = ifelse(negocios_jangueo == "n/a", NA, negocios_jangueo)
  ) |>
  separate_rows(negocios_jangueo, sep = ", ") |>
  mutate(
    negocios_jangueo = str_replace_all(negocios_jangueo, "cafe cafe boriken", "cafe boriken"),
  )

#View(datos_jangueo_neg)

datos_establecimientos <- datos |>
  mutate(
    establecimientos = str_replace_all(establecimientos, "[,\n\t\\u00A0]+", ","),
    establecimientos = str_replace_all(establecimientos, ",+", ","),
    establecimientos = str_replace_all(establecimientos, ",\\s*", ", "),
    establecimientos = str_replace_all(establecimientos, "el 8", "el ocho"),
    establecimientos = str_replace_all(establecimientos, "el ocho de rio", "el ocho"),
    establecimientos = str_replace_all(establecimientos, "el vidy s", "el vidy's"),
    establecimientos = str_replace_all(establecimientos, "scryer/el lab", "scryer, el lab"),
    establecimientos = str_replace_all(establecimientos, "mr mac", "mr. mac"),
#    establecimientos = str_replace_all(establecimientos, "boriken", "cafe boriken"),
    establecimientos = str_replace_all(establecimientos, "rancho mexicano en plaza uni", "rancho mexicano"),
    establecimientos = str_replace_all(establecimientos, "cafe boriken plaza del mercado", "cafe boriken, plaza del mercado"),
    establecimientos = str_replace_all(establecimientos, "dipea/la lata", "dipea"),
    establecimientos = str_replace_all(establecimientos, "scryer/el lab", "scryer, el lab"),
    establecimientos = str_replace_all(establecimientos, "cafe boriken,", "cafe boriken"),
    establecimientos = str_replace_all(establecimientos, " y ", ", "),
    establecimientos = str_squish(establecimientos),
    establecimientos = trimws(establecimientos),
  ) |>
  separate_rows(establecimientos, sep = ",\\s*") |>
  mutate(establecimientos = str_replace_all(establecimientos, "cafe boriken plaza del mercado", "cafe boriken, plaza del mercado"),)|>  separate_rows(establecimientos, sep = ",\\s*")



tibble(
  nombre = c(datos_jangueo_neg$negocios_jangueo, datos_establecimientos$establecimientos)
) |>
  filter(!is.na(nombre), nombre != "") |>
  count(nombre, sort = FALSE) |>
  arrange(nombre) |> print(n=27)

# Crear variables binarias para cada ocupación
datos_ocupacion_bin <- datos_ocupacion |>
  select(id_respuesta, ocupacion) |>
  mutate(valor = 1) |>
  pivot_wider(names_from = ocupacion, values_from = valor, values_fill = list(valor = 0))
names(datos_ocupacion_bin)
datos_ocupacion_bin
 #datos_ocupacion |>
#  mutate(valor = 1) |>
#  pivot_wider(names_from = ocupacion, values_from = valor, values_fill = list(valor = 0)) |>
#  select(sexo,edad,)

# Crear variables binarias para cada responsabilidad
datos_responsabilidad_bin <- datos_responsabilidad |>
  select(id_respuesta, responsabilidad) |>
  mutate(valor = 1) |>
  pivot_wider(names_from = responsabilidad, values_from = valor, values_fill = list(valor = 0))

# Crear variables binarias para cada establecimiento
datos_establecimientos_bin <- datos_establecimientos |>
  select(id_respuesta, establecimientos) |>
  mutate(valor = 1) |>
  pivot_wider(names_from = establecimientos, values_from = valor, values_fill = list(valor = 0))

# Crear variables binarias para cada establecimiento de jangueo

datos_jangueo_neg |>
  mutate(valor = 1)|>
  pivot_wider(names_from = negocios_jangueo, values_from = valor, values_fill = list(valor = 0)) 

datos_jangueo_neg_bin <- datos_jangueo_neg |>
  select(id_respuesta, negocios_jangueo) |>
  mutate(
    negocios_jangueo = tolower(negocios_jangueo),
    negocios_jangueo = str_replace_all(negocios_jangueo, "[,\n\t\\u00A0]+", ","),  # limpieza de separadores raros
    negocios_jangueo = str_replace_all(negocios_jangueo, ",+", ","),
    negocios_jangueo = str_replace_all(negocios_jangueo, ",\\s*", ", "),
    negocios_jangueo = str_squish(negocios_jangueo),
    negocios_jangueo = str_trim(negocios_jangueo),
    negocios_jangueo = ifelse(is.na(negocios_jangueo) | negocios_jangueo == "", "n/a", negocios_jangueo)
  ) |>
  separate_rows(negocios_jangueo, sep = ", ") |>  # ¡Aquí se expande!
  mutate(valor = 1) |>
  pivot_wider(
    names_from = negocios_jangueo,
    values_from = valor,
    values_fill = list(valor = 0)
  )
datos_jangueo_neg_bin
datos <- datos |>
  left_join(datos_ocupacion_bin, by = "id_respuesta") |>
  left_join(datos_responsabilidad_bin, by = "id_respuesta") |>
  left_join(datos_establecimientos_bin, by = "id_respuesta") |>
  left_join(datos_jangueo_neg_bin, by = "id_respuesta") 


# 6. Recodificar y estandarizar categorías
# Unificar respuestas similares en 'region'
datos <- datos |>
  mutate(region = recode(region,
                         "San Juan, Santurce o Río Piedras" = "San Juan",
                         "Área metropolitana, excepto municipio de San Juan" = "Área metropolitana",
                         "Fuera del área metropolitana de San Juan" = "Fuera del área metropolitana"))

# 7. Manejar valores faltantes y duplicados
# Eliminar filas duplicadas
datos <- datos |>
  distinct()

datos |>
  separate_rows(negocios_jangueo, sep = ",\\s*") |>
  mutate(negocios_jangueo = tolower(negocios_jangueo)) |>
  count(negocios_jangueo) |>
  filter(str_detect(negocios_jangueo, "boriken"))

datos |>
  select(id_respuesta,negocios_jangueo,matches("\\.x$|\\.y$"))
#datos|>
#  select(id_respuesta,negocios_jangueo,`el refugio`,matches("\\.x$|\\.y$")) 

datos <- datos |>
  select(-matches("\\.y$"), -any_of("NA")) |>
  rename_with(~str_remove(., "\\.x$"), matches("\\.x$"))

# 8. Análisis exploratorio inicial
# Resumen estadístico
summary(datos)
View(datos)

# 9. Guardar el conjunto de datos limpio
write_csv(datos, "encuesta_limpia.csv")

