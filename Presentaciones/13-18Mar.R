#Empezamos cargando paquetes que usaremos hoy
library(dslabs)
data(murders)
head(murders)
library(tidyverse)
library(dslabs)

# Usaré datos modificados para la sesión
wd<-getwd()
read_lines("murders.csv",n_max=4)
objeton<-file.path(wd, "murders.csv") #podriamos usar la dirección entera
murders<-read_csv(objeton)
#quitar la columna ...1 con tidyverse (dplyr)
murders <- murders |> select(-`...1`)

#visualización general
#primero prepararé la información tal cuál la teníamos (final de MiSesión2.R)

#Componentes de gráficos:
# 1. datos
murders|>ggplot()
# 2. geometrías
# 3. mapeo estético (representación estética en el mapa)
#en ggplot esto se pasa en varios argumentos usando la gramática de gráficos

murdersg<-murders|>
  mutate(grupo = case_when(
    abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "Nueva Inglaterra",
    abb %in% c("WA", "OR", "CA") ~ "Costa del Pacífico",
    region == "South" ~ "el Sur",
    TRUE ~ "Otras regiones"))|>
  group_by(grupo)|>
  summarise(tasa_100k = sum(total)/ sum(population) * 10^5)
#gráficas circulares (y porqué son subóptimas en general)
pie(murdersg$tasa_100k)

murdersg
pie(murdersg$tasa_100k, labels = c("Costa del Pacífico (¿?)","Nueva Inglaterra","Otras regiones","el Sur (¿?)"))

murders|>
  group_by(region)|>
  summarise(tasa_100k_reg = sum(total) / sum(population) * 10^5)|>
  ggplot(aes(x = region, y = tasa_100k_reg)) +
  geom_col()

murdersg |>
  ggplot() +
  geom_col(aes(x=grupo, y=tasa_100k))

murders |>
  ggplot()+
  geom_col(aes(x=tasa,y=region))

murders|> ggplot() + geom_point(aes(x = population/10^6, y = total))

p<-murders|>
  ggplot() +
  geom_point(aes(x = population/10^6, y = total),size=2)

p

p+
  geom_text(aes(population/10^6, total, label = abb))

p+
  geom_text(aes(population/10^6, total, label = abb),nudge_x = 1.5)
murders<-murders|>mutate(poblacion_10millones=population/10^7)
murders
# gráfico con tamaño según población
p <- murders |> 
  ggplot(aes(x = population / 10^6, 
             y = total, 
             colour = region,
             size = tasa)) +  # aquí mapeas el tamaño
  geom_point(alpha = 0.7) +  # transparencia para mejor visibilidad
  labs(x = "Población (millones)",
       y = "Total de asesinatos",
       colour = "Región",
       size = "Tasa homicidios)",
       title = "Total de asesinatos por población y región en EE.UU.") +
  theme_minimal()

# Mostrar gráfico
p

murders |> ggplot() +
  geom_point(aes(x = population / 10^6, y = total, colour = region, size = tasa))



p <- murders |>
  ggplot() +
  geom_point(aes(x = population / 10^6, y = total, size = population / 10^7),
             alpha = 0.7) +
  labs(x = "Población (millones)",
       y = "Total de asesinatos",
       size = "Población (10 millones)",
       title = "Total de asesinatos según población estatal")
p
# Crear el gráfico base con puntos coloreados por región
p <- murders|>
  ggplot() +
  geom_point(aes(x = population/10^6, y = total, colour = region), size = 2)

# Mostrar el gráfico
p

# Añadir etiquetas (con color por región)
p + 
  geom_text(aes(x = population/10^6, y = total, label = abb, colour = region))

# Añadir etiquetas desplazadas en el eje x (con color por región)
p<-p + 
  geom_text(aes(x = population/10^6, y = total, label = abb, colour = region), nudge_x = 1.5)
p

#démosle más información a la gráfica de dispersión.

p+labs(
  x = "Población (millones)",   # Cambia el nombre del eje x
  y = "Homicidios por arma de fuego",            # Cambia el nombre del eje y
  colour = "Región",              # Cambia el nombre de la leyenda de color
  title = "Homicidios por arma de fuego vs población, por región"  # Título del gráfico
)

#quizás esta forma no haya sido tan informativa... ¿qué tal si aplicamos una transformación logarítmica?
p2<-murders|>
  ggplot() +
  geom_point(aes(x = log(population/10^6), y = log(total), colour = region), size = 2)
p2
p2+geom_text(aes(x = log(population/10^6), y = log(total), label = abb), nudge_x = 0.3)+
  labs(
    x = "Población (millones, escala log.)",   # Cambia el nombre del eje x
    y = "Homicidios por arma de fuego (escala log.)",            # Cambia el nombre del eje y
    colour = "Región",              # Cambia el nombre de la leyenda de color
    title = "Homicidios por arma de fuego vs población, por región"  # Título del gráfico
  )

#de hecho siguiendo el libro, ggplot está acostumbrado a esta transformación y 
#ofrece sus funciones:
p3<-murders|>
  ggplot() +
  geom_point(aes(x = population/10^6, y = total, colour = region), size = 3)
p3<- p3 + geom_text(aes(x = population/10^6, y = total, label = abb),nudge_x = 0.05)
p3
p3<-p3+scale_x_continuous(trans = "log10")+
  scale_y_continuous(transform = "log10")
p3<-p3+
  labs(
    x = "Población (millones, escala log.)",   # Cambia el nombre del eje x
    y = "Homicidios por arma de fuego (escala log.)",            # Cambia el nombre del eje y
    colour = "Región",              # Cambia el nombre de la leyenda de color
    title = "Homicidios por arma de fuego vs población, por región"  # Título del gráfico
  )

library(ggthemes)
p3 + theme_economist()
#varios themes disponibles.

#todo junto

library(ggthemes)
library(ggrepel)
#library(dslabs)
#data(murders)
t <- murders |> 
  summarise(tasa = sum(total) /  sum(population) * 10^6) |>
  pull(tasa)

murders |> 
  ggplot(aes(population/10^6, total,size = tasa)) +   
  geom_abline(intercept = log10(t), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region)) +
  geom_text_repel(aes(label = abb)) + 
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Homicidios por arma de fuego vs población, por región, 2010",
       x = "Población (millones, escala log.)", 
       y = "Homicidios por arma de fuego (escala log.)",
       color = "Región") +
  theme_economist()
#ejemplos adicionales, y el quickplot
#data(murders)
x <- log10(murders$population)
y <- murders$total

data.frame(x = x, y = y) |>
  ggplot(aes(x, y)) +
  geom_point()

qplot(x, y)
murdersg<-murders|>
  mutate(tasa= total/population *10^5,
         ,grupo = case_when(
           abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "Nueva Inglaterra",
           abb %in% c("WA", "OR", "CA") ~ "Costa del Pacífico",
           region == "South" ~ "el Sur",
           TRUE ~ "Otras regiones"))
qplot(tasa, data = murdersg, geom= "density", fill = grupo, linetype=grupo, alpha=0.7)

a <- murdersg|>
  ggplot(aes(x = tasa))
a 
a+geom_area(stat="bin")
a+ geom_density()
a+ geom_density(aes(color = grupo)) 
a+ geom_density(aes(fill = grupo),alpha=0.4) 


mu<-murdersg|>
  group_by(grupo)|>
  summarise(mediagrupal=mean(tasa))
head(mu)

a+ geom_density(aes(color = grupo)) +
  geom_vline(data=mu, aes(xintercept=mediagrupal, color=grupo),
             linetype="dashed") +
  scale_color_manual(values=c("#999999", "#E69F00","skyblue","red3")) 

#data(heights)     
heights <- read_csv("heights.csv")
#salir de columna `...1`
heights <- heights |> select(-`...1`)
mu_alt<-heights|>
  group_by(género)|>
  summarise(media=mean(alturacm))
b<-heights|>ggplot()
qplot(género, alturacm, data = heights, geom= "boxplot", fill = género)
qplot(género, alturacm, data = heights, geom= "violin", fill = género)
qplot(género, alturacm, data = heights, geom = "dotplot",
      stackdir = "center", binaxis = "y", dotsize = 0.3)
qplot(alturacm, data = heights, geom = "density", fill = sex)
qplot(alturacm, data = heights, geom = "density", color = sex, linetype = sex)
b
b+geom_density(aes(x=alturacm,colour = género)) 
b+geom_density(aes(x=alturacm,fill = género), alpha=0.4)
mu_alt
c<-heights|>ggplot(aes(x = alturacm))
c+geom_density(aes(fill = género), alpha=0.4)
c+ geom_density(aes(color = género)) +
  geom_vline(data=mu_alt, aes(xintercept=media, color=género),
             linetype="dashed") +
  scale_color_manual(values=c("#E69F00", "#999999")) 
