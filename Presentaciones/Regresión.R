tab <- matrix(c(4,0,0,4),2,2) #¿qué pasa al cambiar los valores?
rownames(tab)<-c("Leche servida antes","Leche servida después")
colnames(tab)<-c("Adivinó «antes»","Adivinó «después»")
tab
fisher.test(tab, alternative="greater")$p.value

#>   

#Regresión
library(tidyverse)
url<-"https://inductivestep.github.io/R-notes/prestige.csv"
dat<-read_csv(url)
dat

sample(dat)
summary(dat)

library(tableone)
library(stargazer)

dat |>
  select(-occ) |>
  CreateTableOne(data = _) 

# Supongamos que 'dat' es tu data frame de 102 filas
# Crear la columna 'prob' asignando la probabilidad a cada fila según su 'type'
dat$prob <- with(dat, ifelse(type == "bc", 0.449/44,
                             ifelse(type == "prof", 0.316/31,
                                    ifelse(type == "wc", 0.235/23, NA))))
dat$prob
dat_m<-na.omit(dat)
# Verificamos que la suma de probabilidades sea 1 (debería cumplirse):
sum(dat_m$prob)  # Esto debería dar 1

# Muestrear 10 filas manteniendo las probabilidades
set.seed(123)  # Para reproducibilidad
muestra_10 <- dat_m[sample(1:nrow(dat_m), size = 10, replace = TRUE, prob = dat_m$prob), ]
print(muestra_10)


dat |>
  ggplot(aes(x = education, y = prestige)) +
  geom_point() +
  labs(x = "Años de educación media",
       y = "Prestigio Pineo-Porter ocupacional")
cor(dat$education,dat$prestige)
dat |>
  ggplot(aes(x = income, y = prestige)) +
  geom_point() +
  labs(x = "Ingreso medio (dólares canadienses)",
       y = "Prestigio Pineo-Porter ocupacional")
cor(dat$prestige,dat$income)

mean(dat$prestige)
sd(dat$prestige)

library(GGally)
#matriz de correlación
ggpairs(dat,columns=2:6,ggplot2::aes(colour=type))
#El modelo de regresión más sencillo: modelo de intercepto
mod0 <- lm(formula = prestige ~ 1, data = dat)
mod0
str(mod0)
summary(mod0)
plot_ed_prestigio <- ggplot(dat, aes(x = education,
                                    y = prestige)) +
  geom_point() +
  labs(x = "Años de educación media",
       y = "Prestigio Pineo-Porter ocupacional")
plot_ed_prestigio

plot_ed_prestigio +
  geom_hline(yintercept = coef(mod0)[1])

mod1 <- lm(prestige ~ 1 + education, data = dat) #el 1 sigue siendo del intercepto
#Mencionábase que innecesario resulta colocar el número uno, 
#siéndolo realizado automáticamente por R.
#lm(prestige ~ + education, data = dat)
summary(mod1)

plot_ed_prestigio +
  geom_abline(intercept = coef(mod1)[1],
              slope     = coef(mod1)[2])

mod_ingreso <- dat|>lm(prestige~1+income,data = _) #noten el uso de _ por |>. si fuera %>%, tienen que usar .
summary(mod_ingreso) #el resumen del objeto es la mejor forma de ver lo más crucial.
coef(mod_ingreso) #veamos lo que hay dentro de los coeficientes nada más
coef(mod_ingreso)[2] *1000 #1000 dolares mas salariales se asocian con 3 pts prestigio.


#names(dat)
dat<-dat|>
  mutate(
    ingreso_miles=income/1000
  )
mod_ingresoKs<-dat|>lm(prestige ~ ingreso_miles, data=_)
summary(mod_ingresoKs)

#Residuos
#importantes para entender validez de modelo. Están calculados para cada punto observado.
dat_pa_residuos <- dat %>%
  select(prestige, education) %>%
  mutate(predicción0  = predict(mod0),
         residuo0 = resid(mod0))
head(dat_pa_residuos, 10)

dat_pa_residuos %>%
  ggplot(aes(x = education, y = prestige)) +
  geom_segment(aes(xend = education, yend = predicción0)) +
  geom_point() +
  geom_hline(yintercept = coef(mod0))

dat_pa_residuos <- dat_pa_residuos  %>%
  mutate(predicción1  = predict(mod1),
         residuo1 = resid(mod1))
dat_pa_residuos %>%
  select(prestige, predicción1, residuo1, residuo0) %>%
  head(10)

dat_pa_residuos %>%
  ggplot(aes(x = education, y = prestige)) +
  geom_segment(aes(xend = education, yend = predicción1)) +
  geom_point() +
  geom_abline(intercept = coef(mod1)[1],
              slope     = coef(mod1)[2])

dat_pa_residuos <- dat_pa_residuos %>%
  mutate(resid_dif = abs(residuo1) - abs(residuo0),
         mod0_mejor = resid_dif >= 0)
dat_pa_residuos 
dat_pa_residuos|>
  group_by(mod0_mejor)|>
  count()

dat_pa_residuos %>%
  ggplot(aes(x = education, y = prestige, colour = mod0_mejor)) +
  geom_segment(aes(xend = education, yend = predicción0)) +
  geom_point() +
  geom_hline(yintercept = coef(mod0)) +
  labs(colour = "Modelo de intercepto es mejor") +
  geom_abline(intercept = coef(mod1)[1],
              slope     = coef(mod1)[2],
              linetype = "dashed")
sum(dat_pa_residuos$residuo0^2)
sum(dat_pa_residuos$residuo1^2) #el modelo de regresión minimiza la Suma de Cuadrados

#podemos comparar los modelos con anova
#esto sólo se puede hacer con modelos anidados, o cuyos predictores se usen mín en un modelo.

anova(mod0,mod1) #el p valor aquí va contra H0:Modelo0 es mejor para explicar varianza.
library(rgl)
#scatter3d(prestige ~ education + income, data = dat, surface = FALSE)

library(plotly)

plot_ly(x = dat$education, y = dat$prestige, z = dat$income, type = "scatter3d", color = dat$income) |> 
  layout(scene = list(xaxis = list(title = 'Educación media (en años)'),
                      yaxis = list(title = 'Prestigio (0-100)'),
                      zaxis = list(title = 'Ingreso (CAD, miles)')))
mod_ambos <- lm(prestige ~ education + ingreso_miles, data = dat)
# Ajustamos el mod_ambos de regresión
# Ajustamos el modelo
mod_ambos <- lm(prestige ~ education + ingreso_miles, data = dat)

# Creamos una cuadrícula de valores para education e ingreso_miles
grid <- expand.grid(
  education = seq(min(dat$education), max(dat$education), length.out = 30),
  ingreso_miles = seq(min(dat$ingreso_miles), max(dat$ingreso_miles), length.out = 30)
)

# Predecimos el prestigio (porque ese es el outcome del modelo)
grid$prestige <- predict(mod_ambos, newdata = grid)

# Convertimos las predicciones en una matriz para graficar el plano
z_matrix <- matrix(grid$prestige, nrow = 30, ncol = 30)

# Gráfico 3D con puntos y el plano de regresión
plot_ly(dat, x = ~education, y = ~ingreso_miles, z = ~prestige,
        type = "scatter3d", mode = "markers", marker = list(color = ~prestige)) |>
  add_surface(x = matrix(grid$education, nrow = 30),
              y = matrix(grid$ingreso_miles, nrow = 30),
              z = z_matrix,
              showscale = FALSE,
              opacity = 0.5) |>
  layout(scene = list(
    xaxis = list(title = 'Educación media (en años)'),
    yaxis = list(title = 'Ingreso (CAD, miles)'),
    zaxis = list(title = 'Prestigio (0-100)')
  ))
summary(mod_ambos)
anova(mod0,mod_ambos)
library(car)

Confint(mod_ambos)
#veamos el hiperplano
#scatter3d(prestige ~ education + ingreso_miles, data = dat, surface = TRUE)
# Creamos una variable con los colores personalizados
colores_tipo <- c("bc" = "blue", "wc" = "green", "prof" = "red")

# Gráfico 3D con color por tipo de trabajo
plot_ly(dat,
        x = ~education,
        y = ~ingreso_miles,
        z = ~prestige,
        type = "scatter3d",
        mode = "markers",
        color = ~type,
        colors = colores_tipo,
        marker = list(size = 5),
        hoverinfo = "text",
        text = ~paste("Ocupación:", occ,
                      "<br>Tipo:", type,
                      "<br>Educación:", education,
                      "<br>Ingreso (k):", ingreso_miles,
                      "<br>Prestigio:", prestige)) |>
  layout(scene = list(
    xaxis = list(title = "Educación (años)"),
    yaxis = list(title = "Ingreso (CAD, miles)"),
    zaxis = list(title = "Prestigio (0–100)")
  ))
#Vamos a cambiar el orden de la variable type para que ref sea el más alto,
#es decir prof.
mod_triple<-dat|> 
  mutate(type = relevel(factor(type), ref = "prof")) |>
  lm(prestige ~ education + ingreso_miles + type, data = _) 
mod_triple|> summary()
library(modelsummary)
library(kableExtra)
library(jtools)
plot_summs(mod_triple, plot.distributions = TRUE, inner_ci_level = 0.9)
plot_summs(mod_ambos, mod_triple, plot.distributions = TRUE, inner_ci_level = 0.9)
plot_summs(mod_ambos, mod_triple, plot.distributions = TRUE, inner_ci_level = 0.9,
           model.names = c("Modelo de educación e ingreso", "Modelo de educación, ingreso y tipo de empleo"),
           coefs = c("Intercepto" = "(Intercept)",
                     "Educación (años)" = "education",
                     "Ingreso (miles)" = "ingreso_miles",
                     "Tipo: Cuello azul" = "typebc",
                     "Tipo: Cuello blanco" = "typewc"),
           scale = TRUE)

# Diccionario para cambiar nombres de variables
nombres_vars <- c(
  "(Intercept)"     = "Intercepto",
  "education"       = "Educación (años)",
  "ingreso_miles"   = "Ingreso (en miles de CAD)",
  "typebc"          = "Cuello azul (vs. prof.)",
  "typewc"          = "Cuello blanco (vs. prof.)"
)

# Nombres de los modelos
nombres_modelos <- c("Modelo 1", "Modelo 2", "Modelo 3")

# Mostrar los modelos con variables renombradas y resultados en español
modelsummary(list("Modelo 1" = mod1, "Modelo 2" = mod_ambos, "Modelo 3" = mod_triple),
             coef_map = nombres_vars,
             gof_omit = "IC|Std.Errors", # Quita cosas innecesarias
             fmt = 2,
             estimate  = "{estimate} ***",
             statistic = "[{conf.low}, {conf.high}]",
             output = "latex")  
# default, tinytable, gt, kableExtra, flextable,
#huxtable, DT, html, jupyter, latex, latex_tabular, markdown, dataframe, typst,
#modelsummary_list

# Redefinir modelos con etiquetas personalizadas
mod1_lab <- summ(mod1, scale = TRUE, confint = TRUE,
                 vifs = FALSE, model.info = FALSE,
                 model.fit = FALSE)
mod_ambos_lab <- summ(mod_ambos, scale = TRUE, confint = TRUE,
                      vifs = FALSE, model.info = FALSE,
                      model.fit = FALSE)
mod_triple_lab <- summ(mod_triple, scale = TRUE, confint = TRUE,
                       vifs = FALSE, model.info = FALSE,
                       model.fit = FALSE)

export_summs(mod1, mod_ambos, mod_triple,
             scale = TRUE,
             error_format = "[{conf.low}, {conf.high}]",
             coefs = c(
               "Intercepto"                      = "(Intercept)",
               "Educación (años)"               = "education",
               "Ingreso (en miles de CAD)"      = "ingreso_miles",
               "Cuello azul (vs. prof.)"        = "typebc",
               "Cuello blanco (vs. prof.)"      = "typewc"
             ),
             model.names = c("Modelo 1", "Modelo 2", "Modelo 3"),
             to.file = "Word" )
library(modelsummary)

model_list <- list(
  "Modelo 1" = mod1,
  "Modelo 2" = mod_ambos,
  "Modelo 3" = mod_triple
)

# Exportar a archivo .tex
modelsummary(model_list,
             coef_map = c(
               "(Intercept)"   = "Intercepto",
               "education"     = "Educación (años)",
               "ingreso_miles" = "Ingreso (en miles de CAD)",
               "typebc"        = "Cuello azul (vs. prof.)",
               "typewc"        = "Cuello blanco (vs. prof.)"
             ),
             output = "tabla_modelos.tex")

export_summs(mod1,mod_ambos, mod_triple, scale = TRUE,
             error_format = "[{conf.low}, {conf.high}]")
#si quisiéreamos sacar las tablas de regresión a LaTeX:
stargazer::stargazer(mod1,mod_ambos,mod_triple)
## Verificando supuestos del modelo

dat$mod_ambos_resid<-resid(mod_ambos)
hist(dat$mod_ambos_resid)
qqPlot(dat$mod_ambos_resid, id = list(labels = dat$occ, n = 2))
#subraya por defecto los dos valores más extremos con el # de fila
# se puede alterar cantidad con argumento n=
set.seed(42) # con esto obtendrán los mismo valores aleatorios en la 1ª corrida
sesgáu <- exp(rnorm(50))
hist(sesgáu)
qqPlot(sesgáu)
# otra simulación
set.seed(5)
súper_normal <- rnorm(2500, 1000, 200)
hist(súper_normal)

# Prueba de normalidad estadística (en dist)
shapiro.test(súper_normal)
shapiro.test(sesgáu)
shapiro.test(dat$mod_ambos_resid)

#chequeando homoscedasticidad
dat$mod_ambos_pred <- predict(mod_ambos)
ggplot(dat, aes(x = mod_ambos_pred, y = mod_ambos_resid)) +
  geom_point() +
  labs(x = "Predicción", y = "Residuo")
residualPlots(mod_ambos)
ncvTest(mod_ambos)
ncvTest(mod_ambos, ~ingreso_miles)


#Evaluando lo categórico

dat |>
  ggplot(aes(type, prestige, colour = type)) +
  geom_jitter(height = 0,
              width = .1,
              show.legend = FALSE) +
  ylim(0,100)

dat |> 
  filter(is.na(type))

dat_medias <- dat |>
  group_by(type) |>
  summarise(media = mean(prestige),
            Desv.Est.   = sd(prestige),
            Err.Est.   = Desv.Est./sqrt(n()),
            n    = n())
dat_medias

dat_medias |>
  ggplot(aes(type, media)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = media - Err.Est.,
                    ymax = media + Err.Est.))

dat_sin_NAs <- dat |>
  select(occ, prestige, type) |>
  na.omit()
mod_tipo <- lm(prestige ~ relevel(factor(type),"prof"), data = dat_sin_NAs)
lm(prestige ~ relevel(factor(type), "prof"), data = dat_sin_NAs) %>%
  summary()
Anova(mod_tipo)
summary(mod_tipo)
stargazer::stargazer(mod_tipo, dep.var.labels = "Prestigio",
                     dep.var.caption = "Variable dependiente",
                     covariate.labels = c("Intercepto",
                                          "Cuello blanco",
                                          "Cuello azul"),
                     ci=T,
                     single.row = T,
                     title = "Regresión con predictores categóricos")
