#Visualización en la práctica ------------------------------
library(tidyverse)
library(dslabs)
data(gapminder)
?gapminder
gapminder |> as_tibble()

gapminder |>
  filter(year == 2015 & country %in% c("Poland","South Korea")) |>
  select(country, infant_mortality)
#¿se divide el mundo en las dicotomías de países ricos y occidentales ricos y de alta esperanza de vida
#vs países pobres de familias grandes y vidas cortas?
#¡usemos datos y visualizaciones!
filter(gapminder, year == 1962) |>
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()

filter(gapminder, year == 1962) |>
  ggplot(aes(fertility, life_expectancy, colour = continent)) +
  geom_point()

filter(gapminder, year%in%c(1962, 2012)) |>
  ggplot(aes(fertility, life_expectancy, colour = continent)) +
  geom_point() +
  facet_grid(continent~year)

filter(gapminder, year%in%c(1962, 2015)) |>
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(. ~ year)

years <- c(1962, 1980, 1990, 2000, 2012,2015)
continents <- c("Europe", "Asia")
gapminder |>
  filter(year %in% years & continent %in% continents) |>
  ggplot( aes(fertility, life_expectancy, colour = continent)) +
  geom_point() +
  facet_wrap(~year)

filter(gapminder, year%in%c(1962, 2012)) |>
  ggplot(aes(fertility, life_expectancy, colour = continent)) +
  geom_point() +
  facet_wrap(. ~ year, scales = "free")

gapminder |>
  filter(country == "Japan") |>
  ggplot(aes(year, fertility)) +
  geom_point()

gapminder |>
  filter(country == "Japan") |>
  ggplot(aes(year, fertility)) +
  geom_line()

countries <- c("South Korea","Germany", "France", "United Kingdom", "United States", "Japan")

gapminder |> filter(country %in% countries & !is.na(fertility)) |>
  ggplot(aes(year, fertility, group = country)) +
  geom_line()

gapminder |> filter(country %in% countries & !is.na(fertility)) |>
  ggplot(aes(year,fertility, colour = country)) +
  geom_line()

labels <- data.frame(country = countries, x = c(1975,1965), y = c(60,72))

gapminder |>
  filter(country %in% countries) |>
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")


gapminder <- gapminder |> mutate(dólares_diarios = gdp/population/365)

antaño <- 1970



gapminder |>
  filter(year == antaño & !is.na(gdp)) |>
  mutate(region = reorder(region, dólares_diarios, FUN = median)) |>
  ggplot(aes(dólares_diarios, region)) +
  geom_point() +
  scale_x_continuous(trans = "log2")

gapminder <- gapminder |>
  mutate(grupo = case_when(
    region %in% c("Western Europe", "Northern Europe","Southern Europe",
                  "Northern America",
                  "Australia and New Zealand") ~ "Occidente",
    region %in% c("Eastern Asia", "South-Eastern Asia") ~ "Asia oriental",
    region %in% c("Caribbean", "Central America",
                  "South America") ~ "Latinoamérica",
    continent == "Africa" &
      region != "Northern Africa" ~ "África subsahariana",
    TRUE ~ "Otros"))
#le damos orden a los niveles
gapminder <- gapminder |>
  mutate(grupo = factor(grupo, levels = c("Otros", "Latinoamérica",
                                          "Asia oriental", "África subsahariana",
                                          "Occidente")))

p <- gapminder |>
  filter(year == antaño & !is.na(gdp)) |>
  ggplot(aes(grupo, dólares_diarios)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
p + geom_point(alpha = 0.5)

library(ggridges)
p <- gapminder |>
  filter(year == antaño & !is.na(dólares_diarios)) |>
  ggplot(aes(dólares_diarios, grupo)) +
  scale_x_continuous(trans = "log2")
p + geom_density_ridges()
p + geom_density_ridges(jittered_points = TRUE)

p + geom_density_ridges(jittered_points = TRUE,
                        position = position_points_jitter(height = 0),
                        point_shape = '|', point_size = 3,
                        point_alpha = 1, alpha = 0.7)
gapminder$grupo

antaño <- 1970
año_presente <- 2010

years <- c(antaño, año_presente)
gapminder |>
  filter(year %in% years & !is.na(gdp)) |>
  mutate(west = ifelse(grupo == "Occidente", "Occidente", "El Resto")) |>
  ggplot(aes(dólares_diarios)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ west)

country_list_1 <- gapminder |>
  filter(year == antaño & !is.na(dólares_diarios)) |>
  pull(country)

country_list_2 <- gapminder |>
  filter(year == año_presente & !is.na(dólares_diarios)) |>
  pull(country)

country_list <- intersect(country_list_1, country_list_2)

gapminder |>
  filter(year %in% years & country %in% country_list) |>
  ggplot(aes(grupo, dólares_diarios)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("") +
  facet_grid(. ~ year)

gapminder |>
  filter(year %in% years & country %in% country_list) |>
  mutate(year = factor(year)) |>
  ggplot(aes(grupo, dólares_diarios, fill = year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("")

gapminder |>
  filter(year %in% years & country %in% country_list) |>
  ggplot(aes(dólares_diarios)) +
  geom_density(fill = "grey") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ year)

gapminder |>
  filter(year %in% years & country %in% country_list) |>
  mutate(grupo = ifelse(grupo == "Occidente", "Occidente", "Resto")) |>
  ggplot(aes(dólares_diarios, fill = grupo)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2) +
  facet_grid(year ~ .)

p <- gapminder |>
  filter(year %in% years & country %in% country_list) |>
  mutate(grupo = ifelse(grupo == "Occidente", "Occidente", "Resto")) |>
  ggplot(aes(dólares_diarios, y = ..count.., fill = grupo)) +
  scale_x_continuous(trans = "log2", limit = c(0.125, 300))

p + geom_density(alpha = 0.2) +
  facet_grid(year ~ .)

gapminder |>
  filter(year %in% years & !is.na(dólares_diarios)) |>
  ggplot(aes(dólares_diarios, grupo)) +
  scale_x_continuous(trans = "log2") +
  geom_density_ridges(adjust = 1.5) +
  facet_grid(. ~ year)

gapminder |>
  filter(year %in% years & country %in% country_list) |>
  group_by(year) |>
  mutate(weight = population/sum(population)*2) |>
  ungroup() |>
  ggplot(aes(dólares_diarios, fill = grupo)) +
  scale_x_continuous(trans = "log2", limit = c(0.125, 300)) +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)+labs(title = "Densidades apiladas: ingreso en dólares por día per cápita entre 1970 y 2010",
                            x = "Dólares por día", 
                            y = "Densidad",
                            color = "Regiones")

# Usando el tidycensus para generar mapas con datos censales. ------
#install.packages(c("tidycensus", "sf", "tigris"))
library(tidyverse)
library(tidycensus)
library(sf)
#census_api_key(INSERTE SU LLAVE DEL CENSO AQUÍ)

options(scipen=999)
options(tigris_class = "sf")
#https://api.census.gov/data.html
census_variables <- load_variables(year = 2020, dataset = "acs5", cache = TRUE)
#ensus_variablessf1 <- load_variables(year = 2020, dataset = "sf1", cache = TRUE)
#census_variablessf2 <- load_variables(year = 2020, dataset = "sf2", cache = TRUE)
#census_variables4Redist <- load_variables(year = 2020, dataset = "pl", cache = TRUE)
#v00 <- load_variables(2000, "sf3", cache = TRUE))
v22 <- load_variables(2022, "acs5", cache = TRUE)
v10 <- load_variables(2010, "sf1", cache = TRUE)
census_variables
View(census_variables)
v10
v22
?get_acs
help("get_acs")

prmedian_2022 <- get_acs(geography = "state legislative district (lower chamber)", 
                         variables = c(medincome = "B19013_001"), 
                         state = "PR", 
                         year = 2022)
prmedian_2020 <- get_acs(geography = "state legislative district (lower chamber)", 
                         variables = c(medincome = "B19013_001"), 
                         state = "PR", 
                         year = 2020)
head(prmedian_2022)
pr_median_combined <- prmedian_2022 |>
  mutate(year = 2022) |>
  bind_rows(
    prmedian_2020 |> mutate(year = 2020)
  )
head(pr_median_combined)

pr_median_combined <- pr_median_combined |>
  filter(!is.na(estimate))  # Filter out rows where the estimate is NA

pr_median_combined|>
  ggplot(aes(x = NAME, y = estimate, fill = factor(year))) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe), 
                position = position_dodge(0.9), width = 0.25) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Ingreso mediano por distrito legislativo en Puerto Rico (2020 vs 2022)",
    x = "Distrito representativo",
    y = "Ingreso mediano",
    fill = "Año"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)  # Rotar nombres de distritos 90 grados para legibilidad
  )
pr_median_combined <- pr_median_combined |>
  # Extraer el número del distrito desde el nombre original y convertirlo en numérico
  mutate(district_number = as.numeric(gsub(".*District ([0-9]+).*", "\\1", NAME))) |>
  # Renombrar el campo NAME para que tenga el formato "Distrito 01" hasta "Distrito 40"
  mutate(NAME = sprintf("Distrito %02d", district_number)) |>
  # Ordenar los datos según el número de distrito
  arrange(district_number)

# Crear el gráfico nuevamente
pr_median_combined |>
  ggplot(aes(x = NAME, y = estimate, fill = factor(year))) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe), 
                position = position_dodge(0.9), width = 0.25) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Ingreso mediano por distrito representativo en Puerto Rico (2018 vs 2019)",
    x = "Distrito representativo",
    y = "Ingreso mediano",
    fill = "Año"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)  # Rotar nombres de distritos 90 grados para legibilidad
  )


library(tigris)

pr_legislative_districts <- state_legislative_districts(state = "PR", house = "lower", year = 2022)
pr_legislative_districts <- st_transform(pr_legislative_districts, crs = "EPSG:4326")  # ajustar proyección tal cual necesario
pr_legislative_districts_2020 <- state_legislative_districts(state = "PR", house = "lower", year = 2020)
pr_legislative_districts_2020 <- st_transform(pr_legislative_districts_2020, crs = "EPSG:4326")  # ajustar proyección tal cual necesario
pr_median_2020_geo <- pr_legislative_districts_2020 |>
  left_join(prmedian_2020, by = c("GEOID" = "GEOID"))  # Unir datos de ingreso mediano con geometría de distritos
prmedian_2022_geo <- pr_legislative_districts |>
  left_join(prmedian_2022, by = c("GEOID" = "GEOID"))  # Unir datos de ingreso mediano con geometría de distritos

a<-pr_median_2020_geo|>
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c(option = "plasma", name = "Ingreso mediano") +
  theme_minimal() +
  labs(
    title = "Ingreso mediano por distrito representativo (2020)",
    fill = "Ingreso mediano"
  )
a
b<-prmedian_2022_geo|>
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c(option = "plasma", name = "Ingreso mediano") +
  theme_minimal() +
  labs(
    title = "Ingreso mediano por distrito representativo (2022)",
    fill = "Ingreso mediano"
  )
library(gridExtra)
pr2020_2022<-grid.arrange(a,b,nrow = 2)
pr2020_2022
ggsave(pr2020_2022, filename = "pr2020_2022.png", width = 10, height = 10, units = "in", dpi = 300)