source("1. Data/00. Sources.R")

# Veamos el modelo espacial
dfgeo <- readRDS("1. Data/df_final.rds")

dfgeo <- sf::st_as_sf(dfgeo)

# Crear objeto espacial solo para un periodo 
mapa_2014 <- dfgeo %>%
  filter(fecha == as.Date("2014-01-01"),
         municipio %in% c("Lisboa", "Cascais", "Sintra", "Amadora", "Odivelas", 
                          "Vila Franca de Xira")) %>%
  st_as_sf()

ggplot(mapa_2014) +
  geom_sf(aes(fill = precios), color = "white") +
  scale_fill_viridis_c(option = "plasma", name = "Precios") +
  labs(
    title = "Mapa de precios por municipio (enero 2014)",
    subtitle = "Municipios de la región de Lisboa",
    fill = "Precio"
  ) +
  theme_minimal()

# Crear matriz de pesos espaciales (listw)
vecinos <- poly2nb(mapa_2014)
listw <- nb2listw(vecinos, style = "W", zero.policy = TRUE) #Por contigüidad

# Verificar tamaño correcto
length(listw$neighbours)  
length(unique(dfgeo$municipio))
table(dfgeo$municipio)

# Ordenar correctamente los datos
df2q <- dfgeo %>%
  filter(fecha <= as.Date("2019-12-01"),
         fecha >= as.Date("2013-01-01"),
         municipio %in% c("Lisboa", "Cascais", "Sintra", "Amadora", "Odivelas", 
                          "Vila Franca de Xira")) %>%
  mutate(treatment = case_when(
    municipio == "Lisboa" ~ 1,
    municipio == "Cascais" ~ 1,
    municipio == "Sintra" ~ 1,
    municipio == "Amadora" ~ 0,
    municipio == "Odivelas" ~ 0,
    municipio == "Vila Franca de Xira" ~ 0
  )) %>%
  group_by(municipio) %>%
  mutate(time = row_number(), 
         post = ifelse(fecha >= as.Date("2014-08-01"), 1, 0),
         treated_before = ifelse(treatment == 1 & post == 0, 1, 0),
         control_before = ifelse(treatment == 0 & post == 0, 1, 0),
         control_after = ifelse(treatment == 0 & post == 1, 1, 0),
         treated_before_trend = ifelse(treatment == 1 & post == 0, time, 0),
         control_before_trend = ifelse(treatment == 0 & post == 0, time, 0),
         control_after_trend = ifelse(treatment == 0 & post == 1, time, 0),
         
         DiD = treatment*post,
         
         mes = as.integer(format(fecha, "%m")),
         enero = as.integer(mes == 1),
         febrero = as.integer(mes == 2),
         marzo = as.integer(mes == 3),
         abril = as.integer(mes == 4),
         mayo = as.integer(mes == 5),
         junio = as.integer(mes == 6),
         julio = as.integer(mes == 7),
         agosto = as.integer(mes == 8),
         septiembre = as.integer(mes == 9),
         octubre = as.integer(mes == 10),
         noviembre = as.integer(mes == 11),
         
         year = as.integer(format(fecha, "%Y")),
         
         y2012 = as.integer(year == 2012),
         y2013 = as.integer(year == 2013),
         y2014 = as.integer(year == 2014),
         y2015 = as.integer(year == 2015),
         y2016 = as.integer(year == 2016),
         y2017 = as.integer(year == 2017),
         y2018 = as.integer(year == 2018),
         y2019 = as.integer(year == 2019),
         
         d2014 = y2014*treatment,
         d2015 = y2015*treatment,
         d2016 = y2016*treatment,
         d2017 = y2017*treatment,
         d2018 = y2018*treatment,
         d2019 = y2019*treatment,
         
         dwelling = dwellings + 0.000000000001
  ) %>%
  ungroup() %>%
  arrange(municipio, fecha)

variables_cor <- dfgeo %>%
  dplyr::select(precios, ir, cost, CPI, sales, loans, wages, popdensity, 
                popincrease, population, housestock, ageing_ratio, crime_rate, dwellings)

cor_matrix <- cor(variables_cor, use = "complete.obs")

#Comprobar tendencias paralelas

test <- spml(
  log(precios) ~ 
    treated_before + treated_before_trend +
    control_before + control_after_trend + control_before_trend +
    log(loans) + log(wages) + log(popdensity) + log(housestock) + log(ageing_ratio) +
    log(crime_rate) + log(ir) + log(dwelling) +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + octubre+ noviembre +
    y2014 + y2015 + y2016 + y2017 + y2018 + y2019,
  data = df1q,
  listw = listw,
  lag = FALSE,
  spatial.error = "b",
  model = "within",
  effect = "individual",
  durbin = TRUE,
  robust =TRUE,
  index = c("municipio", "time"),
  zero.policy = TRUE
)

summary(test)
linearHypothesis(test, "treated_before_trend = control_before_trend")

# test de placebo

# Veamos si se cumple el test de placebo
df_placebogeo <- df1 %>%
  mutate(post_placebo = ifelse(fecha >= as.Date("2013-08-01"), 1, 0),
         did_placebo = treatment*post_placebo) %>%
  filter(fecha <= as.Date("2014-08-01"),
         fecha >= as.Date("2013-01-01"))

sdm_model <- spml(
  log(precios) ~ did_placebo +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + octubre+ noviembre +
    log(loans) + log(wages) + log(popdensity) + log(housestock) + log(ageing_ratio) +
    log(crime_rate) + log(ir),
  data = df_placebogeo,
  listw = listw,
  lag = FALSE,
  spatial.error = "b",
  durbin = TRUE,
  robust =TRUE,
  index = c("municipio", "fecha")
)

summary(sdm_model)