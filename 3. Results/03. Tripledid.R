source("1. Data/00. Sources.R")

# Carga de bbdd
dfgeo <- readRDS("1. Data/df_final.rds")

dfgeo <- sf::st_as_sf(dfgeo)

# Crear objeto espacial
mapa_2014 <- dfgeo %>%
  filter(fecha == as.Date("2014-01-01")) %>%
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

# Crear matriz de pesos espaciales
vecinos <- poly2nb(mapa_2014)
listw <- nb2listw(vecinos, style = "W") #Por contigüidad

# Verificar tamaño correcto
length(listw$neighbours)  
length(unique(dfgeo$municipio))
table(dfgeo$municipio)

# Segun pernoctaciones
df3 <- dfgeo %>%
  filter(fecha >= as.Date("2012-01-01"), 
         fecha <= as.Date("2019-12-01")) %>%
  mutate(
    pressure_group = case_when(
      municipio %in% c("Lisboa", "Cascais", "Sintra") ~ "high",
      municipio %in% c("Loures", "Mafra", "Oeiras") ~ "medium",
      municipio %in% c("Amadora", "Odivelas", "Vila Franca de Xira") ~ "low"
    ),
    
    post = as.integer(fecha >= as.Date("2014-08-01")),
    time = as.integer(as.factor(fecha)),
    
    high = as.integer(pressure_group == "high"),
    medium = as.integer(pressure_group == "medium"),
    low = as.integer(pressure_group == "low"),
    
    # Interacciones pre-tratamiento
    high_before = as.integer(high == 1 & post == 0),
    medium_before = as.integer(medium == 1 & post == 0),
    low_before = as.integer(low == 1 & post == 0),
    
    # Interacciones post-tratamiento
    medium_after = as.integer(medium == 1 & post == 1),
    low_after = as.integer(low == 1 & post == 1),
    
    # Tendencias
    high_before_trend = ifelse(high_before == 1, time, 0),
    medium_before_trend = ifelse(medium_before == 1, time, 0),
    low_before_trend = ifelse(low_before == 1, time, 0),
    
    medium_after_trend = ifelse(medium_after == 1, time, 0),
    low_after_trend = ifelse(low_after == 1, time, 0),
    
    DiDh = high * post,
    DiDm = medium * post,
    
    # Variables de control estacionales
    mes = as.integer(format(fecha, "%m")),
    enero = as.integer(mes == 1), febrero = as.integer(mes == 2), marzo = as.integer(mes == 3),
    abril = as.integer(mes == 4), mayo = as.integer(mes == 5), junio = as.integer(mes == 6),
    julio = as.integer(mes == 7), agosto = as.integer(mes == 8), septiembre = as.integer(mes == 9),
    octubre = as.integer(mes == 10), noviembre = as.integer(mes == 11),
    
    # Dummies anuales
    year = as.integer(format(fecha, "%Y")),
    y2013 = as.integer(year == 2013),
    y2014 = as.integer(year == 2014), y2015 = as.integer(year == 2015),
    y2016 = as.integer(year == 2016), y2017 = as.integer(year == 2017),
    y2018 = as.integer(year == 2018), y2019 = as.integer(year == 2019),
    
    #DiD anuales
    d2015h = high * y2015,
    d2016h = high * y2016,
    d2017h = high * y2017,
    d2018h = high * y2018,
    d2019h = high * y2019,
    
    d2015m = medium * y2015,
    d2016m = medium * y2016,
    d2017m = medium * y2017,
    d2018m = medium * y2018,
    d2019m = medium * y2019,
    
    # Ajuste para evitar log(0)
    dwelling = dwellings + 1e-12
  ) %>%
  arrange(municipio, fecha)


variables_cor <- dfgeo %>%
  dplyr::select(precios, ir, cost, CPI, sales, loans, wages, popdensity, 
                popincrease, population, housestock, ageing_ratio, crime_rate, dwellings)


#Comprobar tendencias paralelas

modelo_tendencias <- spml(
  log(precios) ~ 
    high_before + high_before_trend + 
    medium_before + medium_before_trend +
    low_before + low_before_trend +
    log(loans) + log(wages) + log(popdensity) + 
    log(housestock) + log(ageing_ratio) + 
    log(crime_rate) + log(ir) + log(dwelling) +
    enero + febrero + marzo + abril + mayo + junio +julio + agosto + septiembre + octubre + noviembre +
    y2014 + y2015 + y2016 + y2017 + y2018 + y2019,
  data = df3,
  listw = listw,
  lag = TRUE,
  spatial.error = "b",
  model = "within",
  effect = "individual",
  durbin = TRUE,
  robust = TRUE,
  index = c("municipio", "fecha")
)

summary(modelo_tendencias)

linearHypothesis(modelo_tendencias, "high_before_trend = low_before_trend")
linearHypothesis(modelo_tendencias, "medium_before_trend = low_before_trend")

# Se cumplen tendencias paralelas

df_placebo <- df3 %>%
  mutate(
    post_placebo = as.integer(fecha >= as.Date("2013-08-01")),
    did_placeboh = high * post_placebo,
    did_placebom = medium * post_placebo
  ) %>%
  filter(fecha < as.Date("2014-08-01"))  # solo antes de tratamiento real

modelo_placebo <- spml(
  log(precios) ~ did_placeboh  + did_placebom + post_placebo +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + octubre + noviembre +
    log(loans) + log(wages) + log(popdensity) + log(housestock) + log(ageing_ratio) +
    log(crime_rate) + log(ir) + log(dwelling),
  data = df_placebo,
  listw = listw,
  lag = FALSE,
  spatial.error = "b",
  model = "within",
  effect = "individual",
  durbin = TRUE,
  robust = TRUE,
  index = c("municipio", "time")
)

summary(modelo_placebo)

# No se cumple el placebo el grupo medio

# Segun presion turistica
df3 <- dfgeo %>%
  filter(fecha >= as.Date("2013-01-01"), 
         fecha <= as.Date("2019-12-01")) %>%
  mutate(
    pressure_group = case_when(
      municipio %in% c("Lisboa", "Cascais", "Mafra") ~ "high",
      municipio %in% c("Amadora", "Oeiras", "Sintra") ~ "medium",
      municipio %in% c("Loures", "Odivelas", "Vila Franca de Xira") ~ "low"
    ),
    
    post = as.integer(fecha >= as.Date("2014-08-01")),
    time = as.integer(as.factor(fecha)),
    
    high = as.integer(pressure_group == "high"),
    medium = as.integer(pressure_group == "medium"),
    low = as.integer(pressure_group == "low"),
    
    # Interacciones pre-tratamiento
    high_before = as.integer(high == 1 & post == 0),
    medium_before = as.integer(medium == 1 & post == 0),
    low_before = as.integer(low == 1 & post == 0),
    
    # Interacciones post-tratamiento
    medium_after = as.integer(medium == 1 & post == 1),
    low_after = as.integer(low == 1 & post == 1),
    
    # Tendencias
    high_before_trend = ifelse(high_before == 1, time, 0),
    medium_before_trend = ifelse(medium_before == 1, time, 0),
    low_before_trend = ifelse(low_before == 1, time, 0),
    
    medium_after_trend = ifelse(medium_after == 1, time, 0),
    low_after_trend = ifelse(low_after == 1, time, 0),
    
    DiDh = high * post,
    DiDm = medium * post,
    
    # Variables de control estacionales
    mes = as.integer(format(fecha, "%m")),
    enero = as.integer(mes == 1), febrero = as.integer(mes == 2), marzo = as.integer(mes == 3),
    abril = as.integer(mes == 4), mayo = as.integer(mes == 5), junio = as.integer(mes == 6),
    julio = as.integer(mes == 7), agosto = as.integer(mes == 8), septiembre = as.integer(mes == 9),
    octubre = as.integer(mes == 10), noviembre = as.integer(mes == 11),
    
    # Dummies anuales
    year = as.integer(format(fecha, "%Y")),
    y2013 = as.integer(year == 2013),
    y2014 = as.integer(year == 2014), y2015 = as.integer(year == 2015),
    y2016 = as.integer(year == 2016), y2017 = as.integer(year == 2017),
    y2018 = as.integer(year == 2018), y2019 = as.integer(year == 2019),
    
    #DiD anuales
    d2015h = high * y2015,
    d2016h = high * y2016,
    d2017h = high * y2017,
    d2018h = high * y2018,
    d2019h = high * y2019,
    
    d2015m = medium * y2015,
    d2016m = medium * y2016,
    d2017m = medium * y2017,
    d2018m = medium * y2018,
    d2019m = medium * y2019,
    
    # Ajuste para evitar log(0)
    dwelling = dwellings + 1e-12
  ) %>%
  arrange(municipio, fecha)



variables_cor <- dfgeo %>%
  dplyr::select(precios, ir, cost, CPI, sales, loans, wages, popdensity, 
                popincrease, population, housestock, ageing_ratio, crime_rate, dwellings)


#Comprobar tendencias paralelas

modelo_tendencias <- spml(
  log(precios) ~ 
    high_before + high_before_trend + 
    medium_before + medium_before_trend +
    low_before + low_before_trend + low_after_trend +
    log(loans) + log(wages) + log(popdensity) + 
    log(housestock) + log(ageing_ratio) + 
    log(crime_rate) + log(ir) + log(dwelling) +
    enero + febrero + marzo + abril + mayo + junio +julio + agosto + septiembre + octubre + noviembre +
    y2014 + y2015 + y2016 + y2017 + y2018 + y2019,
  data = df3,
  listw = listw,
  lag = FALSE,
  spatial.error = "b",
  model = "within",
  effect = "individual",
  durbin = TRUE,
  robust = TRUE,
  index = c("municipio", "fecha")
)

summary(modelo_tendencias)

linearHypothesis(modelo_tendencias, "high_before_trend = low_before_trend")
linearHypothesis(modelo_tendencias, "medium_before_trend = low_before_trend")

# No Se cumplen tendencias paralelas