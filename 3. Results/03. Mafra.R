source("1. Data/00. Sources.R")

# Veamos el modelo espacial con mafra como control, es decir, usando criterio de pernoctaciones
dfgeo <- readRDS("1. Data/df_final.rds")

dfgeo <- sf::st_as_sf(dfgeo)

# Crear objeto espacial solo para un periodo 
mapa_2014 <- dfgeo %>%
  filter(fecha == as.Date("2014-01-01")) %>%
  st_as_sf()


dfgeo <- dfgeo %>%
  select(!c("sales", "loans")) %>%
  filter(fecha <= as.Date("2019-12-01"))


# Crear matriz de pesos espaciales (listw)
vecinos <- poly2nb(mapa_2014)
listw <- nb2listw(vecinos, style = "W") #Por contigüidad

# Verificar tamaño correcto
length(listw$neighbours)  
length(unique(dfgeo$municipio))
table(dfgeo$municipio)

# Ordenar correctamente los datos
dfgeo <- dfgeo %>%
  mutate(treatment = case_when(
    municipio == "Lisboa" ~ 1,
    municipio == "Cascais" ~ 1,
    municipio == "Oeiras" ~ 1,
    municipio == "Sintra" ~ 1,
    municipio == "Mafra" ~ 0,
    municipio == "Amadora" ~ 0,
    municipio == "Loures" ~ 0,
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
         DiD = treatment*post) %>%
  ungroup() %>%
  arrange(municipio, fecha)

#Comprobar tendencias paralelas

sdm_model <- spml(
  log(precios) ~ 
    treated_before + treated_before_trend  + 
    control_before_trend + control_after + control_after_trend +
    log(wages) + log(popdensity) + log(housestock) +
    log(ir) + log(ageing_ratio),
  data = dfgeo,
  listw = listw,
  lag = FALSE,
  spatial.error = "b",
  durbin = TRUE,
  robust =TRUE,
  index = c("municipio", "fecha")
)

summary(sdm_model)
linearHypothesis(sdm_model, "treated_before_trend = control_before_trend")


# Ejecutar modelo DiD
sdm_model <- spml(
  log(precios) ~ DiD + log(wages) + log(popdensity) + log(housestock) + 
    log(ir) + log(ageing_ratio),
  data = dfgeo,
  listw = listw,
  model = "within",
  lag = TRUE,
  spatial.error = "none",
  durbin = TRUE,
  robust = TRUE,
  index = c("municipio", "fecha")
)

summary(sdm_model)

impactos <- impacts(
  sdm_model,
  listw = listw,
  time = 108,  
  R = 1000      
)

summary(impactos, zstats = TRUE)


# Veamos el caso de Mafra como grupo tratado
dfgeo <- readRDS("1. Data/df_final.rds")

dfgeo <- sf::st_as_sf(dfgeo)

# Crear objeto espacial solo para un periodo 
mapa_2014 <- dfgeo %>%
  filter(fecha == as.Date("2014-01-01")) %>%
  st_as_sf()


dfgeo <- dfgeo %>%
  select(!c("sales", "loans")) %>%
  filter(fecha <= as.Date("2019-12-01"))


# Crear matriz de pesos espaciales (listw)
vecinos <- poly2nb(mapa_2014)
listw <- nb2listw(vecinos, style = "W") #Por contigüidad

# Verificar tamaño correcto
length(listw$neighbours)  
length(unique(dfgeo$municipio))
table(dfgeo$municipio)

# Ordenar correctamente los datos
dfgeo <- dfgeo %>%
  mutate(treatment = case_when(
    municipio == "Lisboa" ~ 1,
    municipio == "Cascais" ~ 1,
    municipio == "Oeiras" ~ 1,
    municipio == "Sintra" ~ 1,
    municipio == "Mafra" ~ 1,
    municipio == "Amadora" ~ 0,
    municipio == "Loures" ~ 0,
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
         DiD = treatment*post) %>%
  ungroup() %>%
  arrange(municipio, fecha)

#Comprobar tendencias paralelas

sdm_model <- spml(
  log(precios) ~ 
    treated_before + treated_before_trend  + 
    control_before_trend + control_after + control_after_trend +
    log(wages) + log(popdensity) + log(housestock) +
    log(ir) + log(ageing_ratio),
  data = dfgeo,
  listw = listw,
  lag = FALSE,
  spatial.error = "b",
  durbin = TRUE,
  robust =TRUE,
  index = c("municipio", "fecha")
)

summary(sdm_model)
linearHypothesis(sdm_model, "treated_before_trend = control_before_trend") 
# No se cumple la hipótesis de tendencias paralelas

# Veamos el caso sin Mafra
dfgeo <- readRDS("1. Data/df_final.rds") %>%
  filter(!municipio == "Mafra")

dfgeo <- sf::st_as_sf(dfgeo)

# Crear objeto espacial solo para un periodo 
mapa_2014 <- dfgeo %>%
  filter(fecha == as.Date("2014-01-01")) %>%
  st_as_sf()


dfgeo <- dfgeo %>%
  select(!c("sales", "loans")) %>%
  filter(fecha <= as.Date("2019-12-01"))


# Crear matriz de pesos espaciales (listw)
vecinos <- poly2nb(mapa_2014)
listw <- nb2listw(vecinos, style = "W") #Por contigüidad

# Verificar tamaño correcto
length(listw$neighbours)  
length(unique(dfgeo$municipio))
table(dfgeo$municipio)

# Ordenar correctamente los datos
dfgeo <- dfgeo %>%
  mutate(treatment = case_when(
    municipio == "Lisboa" ~ 1,
    municipio == "Cascais" ~ 1,
    municipio == "Oeiras" ~ 1,
    municipio == "Sintra" ~ 1,
    municipio == "Amadora" ~ 0,
    municipio == "Loures" ~ 0,
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
         DiD = treatment*post) %>%
  ungroup() %>%
  arrange(municipio, fecha)

#Comprobar tendencias paralelas

sdm_model <- spml(
  log(precios) ~ 
    treated_before + treated_before_trend  + 
    control_before_trend + control_after + control_after_trend +
    log(wages) + log(popdensity) + log(housestock) +
    log(ir) + log(ageing_ratio),
  data = dfgeo,
  listw = listw,
  lag = FALSE,
  spatial.error = "b",
  durbin = TRUE,
  robust =TRUE,
  index = c("municipio", "fecha")
)

summary(sdm_model)
linearHypothesis(sdm_model, "treated_before_trend = control_before_trend")

# No se cumple la hipótesis de tendencias paralelas
