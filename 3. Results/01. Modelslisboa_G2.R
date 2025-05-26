source("1. Data/00. Sources.R")

# Veamos el modelo espacial
dfgeo <- readRDS("1. Data/df_final.rds")

dfgeo <- sf::st_as_sf(dfgeo)

# Crear objeto espacial solo para un periodo 
mapa_2014 <- dfgeo %>%
  filter(fecha == as.Date("2014-01-01")) %>%
  st_as_sf()

# Crear matriz de pesos espaciales (listw)
vecinos <- poly2nb(mapa_2014)
listw <- nb2listw(vecinos, style = "W") #Por contigüidad

# Verificar tamaño correcto
length(listw$neighbours)  
length(unique(dfgeo$municipio))
table(dfgeo$municipio)

# Ordenar correctamente los datos
df2 <- dfgeo %>%
  filter(fecha <= as.Date("2019-12-01"),
         fecha >= as.Date("2011-01-01")) %>%
  mutate(treatment = case_when(
    municipio == "Lisboa" ~ 1,
    municipio == "Cascais" ~ 1,
    municipio == "Mafra" ~ 0,
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

#Comprobar tendencias paralelas

sdm_model <- spml(
  log(precios) ~ 
    treated_before + treated_before_trend +
    control_before + control_after_trend + control_before_trend +
    log(loans) + log(wages) + log(popdensity) + log(housestock) + log(ageing_ratio) +
    log(crime_rate) + log(ir) + log(dwelling) +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + octubre+ noviembre +
    y2014 + y2015 + y2016 + y2017 + y2018 + y2019,
  data = df2,
  listw = listw,
  lag = FALSE,
  spatial.error = "b",
  durbin = TRUE,
  robust =TRUE,
  index = c("municipio", "time")
)

summary(sdm_model)
linearHypothesis(sdm_model, "treated_before_trend = control_before_trend")

# test de placebo

# Veamos si se cumple el test de placebo
df_placebogeo <- df2 %>%
  mutate(post_placebo = ifelse(fecha >= as.Date("2013-08-01"), 1, 0),
         did_placebo = treatment*post_placebo) %>%
  filter(fecha <= as.Date("2014-08-01"),
         fecha >= as.Date("2011-01-01"))

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


# modelos generales fijando distinto
# Fijando por individuo
modelo1 <- spml(
  log(precios) ~ DiD  +
    log(wages) + log(popdensity) + log(housestock) + log(ageing_ratio)+
    log(loans) + log(crime_rate) + log(ir) + log(dwelling),
  data = df2,
  listw = listw,
  model = "within",
  effect = "individual",
  lag = TRUE,
  spatial.error = "none",
  durbin = TRUE,
  robust = TRUE,
  index = c("municipio", "time")
)

summary(modelo1)

# Fijando por individuo y mes
modelo2 <- spml(
  log(precios) ~ DiD + 
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre +
    log(wages) + log(popdensity) + log(housestock) + log(ageing_ratio)+
    log(loans) + log(crime_rate) + log(ir) + log(dwelling),
  data = df2,
  listw = listw,
  model = "within",
  effect = "individual",
  lag = TRUE,
  spatial.error = "none",
  durbin = TRUE,
  robust = TRUE,
  index = c("municipio", "time")
)

summary(modelo2)

# Fijando por individuo, mes y año
modelo3 <- spml(
  log(precios) ~ DiD +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre +
    y2014 + y2015 + y2016 + y2017 + y2018 + y2019 + 
    log(wages) + log(popdensity) + log(housestock) + log(ageing_ratio)+
    log(loans) + log(crime_rate) + log(ir) + log(dwelling),
  data = df2,
  listw = listw,
  model = "within",
  effect = "individual",
  lag = TRUE,
  spatial.error = "none",
  durbin = TRUE,
  robust = TRUE,
  index = c("municipio", "time")
)

summary(modelo3)

# Fijando por municipio y tendencia
modelo4 <- spml(
  log(precios) ~ DiD + 
    log(wages) + log(popdensity) + log(housestock) + log(ageing_ratio)+
    log(loans) + log(crime_rate) + log(dwelling),
  data = df2,
  listw = listw,
  model = "within",
  effect = "twoways",
  lag = TRUE,
  spatial.error = "none",
  durbin = TRUE,
  robust = TRUE,
  index = c("municipio", "time")
)

summary(modelo4)

extraer_resultados <- function(modelo) {
  resumen <- summary(modelo)
  coefs <- resumen$CoefTable
  data.frame(
    term = rownames(coefs),
    estimate = coefs[, "Estimate"]
  )
}

# Aplicar a cada modelo
rmodelo1 <- extraer_resultados(modelo1)
rmodelo2 <- extraer_resultados(modelo2)
rmodelo3 <- extraer_resultados(modelo3)
rmodelo4 <- extraer_resultados(modelo4)

rmodelo1$modelo <- "Modelo 1"
rmodelo2$modelo <- "Modelo 2"
rmodelo3$modelo <- "Modelo 3"
rmodelo4$modelo <- "Modelo 4"

resultados <- rbind(rmodelo1, rmodelo2, rmodelo3, rmodelo4) 
write.xlsx(resultados, "3. Results/tablas/modeloespacialg2.xlsx")


# Función para extraer AIC y número de observaciones manualmente
extraer_info_modelo <- function(modelo, datos) {
  logLik_val <- as.numeric(modelo$logLik)           # Log-verosimilitud
  k <- length(coef(modelo)) + 1                      # Parámetros + lambda
  aic <- 2 * k - 2 * logLik_val                      # AIC
  n <- nrow(datos)                                   # Número de observaciones
  return(c(AIC = aic, N = n))
}


# Aplicar la función a los modelos 1 al 4
resultados_bondad <- t(sapply(
  list(modelo1, modelo2, modelo3, modelo4),
  extraer_info_modelo,
  datos = df2  # Aquí se pasa el segundo argumento
))

# Añadir nombres de modelos
rownames(resultados_bondad) <- paste0("Modelo ", 1:4)

# Convertir a data.frame si deseas exportar
tabla_bondad <- as.data.frame(resultados_bondad)

# Mostrar tabla
print(tabla_bondad)

# modelo a probar
sdm_model <- spml(
  log(precios) ~ 
    treated_before + treated_before_trend +
    control_before + control_after_trend + control_before_trend +
    log(loans) + log(wages) + log(popdensity) + log(housestock) + log(ageing_ratio) +
    log(crime_rate) + log(ir) + log(dwelling) +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + octubre+ noviembre,
  data = df2,
  listw = listw,
  lag = FALSE,
  spatial.error = "b",
  durbin = TRUE,
  robust =TRUE,
  index = c("municipio", "time")
)

summary(sdm_model)
linearHypothesis(sdm_model, "treated_before_trend = control_before_trend")


# Repetimos incorporando error espacial
# Fijando por individuo
modelo1 <- spml(
  log(precios) ~ DiD  +
    log(wages) + log(popdensity) + log(housestock) + log(ageing_ratio)+
    log(loans) + log(crime_rate) + log(ir) + log(dwelling),
  data = df2,
  listw = listw,
  model = "within",
  effect = "individual",
  lag = TRUE,
  spatial.error = "k",
  durbin = TRUE,
  robust = TRUE,
  index = c("municipio", "time")
)

summary(modelo1)

# Fijando por individuo y mes
modelo2 <- spml(
  log(precios) ~ DiD + 
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre +
    log(wages) + log(popdensity) + log(housestock) + log(ageing_ratio)+
    log(loans) + log(crime_rate) + log(ir) + log(dwelling),
  data = df2,
  listw = listw,
  model = "within",
  effect = "individual",
  lag = TRUE,
  spatial.error = "k",
  durbin = TRUE,
  robust = TRUE,
  index = c("municipio", "time")
)

summary(modelo2)

# Fijando por individuo, mes y año
modelo3 <- spml(
  log(precios) ~ DiD +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre +
    y2014 + y2015 + y2016 + y2017 + y2018 + y2019 + 
    log(wages) + log(popdensity) + log(housestock) + log(ageing_ratio)+
    log(loans) + log(crime_rate) + log(ir) + log(dwelling),
  data = df2,
  listw = listw,
  model = "within",
  effect = "individual",
  lag = TRUE,
  spatial.error = "k",
  durbin = TRUE,
  robust = TRUE,
  index = c("municipio", "time")
)

summary(modelo3)

# Fijando por municipio y tendencia
modelo4 <- spml(
  log(precios) ~ DiD + 
    log(wages) + log(popdensity) + log(housestock) + log(ageing_ratio)+
    log(loans) + log(crime_rate) + dwellings,
  data = df2,
  listw = listw,
  model = "within",
  effect = "twoways",
  lag = TRUE,
  spatial.error = "k",
  durbin = TRUE,
  robust = TRUE,
  index = c("municipio", "time")
)

summary(modelo4)

df_plot <- df2 %>%
  st_drop_geometry() %>%  
  mutate(
    logprecios = log(precios),
    logwages = log(wages),
    logpopdensity = log(popdensity),
    loghousestock = log(housestock),
    logcrime = log(crime_rate),
    logdwelling = log(dwelling),
    logageing = log(ageing_ratio),
    logir = log(ir),
    logloans = log(loans)
  ) %>%
  select(logprecios, logwages, logpopdensity, loghousestock, logcrime,
         dwellings, logageing, logir, logloans) %>%
  pivot_longer(cols = -logprecios, names_to = "variable", values_to = "valor")

ggplot(df_plot, aes(x = valor, y = logprecios)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Relación entre precios y variables de control",
       x = "Variable de control (log)",
       y = "log(Precio de vivienda)")