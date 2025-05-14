source("1. Data/00. Sources.R")

df <- readRDS("2. Scripts/modellisboa.rds")

# Comprobar las tendencias paralelas
df <- df %>%
  arrange(municipio, fecha) %>%
  group_by(municipio) %>%
  mutate(treated = case_when(
    municipio == "Lisboa" ~ 1,
    municipio == "Cascais" ~ 1,
    municipio == "Mafra" ~ 1,
    municipio == "Oeiras" ~ 1,
    municipio == "Sintra" ~ 1,
    municipio == "Amadora" ~ 0,
    municipio == "Loures" ~ 0,
    municipio == "Odivelas" ~ 0,
    municipio == "Vila Franca de Xira" ~ 0
  ), 
    time = row_number(), 
    post = ifelse(fecha >= as.Date("2014-08-01"), 1, 0),
    treated_before = ifelse(treated == 1 & post == 0, 1, 0),
    control_before = ifelse(treated == 0 & post == 0, 1, 0),
    control_after = ifelse(treated == 0 & post == 1, 1, 0),
    treated_before_trend = ifelse(treated == 1 & post == 0, time, 0),
    control_before_trend = ifelse(treated == 0 & post == 0, time, 0),
    control_after_trend = ifelse(treated == 0 & post == 1, time, 0),
    did = post*treated,
  
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
    y2019 = as.integer(year == 2019)
    
  ) %>%
  ungroup() %>%
  filter(fecha <= as.Date("2019-12-01"),
         fecha >= as.Date("2012-01-01"),
         treated == 0 | treated == 1)

# veamos con un modelo de efectos fijos
modelo <- feols(
  log(precios) ~ 0 + 
    treated_before + treated_before_trend + 
    control_before + control_before_trend + 
    control_after + control_after_trend +
    log(wages) + log(ir) + log(popdensity) + log(housestock) + log(ageing_ratio) +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre +
    y2012 + y2013 + y2014 + y2014 + y2015 + y2016 + y2017 + y2018 + y2019,
  data = df,
  vcov = "hetero"
)

summary(modelo)

linearHypothesis(modelo, "treated_before_trend = control_before_trend")
# Se cumplen las tendencias paralelas

df_placebo <- df %>%
  mutate(post_placebo = ifelse(fecha >= as.Date("2013-08-01"), 1, 0)) %>%
  filter(fecha <= as.Date("2014-08-01"),
         fecha >= as.Date("2012-01-01"))

modelo_placebo <- feols(
  log(precios) ~ treated * post_placebo +
    log(wages) + log(ir) + log(popdensity) + log(housestock) + log(ageing_ratio) +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre +
    y2012 + y2013 + y2014 + y2014 + y2015 + y2016 + y2017 + y2018 + y2019 |
    municipio,
  data = df_placebo,
  vcov = "hetero"
)

summary(modelo_placebo)
# No hay tendencias antes del tratamiento

variables_cor <- df %>%
  dplyr::select(precios, ir, cost, CPI, sales, loans, wages, popdensity, 
                popincrease, population, housestock, ageing_ratio, crime_rate, dwellings)

cor_matrix <- cor(variables_cor, use = "complete.obs")

# 1
modelo_did <- feols(log(precios) ~ treated*post + 
                      enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre +
                      y2012 + y2013 + y2014 + y2015 + y2016 + y2017 + y2018 + y2018 + y2019
                    | municipio,
                    data = df,
                    vcov = "hetero")

summary(modelo_did)
residual <- resid(modelo_did)
jarque.bera.test(residual)
acf(residual)


modelsummary(modelo_did,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_omit = "IC|Log|F|AIC|BIC|RMSE",
             coef_rename = c("treated:post" = "Tratado × Post"),
             title = "Estimación del efecto de la presión turística sobre el precio de la vivienda",
             notes = "Errores estándar robustos entre paréntesis. * p < 0.1, ** p < 0.05, *** p < 0.01.",
             output = "latex") # puedes usar "markdown", "html" o "latex"
modelsummary(modelo_did,
             stars = TRUE,
             output = "tabla_did.docx") 

# 2
modelo_did <- feols(log(precios) ~ treated*post + 
                      log(wages) + log(popdensity) + log(crime_rate) +
                      log(ageing_ratio) + log(housestock)
                    | municipio  + mes,
                    data = df,
                    vcov = "hetero")

summary(modelo_did)
residual <- resid(modelo_did)
jarque.bera.test(residual)

collinearity(modelo_did)
vif(modelo_did)
acf(residual)

# 3

modelo_did_robusto <- feols(log(precios) ~ treated*post +
                            log(wages) + log(housestock) + log(loans) +
                            enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre 
                            | municipio,
                            data = df,
                            vcov = "twoway")
summary(modelo_did_robusto)

residual <- resid(modelo_did_robusto)
jarque.bera.test(residual)

collinearity(modelo_did_robusto)
vif(modelo_did_robusto)
acf(residual)

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
dfgeo <- dfgeo %>%
  filter(fecha <= as.Date("2019-12-01"),
         fecha >= as.Date("2013-01-01")) %>%
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
         d2019 = y2019*treatment
  ) %>%
  ungroup() %>%
  arrange(municipio, fecha)

variables_cor <- dfgeo %>%
  dplyr::select(precios, ir, cost, CPI, sales, loans, wages, popdensity, 
                popincrease, population, housestock, ageing_ratio, crime_rate, dwellings)

cor_matrix <- cor(variables_cor, use = "complete.obs")



#Comprobar tendencias paralelas

sdm_model <- spml(
  log(precios) ~ 
    treated_before + treated_before_trend  + 
    control_before + control_after_trend + control_before_trend +
    log(loans) + log(wages) + log(popdensity) + log(housestock) + log(ageing_ratio) +
    log(crime_rate) + log(ir) +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + octubre+ noviembre +
    y2014 + y2015 + y2016 + y2017 + y2018 + y2019,
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

# test de placebo

# Veamos si se cumple el test de placebo
df_placebogeo <- dfgeo %>%
  mutate(post_placebo = ifelse(fecha >= as.Date("2013-08-01"), 1, 0),
         did_placebo = treatment*post_placebo) %>%
  filter(fecha <= as.Date("2014-08-01"),
         fecha >= as.Date("2013-01-01"))

sdm_model <- spml(
  log(precios) ~ did_placebo +
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

# Ejecutar modelo 
sdm_model <- spml(
  log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019  +
    log(loans) + log(wages) + log(popdensity) + log(housestock) + log(ageing_ratio),
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



