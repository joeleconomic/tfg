source("1. Data/00. Sources.R")

df_amplio <- readRDS("2. Scripts/dfamplio.rds")

# Veamos si se cumple el criterio poblacion turistica
df_amplio <- df_amplio %>%
  arrange(municipio, fecha) %>%
  filter(grupo1 == 1 | grupo1 == 0) %>%
  filter(!municipio %in% c("Oliveira de Azeméis", "Trofa", "Espinho"),
         fecha <= as.Date("2019-12-01"),
         fecha >= as.Date ("2013-01-01")) %>%
  group_by(municipio) %>%
  mutate(post = ifelse(fecha >= as.Date("2014-08-01"), 1, 0),
         time = row_number(),
         
         treated_before = ifelse(grupo1 == 1 & post == 0, 1, 0),
         treated_before_trend = ifelse(grupo1 == 1 & post == 0, time, 0),
         
         control_before = ifelse(grupo1 == 0 & post == 0, 1, 0),
         control_before_trend = ifelse(grupo1 == 0 & post == 0, time, 0),
         
         control_after = ifelse(grupo1 == 0 & post == 1, 1, 0),
         control_after_trend = ifelse(grupo1 == 0 & post == 1, time, 0),
         
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
         
         d2014 = y2014*grupo1,
         d2015 = y2015*grupo1,
         d2016 = y2016*grupo1,
         d2017 = y2017*grupo1,
         d2018 = y2018*grupo1,
         d2019 = y2019*grupo1
  ) %>%
    ungroup()

modelo <- feols(
  log(precios) ~ 0 + 
    treated_before + treated_before_trend + 
    control_before + control_before_trend + 
    control_after + control_after_trend +
    log(loans) + log(wages) + log(popdensity) + log(housestock) + dwellings +
    log(ageing_ratio) + log(crime_rate) + log(ir) +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre +
    y2014 + y2014 + y2015 + y2016 + y2017 + y2018 + y2019
  | municipio,
  data = df_amplio,
  vcov = ~ municipio + mes
)

summary(modelo)
modelsummary(modelo, stars = TRUE, outp = "tendendiasparalelas.docx")

linearHypothesis(modelo, "treated_before_trend = control_before_trend")
# Se cumplen las tendencias paralelas

# Veamos si se cumple el test de placebo
df_placebo <- df_amplio %>%
  mutate(post_placebo = ifelse(fecha >= as.Date("2013-08-01"), 1, 0)) %>%
  filter(fecha <= as.Date("2014-08-01"),
         fecha >= as.Date("2012-08-01"))

modelo_placebo <- feols(
  log(precios) ~ grupo1 * post_placebo +
  log(loans) + log(wages) + log(popdensity) + log(housestock) + log(dwellings) +
  log(ageing_ratio) + log(crime_rate) + log(ir) +
  enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre +
  y2013 + y2014
  | municipio,
  data = df_placebo,
  vcov = ~ municipio + mes)

summary(modelo_placebo)
modelsummary(modelo_placebo, stars = TRUE, outp = "testdeplacebo.docx")

modelsummary(
  list("Test de tendencias paralelas" = modelo, "Test de placebo" = modelo_placebo),
  stars = TRUE,
  statistic = NULL,  # Oculta los errores estándar o cualquier estadístico debajo del coeficiente
  output = "3. Results/test.docx"
)


# Matriz de correlaciones
# Seleccionar solo variables numéricas relevantes
variables_cor <- df_amplio %>%
  dplyr::select(precios, ir, cost, CPI, sales, loans, wages, popdensity, 
                popincrease, population, housestock, ageing_ratio, crime_rate, dwellings)

cor_matrix <- cor(variables_cor, use = "complete.obs")


#1
modelo1 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                 y2014 + y2015 + y2016 + y2017 + y2018 + y2019 +
                 log(housestock) + log(ageing_ratio) + log(crime_rate) + 
                   log(loans) + log(wages) + log(popdensity) + dwellings + 
                   log(ir)|
                 municipio + mes,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo1)
residual <- resid(modelo1)
jarque.bera.test(residual)

modelo2 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   y2014 + y2015 + y2016 + y2017 + y2018 + y2019 +
                   log(housestock) + log(ageing_ratio) + log(crime_rate) + 
                   log(loans) + log(wages) + log(popdensity) + dwellings |
                   municipio + mes,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo2)

modelo3 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   y2014 + y2015 + y2016 + y2017 + y2018 + y2019 +
                   log(housestock) + log(ageing_ratio) + log(crime_rate) + 
                   log(loans) + log(wages) + log(popdensity)  |
                   municipio + mes,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo3)

modelo4 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   y2014 + y2015 + y2016 + y2017 + y2018 + y2019 +
                   log(housestock) + log(ageing_ratio) + log(crime_rate) + 
                   log(loans) + log(wages) |
                   municipio + mes,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo4)

modelo5 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   y2014 + y2015 + y2016 + y2017 + y2018 + y2019 +
                   log(housestock) + log(ageing_ratio) + log(crime_rate) + 
                   log(loans) |
                   municipio + mes,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo5)

modelo6 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   y2014 + y2015 + y2016 + y2017 + y2018 + y2019 +
                   log(housestock) + log(ageing_ratio) + log(crime_rate) |
                   municipio + mes,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo6)

modelo7 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   y2014 + y2015 + y2016 + y2017 + y2018 + y2019 +
                   log(housestock) + log(ageing_ratio) |
                   municipio + mes,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo7)

modelsummary(
  list(
    "Modelo 1" = modelo1,
    "Modelo 2" = modelo2,
    "Modelo 3" = modelo3,
    "Modelo 4" = modelo4,
    "Modelo 5" = modelo5,
    "Modelo 6" = modelo6,
    "Modelo 7" = modelo7
  ),
  statistic = NULL,
  stars = TRUE,
  output = "3. Results/comparacion_modelos1.docx"
)


#2
modelo1 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   log(housestock) +  log(wages) + log(ir) + log(popdensity) + 
                   log(loans)  + +log(ageing_ratio) + dwellings + 
                   + log(crime_rate)|
                   municipio + mes,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo1)
residual <- resid(modelo1)
jarque.bera.test(residual)

modelo2 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   log(housestock) +  log(wages) + log(ir) + log(popdensity) + 
                   log(loans)  + +log(ageing_ratio) + dwellings|
                   municipio + mes,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo2)


modelo3 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   log(housestock) +  log(wages) + log(ir) + log(popdensity) + 
                   log(loans)  + +log(ageing_ratio)|
                   municipio + mes,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo3)



modelo4 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   log(housestock) +  log(wages) + log(ir) + log(popdensity) + 
                   log(loans) |
                   municipio + mes,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo4)


modelo5 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   log(housestock) +  log(wages) + log(ir) + log(popdensity) |
                   municipio + mes,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo5)

modelo6 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   log(housestock) +  log(wages) + log(ir)|
                   municipio + mes,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo6)


modelsummary(
  list(
    "Modelo 1" = modelo1,
    "Modelo 2" = modelo2,
    "Modelo 3" = modelo3,
    "Modelo 4" = modelo4,
    "Modelo 5" = modelo5,
    "Modelo 6" = modelo6
  ),
  statistic = NULL,
  stars = TRUE,
  output = "3. Results/comparacion_modelos2.docx"
)

#3
modelo1 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   y2015 + y2016 + y2017 + y2018 + y2019 +
                   log(housestock) + log(ageing_ratio) + log(crime_rate) + 
                   log(loans) + log(wages) + log(popdensity) + dwellings|
                   municipio + time,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo1)


#4
modelo1 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   log(housestock) +  log(wages) + log(popdensity) + 
                   log(loans)  + +log(ageing_ratio) + dwellings + 
                   + log(crime_rate)|
                   municipio + time,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo1)
residual <- resid(modelo1)
jarque.bera.test(residual)

modelo2 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   log(housestock) +  log(wages) + log(popdensity) + 
                   log(loans)  + +log(ageing_ratio) + 
                   + log(crime_rate)|
                   municipio + time,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo2)

modelo3 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   log(housestock) +  log(wages) + log(popdensity) + 
                   +log(ageing_ratio) + 
                   + log(crime_rate)|
                   municipio + time,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo3)

modelo4 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   log(housestock) +  log(wages)  + 
                   +log(ageing_ratio) + 
                   + log(crime_rate)|
                   municipio + time,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo4)

modelo5 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   log(housestock)  + 
                   +log(ageing_ratio) + 
                   + log(crime_rate)|
                   municipio + time,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo5)

modelo6 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
                   log(housestock)  + 
                   +log(ageing_ratio) 
                   |
                   municipio + time,
                 data = df_amplio,
                 vcov = "twoway")

summary(modelo6)

modelsummary(
  list(
    "Modelo 1" = modelo1,
    "Modelo 2" = modelo2,
    "Modelo 3" = modelo3,
    "Modelo 4" = modelo4,
    "Modelo 5" = modelo5,
    "Modelo 6" = modelo6
  ),
  statistic = NULL,
  stars = TRUE,
  output = "3. Results/comparacion_modelos2.docx")



# Veamos solo el comportamiento de Lisboa
df_lisboa <- df_amplio %>%
  filter(comunidad == "Área Metropolitana de Lisboa")

# Hipotesis de la metodologia DiD
modelo <- feols(
  log(precios) ~ 0 + 
    treated_before + treated_before_trend + 
    control_before + control_before_trend + 
    control_after + control_after_trend +
    log(wages) + log(ir) + log(popdensity) + log(housestock) + log(ageing_ratio),
  data = df_lisboa,
  vcov = "hetero"
)

summary(modelo)
linearHypothesis(modelo, "treated_before_trend = control_before_trend")

df_placebolisboa <- df_placebo %>%
  filter(comunidad == "Área Metropolitana de Lisboa")

modelo_placebo <- feols(
  log(precios) ~ grupo1 * post_placebo +
    log(wages) + log(ir) + log(popdensity) + log(housestock) + log(ageing_ratio) +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre |
    municipio,
  data = df_placebolisboa,
  vcov = "hetero"
)

summary(modelo_placebo)

# 1
modelo_did <- feols(log(precios) ~ grupo1*post + 
                      log(ir) + log(wages) + log(popdensity) + log(ageing_ratio) +
                      log(housestock)  +
                      enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre +
                      y2013 + y2014 + y2015 + y2016 + y2017 + y2018 + y2019
                    | municipio,
                    data = df_lisboa,
                    vcov = "hetero")

summary(modelo_did)

residuos <- residuals(modelo_did)
jarque.bera.test(residuos)

# 2
modelo_did <- feols(log(precios) ~ grupo1*post + 
                      log(ir) + log(wages) + log(popdensity) + log(ageing_ratio) +
                      log(housestock)  +
                      enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre 
                    | municipio,
                    data = df_lisboa,
                    vcov = "hetero")

summary(modelo_did)

residuos <- residuals(modelo_did)
jarque.bera.test(residuos)


# 3
modelo_did <- feols(log(precios) ~ grupo1*post + 
                      log(ir) + log(wages) + log(popdensity) +
                      enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre 
                    | municipio,
                    data = df_lisboa,
                    vcov = "hetero")

summary(modelo_did)

residuos <- residuals(modelo_did)
jarque.bera.test(residuos)

# 4
modelo_did <- feols(log(precios) ~ grupo1*post + 
                      log(ir) + log(wages) + log(popdensity) + log(ageing_ratio) +
                      enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre +
                      y2013 + y2014 + y2014 + y2015 + y2016 + y2017 + y2018 + y2019
                    | municipio,
                    data = df_lisboa,
                    vcov = "hetero")

summary(modelo_did)
residual <- resid(modelo_did)
jarque.bera.test(residual)


# 5
modelo_did <- feols(log(precios) ~ grupo1*post + 
                      log(ir) + log(wages) + log(popdensity)  +
                      enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre 
                    | municipio,
                    data = df_lisboa,
                    vcov = "hetero")

summary(modelo_did)
residual <- resid(modelo_did)
jarque.bera.test(residual)


