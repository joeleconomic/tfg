source("1. Data/00. Sources.R")

df_amplio <- readRDS("2. Scripts/dfamplio.rds")

dir.create("3. Results/tablas")

# Veamos si se cumple el criterio poblacion turistica
df_amplio <- df_amplio %>%
  arrange(municipio, fecha) %>%
  filter(grupo1 == 1 | grupo1 == 0) %>%
  filter(!municipio %in% c("Oliveira de Azeméis", "Trofa", "Espinho"),
         fecha <= as.Date("2019-12-01"),
         fecha >= as.Date ("2011-01-01")) %>%
  group_by(municipio) %>%
  mutate(post = ifelse(fecha >= as.Date("2014-09-01"), 1, 0),
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
         d2019 = y2019*grupo1,
         
         dwelling = dwellings + 0.00000001,
         
         DiD= grupo1*post
  ) %>%
  ungroup()


# Sin fijar
modelo_gls <- gls(
  log(precios) ~ grupo1*post +
    log(housestock)  + 
    log(loans) + log(wages) + log(popdensity) + log(dwelling) + 
    log(ir),
  correlation = corARMA(p = 2, q = 0, form = ~ time | municipio),
  weights = varIdent(form = ~1 | municipio),
  data = df_amplio)

summary(modelo_gls)


# Fijar municipio
modelo_gls <- gls(
  log(precios) ~ 0 + DiD +
    log(housestock)  + log(loans) + log(wages) + log(popdensity) + log(dwelling) + 
    log(ir) + factor(municipio),
  correlation = corARMA(form = ~ time | municipio, p = 1, q = 0),
  weights = varIdent(form = ~1 | municipio),
  data = df_amplio)

summary(modelo_gls)
vif(modelo_gls)
residuos <- resid(modelo_gls, type = "normalized")  # residuos normalizados
ajustados <- fitted(modelo_gls)
plot(ajustados, residuos,
     xlab = "Valores ajustados",
     ylab = "Residuos normalizados",
     main = "Residuos vs Ajustados")
abline(h = 0, col = "red")
qqnorm(residuos)
qqline(residuos, col = "red")
acf(residuos, main = "ACF de los residuos normalizados")
shapiro.test(residuos)  # Para muestras pequeñas < 5000

# Fijar municipio y mes
modelo_gls <- gls(
  log(precios) ~ 0 + DiD +
    log(housestock)  + log(crime_rate) +
    log(loans) + log(wages) + log(popdensity) + log(dwelling) + 
    log(ir) + factor(municipio) + factor(mes),
  correlation = corAR1(form = ~ time | municipio),
  weights = varIdent(form = ~1 | municipio),
  data = df_amplio)

summary(modelo_gls)

# Fijar municipio, mes y año
modelo_gls <- gls(
  log(precios) ~ 0 + DiD +
    log(housestock)  + 
    log(loans) + log(wages) + log(popdensity) + log(dwelling) + 
    log(ir) + factor(municipio) + factor(mes) + factor(year),
  correlation = corAR1(form = ~ time | municipio),
  weights = varIdent(form = ~1 | municipio),
  data = df_amplio)

summary(modelo_gls)

#Modelo con did anuales
modelo_gls <- gls(
  log(precios) ~ 0 + d2015 + d2016 + d2017 + d2018 + d2019 +
    log(housestock)  +log(wages)   + 
    log(ir),
  correlation = corAR1(form = ~ time | municipio),
  weights = varIdent(form = ~1 | municipio),
  data = df_amplio)

summary(modelo_gls)

# buen modelo
modelo <- gls(
  log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + post +
    log(loans) + log(wages) + log(housestock) +
    log(ageing_ratio) + log(crime_rate) +
    factor(municipio) + factor(year),
  correlation = corAR1(form = ~ time | municipio),
  weights = varIdent(form = ~1 | municipio),
  data = df_amplio)

summary(modelo)

vcov_hac <- vcovHAC(modelo, prewhite = TRUE, adjust = TRUE)
coeftest(modelo, vcov = vcov_hac)

residuos <- residuals(modelo)
ajustados <- fitted(modelo)

plot(ajustados, residuos,
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = "Residuos vs Valores Ajustados")
abline(h = 0, col = "red")

qqnorm(residuos)
qqline(residuos, col = "red")

acf(residuos, main = "ACF de residuos")

jarque.bera.test(residuos)

bptest(modelo)

dwtest(modelo)