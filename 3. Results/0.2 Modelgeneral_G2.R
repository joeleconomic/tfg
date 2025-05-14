source("1. Data/00. Sources.R")

df_amplio <- readRDS("2. Scripts/dfamplio.rds")

# Veamos si se cumple el criterio poblacion turistica
df_amplio <- df_amplio %>%
  arrange(municipio, fecha) %>%
  filter(grupo2 == 1 | grupo2 == 0) %>%
  filter(!municipio %in% c("Alcochete", "Arouca", "Oliveira de Azeméis", "Trofa", "Vale de Cambra"),
         fecha <= as.Date("2019-12-01"),
         fecha >= as.Date("2013-01-01")) %>%
  group_by(municipio) %>%
  mutate(post = ifelse(fecha >= as.Date("2014-08-01"), 1, 0),
         time = row_number(),
         
         treated_before = ifelse(grupo2 == 1 & post == 0, 1, 0),
         treated_before_trend = ifelse(grupo2 == 1 & post == 0, time, 0),
         
         control_before = ifelse(grupo2 == 0 & post == 0, 1, 0),
         control_before_trend = ifelse(grupo2 == 0 & post == 0, time, 0),
         
         control_after = ifelse(grupo2 == 0 & post == 1, 1, 0),
         control_after_trend = ifelse(grupo2 == 0 & post == 1, time, 0),
         
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
  ungroup()

modelo <- feols(
  log(precios) ~ 0 + 
    treated_before + treated_before_trend + 
    control_before + control_before_trend + 
    control_after + control_after_trend +
    log(wages) + log(ir) + log(popdensity) + log(housestock) + log(ageing_ratio) +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre,
  data = df_amplio,
  vcov = "hetero"
)

summary(modelo)

linearHypothesis(modelo, "treated_before_trend = control_before_trend")
# Se cumplen las tendencias paralelas

# Veamos si se cumple el test de placebo
df_placebo <- df_amplio %>%
  mutate(post_placebo = ifelse(fecha >= as.Date("2013-08-01"), 1, 0)) %>%
  filter(fecha <= as.Date("2014-08-01"),
         fecha >= as.Date("2013-01-01"))

modelo_placebo <- feols(
  log(precios) ~ grupo2 * post_placebo +
    log(wages) + log(ir) + log(popdensity) + log(housestock) + log(ageing_ratio) +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre |
    municipio,
  data = df_placebo,
  vcov = "hetero"
)

summary(modelo_placebo)

# Matriz de correlaciones
# Seleccionar solo variables numéricas relevantes

df_amplio <- df_amplio %>%
  filter(fecha >= as.Date("2013-01-01"))

# 1
modelo_did <- feols(log(precios) ~ grupo2*post + 
                      log(ir) + log(wages) + log(popdensity) + log(ageing_ratio) + 
                      log(sales) + log(cost) +
                      log(housestock) +
                      enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre 
                    | municipio,
                    data = df_amplio,
                    vcov = "hetero")

summary(modelo_did)

residuos <- residuals(modelo_did)
jarque.bera.test(residuos)


# 2
modelo_did <- feols(log(precios) ~ grupo2*post + 
                      log(ir) + log(wages) + log(popdensity) + log(ageing_ratio) +
                      log(housestock) +
                      enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre 
                    | municipio,
                    data = df_amplio,
                    vcov = "hetero")

summary(modelo_did)

residuos <- residuals(modelo_did)
jarque.bera.test(residuos)

# 3
modelo_did <- feols(log(precios) ~ grupo2*post + 
                      log(ir) + log(wages) + log(popdensity) + log(ageing_ratio) +
                      enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre 
                    | municipio,
                    data = df_amplio,
                    vcov = "hetero")

summary(modelo_did)

residuos <- residuals(modelo_did)
jarque.bera.test(residuos)

# 4
modelo_did <- feols(log(precios) ~ grupo2*post + 
                      log(ir) + log(wages) + log(popdensity) + log(ageing_ratio) +
                      log(housestock) +
                      enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre 
                    | municipio,
                    data = df_amplio,
                    vcov = "hetero")

summary(modelo_did)

residuos <- residuals(modelo_did)
jarque.bera.test(residuos)

ggplot(df_amplio) +
  geom_line(aes(x = fecha, y = wages, color = municipio))

# Veamos solo el comportamiento de Lisboa
df_lisboa <- df_amplio %>%
  filter(comunidad == "Área Metropolitana de Lisboa")

# Hipotesis de la metodologia DiD
modelo <- feols(
  log(precios) ~ 0 + 
    treated_before + treated_before_trend + 
    control_before + control_before_trend + 
    control_after + control_after_trend +
    log(wages) + log(ir) + log(popdensity) + log(housestock) + log(ageing_ratio) +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre,
  data = df_lisboa,
  vcov = "hetero"
)

summary(modelo)
linearHypothesis(modelo, "treated_before_trend = control_before_trend")

df_placebolisboa <- df_placebo %>%
  filter(comunidad == "Área Metropolitana de Lisboa")

modelo_placebo <- feols(
  log(precios) ~ grupo2 * post_placebo +
    log(wages) + log(ir) + log(popdensity) + log(housestock) + log(ageing_ratio) +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre |
    municipio,
  data = df_placebolisboa,
  vcov = "hetero"
)

summary(modelo_placebo)

# 1
modelo_did <- feols(log(precios) ~ grupo2*post + 
                      log(ir) + log(wages) + log(popdensity) + log(ageing_ratio) +
                      log(housestock) +
                      enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre +
                      y2014 + y2014 + y2015 + y2016 + y2017 + y2018 + y2019
                    | municipio,
                    data = df_lisboa,
                    vcov = "hetero")

summary(modelo_did)
residuos <- residuals(modelo_did)
jarque.bera.test(residuos)

# 2
modelo_did <- feols(log(precios) ~ grupo2*post + 
                      log(ir) + log(wages) + log(popdensity) + log(ageing_ratio) +
                      log(housestock) +
                      enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre +
                      y2014 + y2014 + y2015 + y2016 + y2017 + y2018 + y2019
                    | municipio,
                    data = df_lisboa,
                    vcov = "hetero")

summary(modelo_did)
residuos <- residuals(modelo_did)
jarque.bera.test(residuos)

# 3
modelo_did <- feols(log(precios) ~ grupo2*post + 
                      log(ir) + log(wages) + log(popdensity) + log(ageing_ratio) +
                      enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre
                    | municipio,
                    data = df_lisboa,
                    vcov = "hetero")

summary(modelo_did)
residuos <- residuals(modelo_did)
jarque.bera.test(residuos)

# 4
modelo_did <- feols(log(precios) ~ grupo2*post + 
                      log(ir) + log(wages) + log(popdensity) + log(ageing_ratio) +
                      enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre
                    | municipio,
                    data = df_lisboa,
                    cluster = ~municipio)

summary(modelo_did)
residuos <- residuals(modelo_did)
jarque.bera.test(residuos)

# 5
modelo_did <- feols(log(precios) ~ grupo2*post + 
                      log(ir) + log(wages) + log(popdensity) +
                      enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre
                    | municipio,
                    data = df_lisboa,
                    cluster = ~municipio)

summary(modelo_did)
residuos <- residuals(modelo_did)
jarque.bera.test(residuos)

# 6
modelo_did <- feols(log(precios) ~ grupo2*post + 
                      log(ir) + log(wages) + log(popdensity) +
                      enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre
                    | municipio,
                    data = df_lisboa,
                    vcov = "hetero")

summary(modelo_did)
residuos <- residuals(modelo_did)
jarque.bera.test(residuos)


