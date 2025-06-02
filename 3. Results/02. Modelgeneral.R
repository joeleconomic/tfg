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
  vcov = ~ municipio + time)

summary(modelo)
modelsummary(modelo, stars = TRUE, statistic = "p.value", outp = "3. Results/tablas/tendendiasparalelas.docx")

linearHypothesis(modelo, "treated_before_trend = control_before_trend")
# Se cumplen las tendencias paralelas

modelo <- feols(
  log(precios) ~ 0 + 
    treated_before + treated_before_trend + 
    control_before + control_before_trend + 
    control_after + control_after_trend +
    log(housestock) + log(ageing_ratio) +
    time +
    enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre +
    y2014 + y2014 + y2015 + y2016 + y2017 + y2018 + y2019
  | municipio,
  data = df_amplio,
  vcov = ~ municipio + time)

summary(modelo)

linearHypothesis(modelo, "treated_before_trend = control_before_trend")
# Se cumplen las tendencias paralelas

# Veamos si se cumple el test de placebo
df_placebo <- df_amplio %>%
  mutate(post_placebo = ifelse(fecha >= as.Date("2013-08-01"), 1, 0)) %>%
  filter(fecha <= as.Date("2014-09-01"),
         fecha >= as.Date("2011-01-01"))

modelo_placebo <- feols(
  log(precios) ~ grupo1 * post_placebo +
  log(loans) + log(wages) + log(popdensity) + log(housestock) + log(dwellings) +
  log(ageing_ratio) + log(crime_rate) + log(ir) +
  enero + febrero + marzo + abril + mayo + junio + julio + agosto + septiembre + noviembre +
  y2013 + y2014
  | municipio,
  data = df_placebo,
  vcov = ~ municipio + time)

summary(modelo_placebo)

modelsummary(
  list("Test de tendencias paralelas" = modelo, "Test de placebo" = modelo_placebo),
  stars = TRUE,
  statistic = "p.value",  
  output = "3. Results/tablas/test.docx")


# Matriz de correlaciones
# Seleccionar solo variables numéricas relevantes
variables_cor <- df_amplio %>%
  dplyr::select(precios, ir, loans, wages, popdensity, housestock, 
                ageing_ratio, crime_rate, dwellings, cost, CPI, sales)

cor_matrix <- cor(variables_cor, use = "complete.obs")

variables_long <- variables_cor %>%
  pivot_longer(cols = -precios, names_to = "variable", values_to = "valor")

p <- ggplot(variables_long, aes(x = valor, y = precios)) +
  geom_point(alpha = 0.6, color = "black") +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  theme_minimal() +
  labs(
    x = " ",
    y = "Precios",
    title = " ",
    caption = "Fuente: Elaboración propia con datos del INE"
  )

print(p)
ggsave("2. Scripts/imagenes/scatter.png", plot = p, width = 12, height = 9)

epsilon <- 1e-12
variables_log <- variables_cor %>%
  mutate(across(everything(), ~ log(. + epsilon)))
variables_long <- variables_log %>%
  pivot_longer(cols = -precios, names_to = "variable", values_to = "valor")
p <- ggplot(variables_long, aes(x = valor, y = precios)) +
  geom_point(alpha = 0.6, color = "black") +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  theme_minimal() +
  labs(
    x = "Log de variable explicativa",
    y = "Log(precios)",
    title = " ",
    caption = "Fuente: Elaboración propia con datos del INE"
  )
print(p)

# 0 fijando distintas cosas sin tendencia
# 1 fijando distintas cosas y manteniendo las variables y con tendencia

modelo1 <- feols(log(precios) ~ grupo1*post  +
                   log(housestock) +   log(loans) + log(wages) + 
                   log(popdensity) + log(ir)+ 
                   log(ageing_ratio) + log(dwelling) +log(crime_rate)|
                   municipio,
                 data = df_amplio,
                 panel.id = c("municipio", "fecha"),
                 vcov = ~municipio + time)

summary(modelo1)

modelo2 <- feols(log(precios) ~ grupo1*post  +
                   log(housestock) +   log(loans) + log(wages) + 
                   log(popdensity) + log(ir)+ 
                   log(ageing_ratio) + log(dwelling) +log(crime_rate)|
                   municipio + mes,
                 data = df_amplio,
                 panel.id = c("municipio", "fecha"),
                 vcov = ~municipio + time)

summary(modelo2)

modelo3 <- feols(log(precios) ~ grupo1*post  +
                   log(housestock) +   log(loans) + log(wages) + 
                   log(popdensity) + log(ir)+ 
                   log(ageing_ratio) + log(dwelling) +log(crime_rate)|
                   municipio + mes + year,
                 data = df_amplio,
                 panel.id = c("municipio", "fecha"),
                 vcov = ~municipio + time)

summary(modelo3)

modelo4 <- feols(log(precios) ~ grupo1*post +
                   log(housestock) +   log(loans) + log(wages) + 
                   log(popdensity) + log(ir)+ 
                   log(ageing_ratio) + log(dwelling) +log(crime_rate)|
                   municipio + mes + year + time,
                 data = df_amplio,
                 panel.id = c("municipio", "fecha"),
                 vcov = ~municipio + time)

summary(modelo4)

modelo5 <- feols(log(precios) ~ grupo1*post +
                   log(housestock) +   log(loans) + log(wages) + 
                   log(popdensity) + log(ir)+ 
                   log(ageing_ratio) + log(dwelling) +log(crime_rate)|
                   municipio + time,
                 data = df_amplio,
                 panel.id = c("municipio", "fecha"),
                 vcov = ~municipio + time)

summary(modelo5)

# 1 fijando distintas cosas y manteniendo las variables y con tendencia

modelo1 <- feols(log(precios) ~ grupo1*post + time +
                   log(housestock) +   log(loans) + log(wages) + 
                   log(popdensity) + log(ir)+ 
                   log(ageing_ratio) + log(dwelling) +log(crime_rate)|
                   municipio,
                 data = df_amplio,
                 panel.id = c("municipio", "fecha"),
                 vcov = ~municipio + time)

summary(modelo1)

modelo2 <- feols(log(precios) ~ grupo1*post + time +
                   log(housestock) +   log(loans) + log(wages) + 
                   log(popdensity) + log(ir)+ 
                   log(ageing_ratio) + log(dwelling) +log(crime_rate)|
                   municipio + mes,
                 data = df_amplio,
                 panel.id = c("municipio", "fecha"),
                 vcov = ~municipio + time)

summary(modelo2)

modelo3 <- feols(log(precios) ~ grupo1*post + time +
                   log(housestock) +   log(loans) + log(wages) + 
                   log(popdensity) + log(ir)+ 
                   log(ageing_ratio) + log(dwelling) +log(crime_rate)|
                   municipio + mes + year,
                 data = df_amplio,
                 panel.id = c("municipio", "fecha"),
                 vcov = ~municipio + time)

summary(modelo3)

modelo4 <- feols(log(precios) ~ grupo1*post + time +
                   log(housestock) +   log(loans) + log(wages) + 
                   log(popdensity) + log(ir)+ 
                   log(ageing_ratio) + log(dwelling) +log(crime_rate)|
                   municipio + mes + year + time,
                 data = df_amplio,
                 panel.id = c("municipio", "fecha"),
                 vcov = ~municipio + time)

summary(modelo4)

modelsummary(
  list("1" = modelo1, "2" = modelo2, "3" = modelo3, "4" = modelo4),
  stars = TRUE,
  statistic = "p.value",  
  output = "3. Results/fijandodistinto.docx")

# Fijando distinto y did anuales sin tendencia
modelo1 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019  +
                   log(housestock) + log(ageing_ratio) + log(crime_rate) + 
                   log(loans) + log(wages) + log(popdensity) + dwellings + 
                   log(ir)|
                   municipio ,
                 data = df_amplio,
                 panel.id = c("municipio", "time"),
                 vcov = ~municipio + time)

summary(modelo1)

modelo2 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019  +
                   log(housestock) + log(ageing_ratio) + log(crime_rate) + 
                   log(loans) + log(wages) + log(popdensity) + dwellings + 
                   log(ir)|
                   municipio + mes,
                 data = df_amplio,
                 panel.id = c("municipio", "time"),
                 vcov = ~municipio + time)

summary(modelo2)

modelo3 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019  +
                   log(housestock) + log(ageing_ratio) + log(crime_rate) + 
                   log(loans) + log(wages) + log(popdensity) + dwellings + 
                   log(ir)|
                   municipio + year + mes,
                 data = df_amplio,
                 panel.id = c("municipio", "time"),
                 vcov = ~municipio + time)

summary(modelo3)

modelo4 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 +
                   log(housestock) + log(ageing_ratio) + log(crime_rate) + 
                   log(loans) + log(wages) + log(popdensity) + dwellings + 
                   log(ir)|
                   municipio + mes + year + time,
                 data = df_amplio,
                 panel.id = c("municipio", "time"),
                 vcov = ~municipio + time)

summary(modelo4)

modelo5 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 +
                   log(housestock) + log(ageing_ratio) + log(crime_rate) + 
                   log(loans) + log(wages) + log(popdensity) + dwellings + 
                   log(ir)|
                   municipio + time,
                 data = df_amplio,
                 panel.id = c("municipio", "time"),
                 vcov = ~municipio + time)

summary(modelo5)

# 2 fijando distinto y did anuales

modelo1 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + time +
                   log(housestock) + log(ageing_ratio) + log(crime_rate) + 
                   log(loans) + log(wages) + log(popdensity) + dwellings + 
                   log(ir)|
                   municipio ,
                 data = df_amplio,
                 panel.id = c("municipio", "time"),
                 vcov = ~municipio + time)

summary(modelo1)

modelo2 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + time +
                   log(housestock) + log(ageing_ratio) + log(crime_rate) + 
                   log(loans) + log(wages) + log(popdensity) + dwellings + 
                   log(ir)|
                   municipio + mes,
                 data = df_amplio,
                 panel.id = c("municipio", "time"),
                 vcov = ~municipio + time)

summary(modelo2)

modelo3 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + time +
                   log(housestock) + log(ageing_ratio) + log(crime_rate) + 
                   log(loans) + log(wages) + log(popdensity) + dwellings + 
                   log(ir)|
                   municipio + mes + year,
                 data = df_amplio,
                 panel.id = c("municipio", "time"),
                 vcov = ~municipio + time)

summary(modelo3)

modelo4 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + time +
                   log(housestock) + log(ageing_ratio) + log(crime_rate) + 
                   log(loans) + log(wages) + log(popdensity) + dwellings + 
                   log(ir)|
                   municipio + mes + year + time,
                 data = df_amplio,
                 panel.id = c("municipio", "time"),
                 vcov = ~municipio + time)

summary(modelo4)

modelsummary(
  list("1" = modelo1, "2" = modelo2, "3" = modelo3, "4" = modelo4),
  statistic = "p.value",
  stars = TRUE,
  output = "3. Results/didanual.docx")

# 3 Comprobación

#para medio
modelo1 <- feols(log(precios) ~ grupo1*post + time +
                   log(housestock) +   log(loans) + log(wages) + 
                   log(ir)+ 
                   log(ageing_ratio) + log(dwelling)|
                   municipio,
                 data = df_amplio,
                 panel.id = c("municipio", "fecha"),
                 vcov = ~municipio + time)

summary(modelo1)


modelo2 <- feols(log(precios) ~ grupo1*post + time +
                   log(housestock) + log(wages) + 
                   log(ir)+ 
                   log(ageing_ratio) + log(dwelling)|
                   municipio + mes,
                 data = df_amplio,
                 panel.id = c("municipio", "fecha"),
                 vcov = ~municipio + time)

summary(modelo2)

modelo3 <- feols(log(precios) ~ grupo1*post + time +
                   log(housestock)  + 
                   log(ir)+ 
                   log(ageing_ratio) |
                   municipio + mes + year,
                 data = df_amplio,
                 panel.id = c("municipio", "fecha"),
                 vcov = ~municipio + time)

summary(modelo3)

modelo4 <- feols(log(precios) ~ grupo1*post + time +
                   log(housestock)  + 
                   log(ageing_ratio) |
                   municipio + mes + year + time,
                 data = df_amplio,
                 panel.id = c("municipio", "fecha"),
                 vcov = ~municipio + time)

summary(modelo4)

modelsummary(
  list("1" = modelo1, "2" = modelo2, "3" = modelo3, "4" = modelo4),
  statistic = "p.value",
  stars = TRUE,
  output = "3. Results/reducdidmedio.docx")

# para anual
modelo1 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + time +
                   log(housestock) + log(ageing_ratio)  + 
                   log(wages) + log(dwelling)  + 
                   log(ir)|
                   municipio ,
                 data = df_amplio,
                 panel.id = c("municipio", "time"),
                 vcov = ~municipio + time)

summary(modelo1)

modelo2 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + time +
                   log(housestock) + log(ageing_ratio) + 
                   log(wages)  + log(dwelling) + 
                   log(ir)|
                   municipio + mes,
                 data = df_amplio,
                 panel.id = c("municipio", "time"),
                 vcov = ~municipio + time)

summary(modelo2)

modelo3 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + time +
                   log(housestock) + log(ageing_ratio)  + 
                   log(ir)|
                   municipio + mes + year,
                 data = df_amplio,
                 panel.id = c("municipio", "time"),
                 vcov = ~municipio + time)

summary(modelo3)

modelo4 <- feols(log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019  +
                   log(housestock) + log(ageing_ratio)|
                   municipio + mes + year + time,
                 data = df_amplio,
                 panel.id = c("municipio", "time"),
                 vcov = ~municipio + time)

summary(modelo4)

modelsummary(
  list("1" = modelo1, "2" = modelo2, "3" = modelo3, "4" = modelo4),
  statistic = "p.value",
  stars = TRUE,
  output = "3. Results/reducdidanual.docx")

# Modelos OLS
modelo_lm <- lm(log(precios) ~ DiD + log(housestock) + log(loans) + log(wages) +
                      log(popdensity) + log(dwelling) + log(ir) + factor(municipio),
                    data = df_amplio)
summary(modelo_lm)
vif(modelo_lm)


