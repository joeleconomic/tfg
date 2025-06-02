source("1. Data/00. Sources.R")

df_amplio <- readRDS("2. Scripts/dfamplio.rds")

dir.create("3. Results/tablas")

# Veamos si se cumple el criterio poblacion turistica
df_amplio <- df_amplio %>%
  arrange(municipio, fecha) %>%
  filter(grupo1 == 1 | grupo1 == 0) %>%
  filter(!municipio %in% c("Oliveira de Azeméis", "Trofa", "Espinho"),
         fecha <= as.Date("2019-12-01"),
         fecha >= as.Date ("2013-01-01")) %>%
  group_by(municipio) %>%
  mutate(post = ifelse(fecha >= as.Date("2014-09-01"), 1, 0),
         time = row_number(),
         
         treated_before = ifelse(grupo1 == 1 & post == 0, 1, 0),
         treated_before_trend = ifelse(grupo1 == 1 & post == 0, time, 0),
         treated_after_trend = ifelse(grupo1 == 1 & post == 1, time, 0),
        
         
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
         
         d2013 = y2013*grupo1,
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

# general 1
modeloa <- feols(
  log(precios) ~ 0 + 
    treated_before + treated_before_trend  + 
    control_before + control_before_trend + 
    control_after_trend + 
    log(loans) + log(wages) + log(housestock)  +
    log(ageing_ratio) + log(crime_rate) + log(ir) + log(ir) | municipio + year,
  data = df_amplio,
  vcov = ~ municipio + time)

summary(modeloa)

linearHypothesis(modeloa, "treated_before_trend = control_before_trend")

modelo1 <- feols(
  log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
    log(loans) + log(wages) +  + log(housestock)  +
    log(ageing_ratio) + log(crime_rate) + log(ir) | municipio + year,
  data = df_amplio,
  vcov = ~ municipio + time)

summary(modelo1)

residuos <- resid(modelo1)  
ajustados <- fitted(modelo1)
plot(ajustados, residuos,
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = " ")
abline(h = 0, col = "red")

qqnorm(residuos)
qqline(residuos, col = "red")

plot(modelo1, which = 3)

resettest(modelo1)

acf(residuos, main = "ACF de los residuos normalizados")

jarque.bera.test(residuos)  
shapiro.test(residuos)  

bptest(modelo1)
coeftest(modelo1, vcov = vcovHC(modelo1, type = "HC1"))

dwtest(modelo1)

tidy_modelo1 <- broom::tidy(modelo1, conf.int = TRUE)

coef_did <- tidy_modelo1 %>%
  filter(term %in% c("d2015", "d2016", "d2017", "d2018", "d2019")) %>%
  mutate(year = as.numeric(str_remove(term, "d")))

ggplot(coef_did, aes(x = year, y = estimate*100)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low*100, ymax = conf.high*100), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = " ",
       x = "Año",
       y = "Impacto estimado (%)",
       caption = "Intervalos de confianza al 95%
       Fuente: Elaboración propia a partir del INE") +
  theme_minimal()


#2
modelo2 <- feols(
  log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 +
    log(wages)  + log(housestock) + log(ageing_ratio) +
    log(crime_rate) + log(ir) | municipio + year,
  data = df_amplio,
  vcov = ~ municipio + time)

summary(modelo2)

modelob <- feols(
  log(precios) ~ 0 + 
    treated_before + treated_before_trend + 
    control_before + control_before_trend + 
    control_after + control_after_trend +
    log(wages)  + log(housestock) + log(ageing_ratio) +
    log(crime_rate) + log(ir) | municipio + year,
  data = df_amplio,
  vcov = ~ municipio + time)

summary(modelob)
linearHypothesis(modelob, "treated_before_trend = control_before_trend")

# 3
modeloc <- feols(
  log(precios) ~ 0 + 
    treated_before + treated_before_trend + 
    control_before + control_before_trend + 
    control_after + control_after_trend +
    log(loans)  + log(popdensity) + log(housestock) +
    log(crime_rate) + log(ir) | municipio + year,
  data = df_amplio,
  vcov = ~ municipio + time)

summary(modeloc)

linearHypothesis(modeloc, "treated_before_trend = control_before_trend")

modelo3 <- feols(
  log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
    log(popdensity) + log(ir) + log(housestock) + log(crime_rate) + 
    log(loans) | municipio + year,
  data = df_amplio,
  vcov = ~ municipio + time)

summary(modelo3)

modelsummary(list("1" = modelo1, "2" = modelo2, "3" = modelo3),
             stars = TRUE,
             statistic = "p.value",
             output = "3. Results/modelosfinales.docx")

modelsummary(list("Modelo 1" = modeloa, "Modelo 2" = modelob, "Modelo 3" = modeloc),
             statistic = "p.value",
             stars = TRUE,
             output = "3. Results/tendparalelas.docx")


# corrigiendo las observaciones influyentes, se mejoran los contrastes pero no cambian mucho
# Crear dummies inicializadas en 0
df_amplio <- df_amplio %>%
  mutate(
    influ_154 = 0,
    influ_678 = 0,
    influ_855 = 0,
    influ_1259 = 0,
    influ_1260 = 0
  )

# Asignar 1 en las observaciones correspondientes
df_amplio$influ_154[154] <- 1
df_amplio$influ_678[678] <- 1
df_amplio$influ_855[855] <- 1
df_amplio$influ_1259[1259] <- 1
df_amplio$influ_1260[1260] <- 1

# Volver a estimar el modelo incluyendo estas dummies
modelo_con_dummies <- feols(
  log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 +
    log(loans) + log(wages) + log(housestock) +
    log(ageing_ratio) + log(crime_rate) + log(ir) +
    influ_154 + influ_678 + influ_855 + influ_1259 + influ_1260 | municipio + year,
  data = df_amplio,
  vcov = ~ municipio + time
)

# Mostrar resumen para comparar
summary(modelo_con_dummies)

# hipotesis
modelo1 <- lm(
  log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
    log(loans) + log(wages) +  + log(housestock)  +
    log(ageing_ratio) + log(crime_rate) + log(ir) + 
    influ_154 + influ_678 + influ_855 + influ_1259 + influ_1260 +
    factor(municipio) + factor(year),
  data = df_amplio)


summary(modelo1)
vif(modelo1)

residuos <- residuals(modelo1)
valores_ajustados <- fitted(modelo1)

modelo <- feols(
  log(precios) ~ 0 + 
    treated_before + treated_before_trend + 
    control_before + control_before_trend + 
    control_after + control_after_trend +
    log(loans)  + log(housestock)  +
    log(popdensity) +log(crime_rate) + log(ir) +
    influ_154 + influ_678 + influ_855 + influ_1259 + influ_1260 | municipio + year,
  data = df_amplio,
  vcov = ~ municipio + time)

summary(modelo)

linearHypothesis(modelo, "treated_before_trend = control_before_trend")
#Empeoran las tendencias paralelas
# Graficos
# Heterocedasticidad
plot(ajustados, residuos,
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = "Residuos vs Valores Ajustados")
abline(h = 0, col = "red")

plot(modelo1, which = 3)

residuos_studentizados <- rstudent(modelo1)
plot(valores_ajustados, residuos_studentizados, main = "Residuos studentizados")
abline(h = c(-2, 0, 2), col = "red", lty = c(2, 1, 2))

#Normalidad
qqnorm(residuos)
qqline(residuos, col = "red")

hist(residuos, breaks = 25, freq = FALSE, 
     main = "Histograma de los residuos",
     xlab = "Residuos", col = "lightgray", border = "white")
lines(density(residuos), col = "blue", lwd = 2)
curve(dnorm(x, mean = mean(residuos), sd = sd(residuos)), 
      col = "red", lwd = 2, add = TRUE)
legend("topright", legend = c("Distribución empírica", "Distribución teórica"), 
       col = c("blue", "red"), lwd = 2, bty = "n")

#Autocorrelacion
acf(residuos, main = "ACF de residuos")
pacf(residuos, main = "ACF de residuos")

# Contrastes
# Heterocedasticidad
bptest(modelo1)

# Normalidad
shapiro.test(residuos)

jarque.bera.test(residuos)

ks.test(residuos, "pnorm", mean(residuos), sd(residuos))
lillie.test(residuos)

install.packages("nortest")
library(nortest)
ad.test(residuos)

cvm.test(residuos)

# Autocorrelacion

dwtest(modelo1)
bgtest(modelo1, order = 1)
bgtest(modelo1, order = 2)

# Especificacion fun
resettest(modelo1)

#Outlier
rstudent(modelo1)
hatvalues(modelo1)

cooks.distance(modelo1)
plot(cooks.distance(modelo1), type = "h")

influencePlot(modelo1)

#VIF
vif(modelo1)


# IV
vars <- df_amplio %>%
  mutate(
    Prestamos = log(loans),
    Salarios = log(wages),
    Stock_de_vivienda = log(housestock),
    Densidad_poblacional = log(popdensity),
    Ratio_envejecimiento = log(ageing_ratio),
    Ratio_criminalidad = log(crime_rate),
    Tipo_de_interes = log(ir),
  
    residuos = residuos
  ) %>%
  select(Prestamos, Salarios, Stock_de_vivienda, Densidad_poblacional,
         Ratio_envejecimiento, Ratio_criminalidad, Tipo_de_interes, residuos)

cor <- cor(vars)

write.xlsx(cor, "3. Results/Endogeneity.xlsx")

cor.mtest <- function(mat, conf.level = 0.9) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      test <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- test$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# Calcular correlaciones y p-values
cor_matrix <- cor(vars, use = "complete.obs")
p_values <- cor.mtest(vars)

corrplot(cor_matrix,
         method = "color",
         type = "lower",
         order = "original",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 65,
         p.mat = p_values) 

# PLot covariables
graficos <- df_amplio %>%
  group_by(year) %>%
  summarise(
    Precios = mean(precios, na.rm = TRUE),
    Préstamos = mean(loans, na.rm = TRUE),
    Tipo_de_interes = mean(ir, na.rm = TRUE),
    Salarios = mean(wages, na.rm = TRUE),
    Stock_de_vivienda = mean(housestock, na.rm = TRUE),
    Densidad_poblacional = mean(popdensity, na.rm = TRUE),
    Ratio_envejecimiento = mean(ageing_ratio, na.rm = TRUE),
    Ratio_criminalidad = mean(crime_rate, na.rm = TRUE),
  ) %>%
  pivot_longer(-year, names_to = "variable", values_to = "media") %>%
  ggplot(aes(x = year, y = media)) +
  geom_line(linewidth = 0.8, color = "steelblue") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = " ",
       x = "Año", y = "Media anual",
       caption = "Fuente: Elaboración propia a partir del INE") +
  theme_minimal()

plot(graficos)

stock <- df_amplio %>%
  filter(!municipio == "Lisboa") %>%
  group_by(municipio, year) %>%
  summarise(stock = mean(housestock)) 

ggplot(stock, aes(x = year, y = stock, color = municipio)) +
  geom_line()

# Endogeneidad
df_amplio$residuos <- residuos

modelo_residuos <- feols(residuos ~ d2015 + d2016 + d2017 + d2018 + d2019 +
                        log(loans) + log(wages) + log(housestock)  +
                        log(ageing_ratio) + log(crime_rate) + log(ir) | municipio + year,
                      data = df_amplio,
                      vcov = ~municipio + time)
summary(modelo_residuos)

modelsummary(modelo_residuos,
             stars = TRUE,
             statistic = "p.value",
             output = "3. Results/endo.docx")


nw_vcov <- NeweyWest(modelo_residuos, lag = 1, prewhite = FALSE)

# Mostrar coeficientes con errores robustos
coeftest(modelo_residuos, vcov = nw_vcov)