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

# Asegúrate de tener el paquete car
if (!require(car)) install.packages("car")
library(car)

# OLS
modelo1 <- lm(
  log(precios) ~ d2015 + d2016 + d2017 + d2018 + d2019 + 
    log(loans) + log(wages) +  + log(housestock)  +
    log(ageing_ratio) + log(crime_rate) + log(ir) + factor(municipio) + factor(year),
  data = df_amplio)


summary(modelo1)
vif(modelo1)

residuos <- residuals(modelo1)
valores_ajustados <- fitted(modelo1)

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


