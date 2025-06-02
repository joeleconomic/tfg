source("1. Data/00. Sources.R")

df <- read_rds("1. Data/df_finalanual.rds")

qualityc <- df %>%
  group_by(comunidad) %>%
  summarise(complete_rate = sum(!is.na(precios))/n()) %>%
  ungroup() %>%
  na.omit()

df <- df %>%
  filter(comunidad == "Área Metropolitana de Lisboa" |
         comunidad == "Área Metropolitana do Porto") 

dfa <- df %>%
  group_by(municipio) %>%
  summarise(poblacion_turistica_equivalente = ((mean(nights, na.rm = TRUE)/365)/mean(population, na.rm = TRUE))*100) %>%
  mutate(grupo1 = case_when(
    poblacion_turistica_equivalente >= quantile(poblacion_turistica_equivalente, 0.75, na.rm = TRUE) ~ 1,
    poblacion_turistica_equivalente <= quantile(poblacion_turistica_equivalente, 0.25, na.rm = TRUE) ~ 0)) %>%
  ungroup()
  
df <- df %>%
    left_join(dfa, by = "municipio")
  
df <- df %>%
  arrange(municipio, fecha) %>%
  filter(grupo1 == 1 | grupo1 == 0) %>%
  filter(fecha <= 2019,
         fecha >= 2011) %>%
  group_by(municipio) %>%
  mutate(post = ifelse(fecha >= 2015, 1, 0),
         time = row_number(),
         
         treated_before = ifelse(grupo1 == 1 & post == 0, 1, 0),
         treated_before_trend = ifelse(grupo1 == 1 & post == 0, time, 0),
         
         control_before = ifelse(grupo1 == 0 & post == 0, 1, 0),
         control_before_trend = ifelse(grupo1 == 0 & post == 0, time, 0),
         
         control_after = ifelse(grupo1 == 0 & post == 1, 1, 0),
         control_after_trend = ifelse(grupo1 == 0 & post == 1, time, 0),
        
         
         y2012 = as.integer(fecha == 2012),
         y2013 = as.integer(fecha == 2013),
         y2014 = as.integer(fecha == 2014),
         y2015 = as.integer(fecha == 2015),
         y2016 = as.integer(fecha == 2016),
         y2017 = as.integer(fecha == 2017),
         y2018 = as.integer(fecha == 2018),
         y2019 = as.integer(fecha == 2019),
         
         d2013 = y2013*grupo1,
         d2014 = y2014*grupo1,
         d2015 = y2015*grupo1,
         d2016 = y2016*grupo1,
         d2017 = y2017*grupo1,
         d2018 = y2018*grupo1,
         d2019 = y2019*grupo1,
         
         DiD= grupo1*post
  ) %>%
  ungroup()

modelo <- feols(
  log(precios) ~ 0 + 
    treated_before + treated_before_trend + 
    control_before + control_before_trend + 
    control_after + control_after_trend +
    log(loans) + log(wages) + log(housestock) +
    log(ageing_ratio) + log(crime_rate) 
  | municipio ,
  data = df,
  vcov = ~ municipio + time)

summary(modelo)

linearHypothesis(modelo, "treated_before_trend = control_before_trend")


modelo <- feols(
  log(precios) ~ grupo1*post +
    log(loans) + log(wages) + log(housestock) +
    log(ageing_ratio) + log(crime_rate) 
  | municipio ,
  data = df,
  vcov = ~ municipio + fecha)

summary(modelo)





modelo <- feols(
  log(precios) ~ 0 + 
    treated_before + treated_before_trend + 
    control_before + control_before_trend + 
    control_after + control_after_trend +
    log(loans) + log(wages) + log(popdensity) + log(housestock) + dwellings.x +
    log(ageing_ratio) + log(crime_rate) +
    y2012 + y2013 + y2014 + y2015+ y2016 + y2017 + y2018 + y2019
  | municipio,
  data = df,
  vcov = ~ municipio + time)

summary(modelo)

linearHypothesis(modelo, "treated_before_trend = control_before_trend")
