source("1. Data/00. Sources.R")

df_final <- readRDS("1. Data/df_finaltodo.rds")

# Establecer Grande Lisboa
df_final <- df_final %>%
  filter(municipio == "Cascais" |
           municipio == "Oeiras" |
           municipio == "Amadora" |
           municipio == "Lisboa" |
           municipio == "Sintra" |
           municipio == "Odivelas" |
           municipio == "Loures" |
           municipio == "Mafra" |
           municipio == "Vila Franca de Xira",
         fecha <= as.Date("2019-12-01")) %>%
  mutate(post = ifelse(fecha >= as.Date("2014-08-01"), 1, 0))

# Estudiamos la calidad
completerate <- df_final %>%
  group_by(municipio) %>%
  summarise(precios = sum(!is.na(precios))/n(),
            sales = sum(is.na(sales)/n()),
            loans = sum(is.na(loans)/n()),
            ir = sum(is.na(ir)/n()),
            cost = sum(is.na(cost)/n()),
            wages = sum(is.na(wages)/n()),
            popdensity = sum(is.na(popdensity)/n()),
            popincrease = sum(is.na(popincrease)/n()),
            housestock = sum(is.na(housestock)/n()),
            crime_rate = sum(is.na(crime_rate)/n()),
            ageing_ratio = sum(is.na(ageing_ratio))/n()) %>%
  ungroup() %>%
  na.omit()

# Vemos la presión turistica
dfanual <- readRDS("1. Data/df_finalanual.rds")

turismoGL <- dfanual %>%
  filter(fecha <= as.Date("2019-12-01")) %>%
  filter(municipio == "Cascais" |
           municipio == "Oeiras" |
           municipio == "Amadora" |
           municipio == "Lisboa" |
           municipio == "Sintra" |
           municipio == "Odivelas" |
           municipio == "Loures" |
           municipio == "Mafra" |
           municipio == "Vila Franca de Xira") %>%
  group_by(municipio) %>%
  summarise(poblacion_turistica_equivalente = ((mean(nights, na.rm = TRUE)/365)/mean(population, na.rm = TRUE))*100,
            noches = mean(nights, na.rm = TRUE),
            precio = mean(precios),
            turism = mean(nights, na.rm = TRUE)/mean(popdensity, na.rm = TRUE)) %>%
  mutate(g1 = case_when(
              poblacion_turistica_equivalente >= quantile(poblacion_turistica_equivalente, 0.75) ~ 1,  
              poblacion_turistica_equivalente <= quantile(poblacion_turistica_equivalente, 0.25) ~ 0),
         g2 = case_when(
              noches >= quantile(noches, 0.75) ~ 1,
              noches <= quantile(noches, 0.25) ~ 0),
         g3 = case_when(
           poblacion_turistica_equivalente >= quantile(poblacion_turistica_equivalente, 0.5) ~ 1,  
           poblacion_turistica_equivalente <= quantile(poblacion_turistica_equivalente, 0.5) ~ 0),
         g4 = case_when(
           noches >= quantile(noches, 0.5) ~ 1,
           noches <= quantile(noches, 0.5) ~ 0)
  )%>%
  ungroup()

turismoGL$treated <- factor(turismoGL$treated, labels = c("Control", "Tratado"))

turismoGL <- turismoGL %>%
  select(c(municipio, g1)) %>%
  rename(treated = g1)
           

df_final <- df_final %>%
  left_join(turismoGL, by = "municipio") 

saveRDS(df_final, "2. Scripts/modellisboa.rds")

# Tabla de diferencia de medias
tabladid <- df_final %>%
  group_by(treated, post) %>%
  summarise(preciosmedio = mean(precios)) %>%
  ungroup()

write.xlsx(tabladid, "2. Scripts/tablas/tabladidlisboa.xlsx")

t.test(precios ~ treated, data = df_final %>% filter(post== 0))

t.test(precios ~ treated, data = df_final %>% filter(post == 1))

t.test(precios ~ post, data = df_final %>% filter(treated == "Tratado"))

t.test(precios ~ post, data = df_final %>% filter(treated == "Control"))

# Grafico temporal
dfgrupo <- df_final %>%
  group_by(fecha, treated) %>%
  summarise(precios = mean(precios)) %>%
  ungroup()

serietemporallisboa <- ggplot(dfgrupo, aes(x = fecha, y = precios, color = treated)) +
  geom_vline(xintercept = as.Date("2014-08-01"), linetype = "dashed" , color = "grey") +
  geom_line() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom") +
  labs(x = "Fecha",
       y = "Precio mediano por metro cuadrado",
       color = "Grupos") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(caption = "Fuente: Elaboración propia con datos del INE") 

dir.create("2. Scripts/imagenes")
png("2. Scripts/imagenes/1. Serie temporal en lisboa de precios.png", width = 800, height =600)
print(serietemporallisboa)
dev.off()

# Analisis de normalidad de la endogena

normalidad <- ggplot(df_final, aes(x = precios)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(df_final$precios, na.rm = TRUE), 
                            sd = sd(df_final$precios, na.rm = TRUE)), 
                color = "red", size = 1) +
  theme_minimal() +
  labs(title = " ",
       x = "Precios (€/m²)",
       y = "Densidad",
       caption = "Fuente: Elaboración propia con datos del INE") +
  theme(plot.title = element_text(hjust = 0.5))

png("2. Scripts/imagenes/3. Analisis de normalidad en lisboa.png", width = 800, height =600)
print(normalidad)
dev.off()

# Descriptivos por grupos

descriptive <- df_final %>%
  group_by(treated) %>%
  summarise(
    n = n(),
    media_precio = mean(precios),
    sd_precio = sd(precios),
    min_precio = min(precios),
    max_precio = max(precios),
    
    media_wages = mean(wages),
    sd_wages = sd(wages),
    min_wages = min(wages),
    max_wages = max(wages),
    
    media_ir = mean(ir),
    sd_ir = sd(ir),
    min_ir = min(ir),
    max_ir = max(ir),
    
    media_pop = mean(popdensity),
    sd_pop = sd(popdensity),
    min_pop = min(popdensity),
    max_pop = max(popdensity),
     
    media_hs = mean(housestock),
    sd_hs = sd(housestock),
    min_hs = min(housestock),
    max_hs = max(housestock),
    
    media_pop = mean(ageing_ratio),
    sd_pop = sd(ageing_ratio),
    min_pop = min(ageing_ratio),
    max_pop = max(ageing_ratio),
     )

write.xlsx(descriptive, "2. Scripts/tablas/descriptivos.xlsx")

saveRDS(df_final, "2. Scripts/dflisboamodelo.rds")

descriptive <- df_final %>%
  group_by(municipio) %>%
  summarise(
    media_precio = mean(precios),

    media_wages = mean(wages),

    media_ir = mean(ir),

    media_pop = mean(popdensity),

    media_hs = mean(housestock),

    media_pop = mean(ageing_ratio)
  ) %>%
  ungroup()
