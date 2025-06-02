source("1. Data/00. Sources.R")

df_final <- readRDS("1. Data/df_finaltodo.rds")

# Problemas con datos Na, analicemos por nivel geografico
qualityc <- df_final %>%
  filter(fecha <= as.Date("2019-12-01"),
         fecha >= as.Date("2013-01-01")) %>%
  group_by(comunidad) %>%
  summarise(complete_rate = sum(!is.na(precios))/n()) %>%
  ungroup() 


dir.create("2. Scripts/tablas")
write.xlsx(qualityc, "2. Scripts/tablas/calidad.xlsx")

completerateL <- df_final %>%
  filter(comunidad == "Área Metropolitana de Lisboa",
         fecha <= as.Date("2019-12-01"),
         fecha >= as.Date("2013-01-01")) %>%
  group_by(municipio) %>%
  summarise(precios = sum(!is.na(precios))/n(),
            sales = sum(!is.na(sales))/n(),
            loans = sum(!is.na(loans))/n(),
            ir = sum(!is.na(ir))/n(),
            cost = sum(!is.na(cost))/n(),
            wages = sum(!is.na(wages))/n(),
            popdensity = sum(!is.na(popdensity))/n(),
            popincrease = sum(!is.na(popincrease))/n(),
            housestock = sum(!is.na(housestock))/n(),
            crime_rate = sum(!is.na(crime_rate))/n(),
            ageing_ratio = sum(!is.na(ageing_ratio))/n()) %>%
  ungroup() %>%
  na.omit()

dfporto <- df_final %>%
  filter(comunidad == "Área Metropolitana do Porto",
         fecha <= as.Date("2019-12-01"))

completerateP <- df_final %>%
  filter(comunidad == "Área Metropolitana do Porto",
         fecha <= as.Date("2019-12-01"),
         fecha >= as.Date("2013-01-01")) %>%
  group_by(municipio) %>%
  summarise(precios = sum(!is.na(precios))/n(),
            sales = sum(!is.na(sales))/n(),
            loans = sum(!is.na(loans))/n(),
            ir = sum(!is.na(ir))/n(),
            cost = sum(!is.na(cost))/n(),
            wages = sum(!is.na(wages))/n(),
            popdensity = sum(!is.na(popdensity))/n(),
            popincrease = sum(!is.na(popincrease))/n(),
            housestock = sum(!is.na(housestock))/n(),
            crime_rate = sum(!is.na(crime_rate))/n(),
            ageing_ratio = sum(!is.na(ageing_ratio))/n()) %>%
  ungroup()

write.xlsx(completerateP, "2. Scripts/tablas/completerateporto.xlsx")

# Filtramos al área de grande Lisboa

df_final <- df_final %>%
  filter(municipio == "Cascais" |
           municipio == "Oeiras" |
           municipio == "Amadora" |
           municipio == "Lisboa" |
           municipio == "Sintra" |
           municipio == "Odivelas" |
           municipio == "Loures" |
           municipio == "Mafra" |
           municipio == "Vila Franca de Xira")

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

completerate <- df_final %>%
  filter(comunidad == "Área Metropolitana do Porto",
         fecha <= as.Date("2019-12-01"),
         fecha >= as.Date("2013-01-01")) %>%
  group_by(municipio) %>%
  summarise(precios = sum(!is.na(precios))/n(),
            sales = sum(!is.na(sales)/n()),
            loans = sum(!is.na(loans)/n()),
            ir = sum(!is.na(ir)/n()),
            cost = sum(!is.na(cost)/n()),
            wages = sum(!is.na(wages)/n()),
            popdensity = sum(!is.na(popdensity)/n()),
            popincrease = sum(!is.na(popincrease)/n()),
            housestock = sum(!is.na(housestock)/n()),
            crime_rate = sum(!is.na(crime_rate)/n()),
            ageing_ratio = sum(!is.na(ageing_ratio))/n()) %>%
  ungroup() %>%
  arrange(precios, municipio)

# Veamos cuales son los más turísticos
dfanual <- readRDS("1. Data/df_finalanual.rds")

turismoGL <- dfanual %>%
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
            precio = mean(precios)) %>%
  ungroup()

turismol <- dfanual %>%
  filter(comunidad == "Área Metropolitana de Lisboa",
         fecha <= as.Date("2019-12-01")) %>%
  group_by(municipio) %>%
  summarise(poblacion_turistica_equivalente = ((mean(nights, na.rm = TRUE)/365)/mean(population, na.rm = TRUE))*100,
            precio = mean(precios),
            noches = mean(nights, na.rm = TRUE),
            res = mean(population, na.rm= TRUE)) %>%
  mutate(grupo1 = case_when(
          poblacion_turistica_equivalente >= quantile(poblacion_turistica_equivalente, 0.75, na.rm = TRUE) ~ 1,
          poblacion_turistica_equivalente <= quantile(poblacion_turistica_equivalente, 0.25, na.rm = TRUE) ~ 0),
        grupo2 = case_when(
          noches >= quantile(noches, 0.75, na.rm = TRUE) ~ 1,
          noches <= quantile(noches, 0.25, na.rm = TRUE) ~ 0)) %>%
  ungroup()

turismop <- dfanual %>%
  filter(comunidad == "Área Metropolitana do Porto",
         fecha <= as.Date("2019-12-01")) %>%
  group_by(municipio) %>%
  summarise(poblacion_turistica_equivalente = ((mean(nights, na.rm = TRUE)/365)/mean(population, na.rm = TRUE))*100,
            precio = mean(precios),
            noches = mean(nights, na.rm = TRUE)) %>%
  mutate(grupo1 = case_when(
    poblacion_turistica_equivalente >= quantile(poblacion_turistica_equivalente, 0.75, na.rm = TRUE) ~ 1,
    poblacion_turistica_equivalente <= quantile(poblacion_turistica_equivalente, 0.25, na.rm = TRUE) ~ 0),
    grupo2 = case_when(
      noches >= quantile(noches, 0.75, na.rm = TRUE) ~ 1,
      noches <= quantile(noches, 0.25, na.rm = TRUE) ~ 0)) %>%
  ungroup()

grupos <- turismol %>%
  full_join(turismop)

write.xlsx(grupos, "2. Scripts/tablas/gruposamplio.xlsx")

dfamplio <- df_final %>%
  left_join(grupos, by = "municipio")

saveRDS(dfamplio, "2. Scripts/dfamplio.rds")

completerate <- completerate %>%
  select(municipio, precios) %>%
  left_join(turismop, by = "municipio")

write.xlsx(completerate, "2. Scripts/tablas/grupoporto.xlsx")

completerateL <- completerateL %>%
  select(municipio, precios) %>%
  left_join(turismol, by = "municipio")

write.xlsx(completerateL, "2. Scripts/tablas/grupolisboa.xlsx")

# Analisis temporal por grupos
df <- df_final %>%
  left_join(grupos, by = "municipio") %>%
  filter(grupo1 == 1 |
         grupo1 == 0,
         fecha >= as.Date("2013-01-01"),
         fecha <= as.Date("2019-12-01")) %>%
  rename(treated = grupo1) %>%
  filter(!municipio %in% c("Espinho", "Oliveira de Azeméis", "Trofa"))

dfgrupos <- df %>%
  group_by(fecha, treated) %>%
  summarise(precios = mean(precios))

dfgrupos$treated <- factor(dfgrupos$treated, labels = c("Control", "Tratado"))

serietemporal <- ggplot(dfgrupos, aes(x = fecha, y = precios, color = treated)) +
  geom_vline(xintercept = as.Date("2014-09-01"), linetype = "dashed" , color = "grey") +
  geom_line() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom") +
  labs(x = "Fecha",
       y = "Precios (€/m²)",
       color = "Grupos") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(caption = "Fuente: Elaboración propia a partir del INE") 

dir.create("2. Scripts/imagenes")
png("2. Scripts/imagenes/1. Serie temporal de precios.png", width = 800, height =600)
print(serietemporal)
dev.off()

# Analisis de normalidad de la endogena

normalidad <- ggplot(df, aes(x = precios)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(df$precios, na.rm = TRUE), 
                            sd = sd(df$precios, na.rm = TRUE)), 
                color = "red", size = 1) +
  theme_minimal() +
  labs(title = " ",
       x = "Precios (€/m²)",
       y = "Densidad",
       caption = "Fuente: Elaboración propia a partir del INE") +
  theme(plot.title = element_text(hjust = 0.5))

png("2. Scripts/imagenes/3. Analisis de normalidad.png", width = 800, height =600)
print(normalidad)
dev.off()

# Diferencia de medias
df <- df %>%
  group_by(municipio) %>%
  mutate(post = ifelse(fecha >= as.Date("2014-08-01"), 1, 0)) %>%
  ungroup() 

diferenciamedias <- df %>%
  group_by(treated, post) %>%
  summarise(precios = mean(precios)) %>%
  ungroup()

t.test(precios ~ treated, data = df %>% filter(post== 0))

t.test(precios ~ treated, data = df %>% filter(post == 1))

t.test(precios ~ post, data = df %>% filter(treated== 0))

t.test(precios ~ post, data = df %>% filter(treated == 1))

write.xlsx(diferenciamedias, "2. Scripts/tablas/diferenciademediasamplio.xlsx")


# Tabla de descriptivos
tabladescriptivos<- df %>%
  filter(fecha <= as.Date("2019-12-01"),
         fecha >= as.Date("2013-01-01")) %>%
  group_by(treated) %>%
  summarise(
    preciosmedio = mean(precios),
    preciossd = sd(precios),
    preciosmin= min(precios),
    preciosmax = max(precios),
    
    loansmedio = mean(loans),
    loanssd = sd(loans),
    loansmin= min(loans),
    loansmax = max(loans),
    
    salariosmedio = mean(wages),
    salariossd = sd(wages),
    salariosmin= min(wages),
    salariosmax = max(wages),
    
    popdensitymedio = mean(popdensity),
    popdensitysd = sd(popdensity),
    popdensitymin= min(popdensity),
    popdensitymax = max(popdensity),
    
    housestockmedio = mean(housestock),
    housestocksd = sd(housestock),
    housestockmin= min(housestock),
    housestockmax = max(housestock),
    
    dwellingsmedio = mean(dwellings),
    dwellingssd = sd(dwellings),
    dwellingsmin= min(dwellings),
    dwellingsmax = max(dwellings),
    
    ageing_ratiomedio = mean(ageing_ratio),
    ageing_ratiosd = sd(ageing_ratio),
    ageing_ratiomin= min(ageing_ratio),
    ageing_ratiomax = max(ageing_ratio),
    
    crime_ratemedio = mean(crime_rate),
    crime_ratesd = sd(crime_rate),
    crime_ratemin= min(crime_rate),
    crime_ratemax = max(crime_rate),
    
    irmedio = mean(ir),
    irsd = sd(ir),
    irmin= min(ir),
    irmax = max(ir)) %>%
  pivot_longer(
    cols = -treated,
    names_to = c("variable", "medida"),
    names_sep = "_",
    values_to = "valor"
  ) %>%
  pivot_wider(
    names_from = treated,
    names_prefix = "grupo_",
    values_from = valor
  )

write.xlsx(tabladescriptivos, "2. Scripts/tablas/descriptivos.xlsx")

# Tabla de variables en Lisboa
dflisboa <- df_final %>%
  filter(municipio == "Cascais" |
           municipio == "Oeiras" |
           municipio == "Amadora" |
           municipio == "Lisboa" |
           municipio == "Sintra" |
           municipio == "Odivelas" |
           municipio == "Loures" |
           municipio == "Mafra" |
           municipio == "Vila Franca de Xira",
         fecha <= as.Date("2019-12-01")) 

skimtb <- skim(dflisboa)
skimdf <- as.data.frame(skimtb)
write.xlsx(skimdf, "2. Scripts/tablas/deslisboa.xlsx")

turismolisboa <- dfanual %>%
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
            preciomedio = mean(precios)) %>%
  mutate(treated = case_when(
    poblacion_turistica_equivalente >= quantile(poblacion_turistica_equivalente, 0.5, na.rm = TRUE) ~ 1,  
    poblacion_turistica_equivalente < quantile(poblacion_turistica_equivalente, 0.5, na.rm = TRUE) ~ 0
  )) %>%
  ungroup()

dflisboa <- dflisboa %>%
  left_join(turismolisboa, by = "municipio")

saveRDS(dflisboa, "2. Scripts/dflisboa.rds")

# Tabla de variables en Oporto
skim(dfporto)
skim(turismop)
skimtb <- skim(dfporto)
skimdf <- as.data.frame(skimtb)
write.xlsx(skimdf, "2. Scripts/tablas/desporto.xlsx")

# Analisis de normalidad de la endogena

normalidad <- ggplot(df, aes(x = log(precios))) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(df$precios, na.rm = TRUE), 
                            sd = sd(df$precios, na.rm = TRUE)), 
                color = "red", size = 1) +
  theme_minimal() +
  labs(title = " ",
       x = "Precios (€/m²)",
       y = "Densidad",
       caption = "Fuente: Elaboración propia con datos del INE") +
  theme(plot.title = element_text(hjust = 0.5))

png("2. Scripts/imagenes/3. Analisis de normalidad.png", width = 800, height =600)
print(normalidad)
dev.off()

# Diferencia de medias
tabla <- df %>%
  group_by(treated, post) %>%
  summarise(precio = mean(precios)) %>%
  ungroup()

write.xlsx(tabla, "2. Scripts/tablas/medias.xlsx")

t.test(precios ~ treated, data = df %>% filter(post== 0))

t.test(precios ~ treated, data = df %>% filter(post == 1))


# Mapa de colores de precios
dfgeo <- readRDS("1. Data/df_final.rds")

df <- sf::st_as_sf(dfgeo)

dfgeopromedio <- dfgeo %>%
  group_by(municipio) %>%
  summarise(preciospromedio = mean(precios)) %>%
  ungroup()

dfgeopromedio <- st_as_sf(dfgeopromedio)

# Calcular el centroide de cada municipio (centro de la geometría)
municipios_centroides <- st_centroid(dfgeopromedio)

# Unir con los datos 
municipios_centroides <- municipios_centroides %>%
  st_join(dfgeopromedio, by = "municipio")


mapacalor <- ggplot(dfgeopromedio) +
  geom_sf(aes(fill = preciospromedio), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 1000, name = "€/m²") +
  geom_sf_text(data = municipios_centroides, aes(label = municipio), 
               size = 3, color = "black") +
  labs(x= " ", y = " "  ,
       caption = "Fuente: Elaboración propia con datos del INE") +
  theme_minimal()

png("2. Scripts/imagenes/2. Mapa de precios.png", width = 800, height =600)
print(mapacalor)
dev.off()

# Veamos si se cumple las tendencias paralelas
df <- df %>%
  mutate(post = case_when(
          fecha < "2014-08-01" ~ 0,
          fecha >= "2014-08-01" ~ 1),
        month_id = (year(fecha) - 2011) * 12 + month(fecha), 
        rel_month = month_id - ((2014 - 2011) * 12 + 8))

  
model <- feols(precios ~ i(rel_month, treated, ref = 0) | municipio, data = df)

coef_plot <- broom::tidy(model) %>%
  filter(str_detect(term, "rel_month")) %>% 
  mutate(
    rel_month = as.numeric(str_extract(term, "-?\\d+")),
    term = str_extract(term, "rel_month\\d+")
  )

str(coef_plot)
summary(model)

tendenciasparalelas <- ggplot(coef_plot, aes(x = rel_month, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                    ymax = estimate + 1.96 * std.error)) +
  theme_minimal() +
  labs(x = "Meses relativos al tratamiento", 
       y = "Coeficiente",
       caption = "Fuente: Elaboración propia con datos del INE")

png("2. Scripts/imagenes/3. Test de tendencias paralelas Lisboa.png", width = 800, height = 600)
print(tendenciasparalelas)
dev.off()

saveRDS(dfgeo, "2. Scripts/df_model.rds")

# Test de placebo
dfplacebo <- df_final %>%
  filter(fecha <= as.Date("2014-08-01")) %>%
  mutate(post = case_when(
    fecha < "2012-08-01" ~ 0,
    fecha >= "2012-08-01" ~ 1),
    month_id = (year(fecha) - 2011) * 12 + month(fecha), 
    rel_month = month_id - ((2012 - 2011) * 12 + 8),
    treatment = case_when(
      municipio == "Cascais" ~ 1,
      municipio == "Lisboa" ~ 1,
      municipio == "Sintra" ~ 1,
      municipio == "Oeiras" ~ 1,
      municipio == "Amadora" ~ 0,
      municipio == "Odivelas" ~ 0,
      municipio == "Mafra" ~ 0,
      municipio == "Loures" ~ 0,
      municipio == "Vila Franca de Xira" ~ 0))

modelplacebo <- feols(precios ~ i(rel_month, treatment, ref = 0) | municipio, data = dfplacebo)

coef_plot <- broom::tidy(modelplacebo) %>%
  filter(str_detect(term, "rel_month")) %>% 
  mutate(
    rel_month = as.numeric(str_extract(term, "-?\\d+")),
    term = str_extract(term, "rel_month\\d+")
  )

placebotest <- ggplot(coef_plot, aes(x = rel_month, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                    ymax = estimate + 1.96 * std.error)) +
  theme_minimal() +
  labs(x = "Meses relativos al tratamiento", 
       y = "Coeficiente",
       caption = "Fuente: Elaboración propia con datos del INE")

png("2. Scripts/imagenes/5. Test de placebo.png", width = 800, height = 600)
print(placebotest)
dev.off()


# Análisis espacial
dfgeovalid<- df %>%
  select(fecha, municipio, precios, geometry) %>%
  st_make_valid()
  
df_municipios <- dfgeovalid %>% 
  group_by(geometry) %>% 
  slice(1)

vecinos <- poly2nb(df_municipios)

pesos <- nb2listw(vecinos, style = "W", zero.policy = TRUE)

ggplot(df_municipios) +
  geom_sf(aes(fill = precios), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", name = "€/m²") +
  labs(x= " ", y = " "  ,
       caption = "Fuente: Elaboración propia con datos del INE") +
  theme_minimal()

moran_test <- moran.test(df_municipios$precios, pesos, zero.policy = TRUE)
print(moran_test)
local_moran <- localmoran(df_municipios$precios, pesos, zero.policy = TRUE)
print(local_moran)

dfgeovalid <- dfgeovalid %>%
  mutate(Ii = local_moran[, 1],
         p_value = local_moran[, 5],
         cluster = case_when(
           Ii > 0 & precios > mean(precios) & p_value < 0.05 ~ "Alto-Alto",
           Ii > 0 & precios < mean(precios) & p_value < 0.05 ~ "Bajo-Bajo",
           Ii < 0 & precios > mean(precios) & p_value < 0.05 ~ "Alto-Bajo",
           Ii < 0 & precios < mean(precios) & p_value < 0.05 ~ "Bajo-Alto",
           TRUE ~ "No significativo"
         ))

mapalocal <- ggplot(dfgeovalid) +
  geom_sf(aes(fill = cluster), color = "white", size = 0.1) +
  scale_fill_manual(values = c(
    "Alto-Alto" = "#d7191c",
    "Bajo-Bajo" = "#2c7bb6",
    "Alto-Bajo" = "#fdae61",
    "Bajo-Alto" = "#abd9e9",
    "No significativo" = "grey80"
  )) +
  labs(fill = "Cluster LISA",
       title = "Clusters de correlación espacial local (LISA)",
       caption = "Fuente: Elaboración propia con datos del INE") +
  theme_minimal()

png("2. Scripts/imagenes/4. LISA Map.png", width = 800, height = 600)
print(mapalocal)
dev.off()


saveRDS(df, "2. Scripts/df_modelamplio.rds")