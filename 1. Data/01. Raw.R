# Llamamos a los paquetes
source("1. Data/00. Sources.R")  


# Obtener datos del INE

# Codificacion: A si los datos son anuales y M si es mensual para los casos que se repitan

# Variable dependiente
medianprices_m <- get_ine_data("0010042") #Median value of bank evaluation (€/ m²) by Geographic localization (Município - 2013) and Type of construction; Monthly 01/2011-11/2023
medianprice_a <- get_ine_data("0010043") #Median value of bank evaluation (€/ m²) by Geographic localization (Município - 2013) and Type of construction; Annual 

# Variables de demanda
pop_density <- get_ine_data("0008337") #Population´s density (No./ km²) by Place of residence (NUTS - 2013); Annual 2004-2022
pop_increase <- get_ine_data("0008263") #Crude rate of natural increase (%) by Place of residence (NUTS - 2013); Annual 2011-2023
pop <- get_ine_data("0008268") #Annual average resident population (No.) by Place of residence (NUTS - 2013), Sex and Age group (By life cycles)
ageing_ratio <- get_ine_data("0008258") #Ageing ratio (No.) by Place of residence (NUTS - 2013); Annual 2011-2023
wages <- get_ine_data("0009047") #Average monthly earnings (€) by Geographic localization (NUTS - 2013); Annual 2011-2022
ir <- get_ine_data("0006340") #Interest rate (Série 2012 - %) on housing loans by Housing loan regime, Financing purpose and Interest payer; Monthly 01/2099-02/2025
sales_m <- get_ine_data("0008653") #Purchase and sale contracts (No.) of real estates by Geographic localization (NUTS - 2013) and Type of building; Monthly 01/2007-12/2019
sales_a <- get_ine_data("0008649") #Purchase and sale contracts (No.) of real estates by Geographic localization (NUTS - 2013) and Type of building; Annual 2004-2019
loans_m <- get_ine_data("0008655") # Loan agreements with conventional mortgage (No.) by Geographic localization (NUTS - 2013) and Type of building; Monthly 01/2007-12/2019
loans_a <- get_ine_data("0008651") # Loan agreements with conventional mortgage (No.) by Geographic localization (NUTS - 2013) and Type of building; Annual 2004-2019

# Variables de oferta
licensed_dwellings_new_construction_a <- get_ine_data("0008308") #Licensed dwellings (No.) in new constructions for family housing by Geographic localization (NUTS - 2013) and Dwelling typology; annual 2002-01/2023
housestock <- get_ine_data("0008329") #Conventional dwellings (Housing stock - No.) by Geographic localization (NUTS - 2013); Annual 2011-2022
cost <- get_ine_data("0009755") #New housing construction cost index (Base - 2015) by Production factor; Monthly 01/2000-12/2022
CPI <- get_ine_data("0008352") #Consumer price index (CPI, Base - 2012) by Geographic localization (NUTS II - 2013) and Individual consumption by purpose; Monthly

# Factores institucionales
crime_rate <- get_ine_data("0008074") #Crime rate (‰) by Geographic localization (NUTS - 2013) and Category of crime; Annual 2011-2022

# Turismo
#local_accomodation <- get_ine_data("0009873") #Tourist accommodation establishments (No.) by Geographic localization (NUTS - 2013) and Type (tourist accommodation establishment); Annual 2017-2023
#hotels_rooms <- get_ine_data("0008578") #Rooms (No.) in hotel establishments by Geographic localization (NUTS - 2013) and Type (hotel establishment); Annual 2014-2017
nights_a <- get_ine_data("0009877") #Nights (No.) in tourist accommodation establishments by Geographic localization (NUTS - 2013) and Type (tourist accommodation establishment); Annual

# Georferenciar datos
portugal_munis <- geodata::gadm(country = "PRT", level = 2, path = tempdir())

# Convertir a sf
portugal_munis_sf <- sf::st_as_sf(portugal_munis)

# Ver cuántos hay
nrow(portugal_munis_sf)  # Debería dar 308

ggplot(data = portugal_munis_sf) +
  geom_sf(fill = "lightblue", color = "gray30", size = 0.2) +
  labs(title = "Mapa de los 308 municipios de Portugal") +
  theme_minimal()


# Guardar datos procesados
dir.create("1. Data/databases")

# Mensuales y municipales
saveRDS(medianprices_m, "1. Data/databases/medianprices_m.rds")
saveRDS(sales_m, "1. Data/databases/sales_m.rds")
saveRDS(loans_m, "1. Data/databases/loans_m.rds")
saveRDS(CPI, "1. Data/databases/CPI.rds")

# Mensuales y nacionales
saveRDS(ir, "1. Data/databases/ir.rds")
saveRDS(cost, "1. Data/databases/cost.rds")

# Anuales y municipales
saveRDS(medianprice_a, "1. Data/databases/medianprices_a.rds")
saveRDS(licensed_dwellings_new_construction_a, "1. Data/databases/licensed_dwellings_new_construction_a.rds")
saveRDS(loans_a, "1. Data/databases/loans_a.rds")
saveRDS(sales_a, "1. Data/databases/sales_a.rds")
saveRDS(wages, "1. Data/databases/wages.rds")
saveRDS(pop_density, "1. Data/databases/pop_density.rds")
saveRDS(pop_increase, "1. Data/databases/pop_increase.rds")
saveRDS(pop, "1. Data/databases/population.rds")
saveRDS(ageing_ratio, "1. Data/databases/ageing_ratio.rds")
saveRDS(housestock, "1. Data/databases/housestock.rds")
saveRDS(crime_rate, "1. Data/databases/crime_rate.rds")
saveRDS(nights_a, "1. Data/databases/nights_a.rds")
saveRDS(portugal_munis_sf, "1. Data/databases/georeferencia.rds")

rm(list = ls())


