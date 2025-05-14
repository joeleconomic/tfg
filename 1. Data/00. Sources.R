# Instalamos las librerias necesarias para el propyecto
# Manipulacion de datos
install.packages("tidyverse") # paquete basico
install.packages("skimr") # resumenes estadisticos
install.packages("openxlsx") # trabajar con excels

# Modelizacion
install.packages("fixest") # modelos datos panel
install.packages("lmtest") # pruebas estadisticas
install.packages("dynlm") # Permite incluir retardos
install.packages("tseries") # Analisis temporal
install.packages("broom") # modelos a dataframes
install.packages("spdep")
install.packages("spatialreg")
install.packages("splm")

# Accesibilidad a datos
install.packages("remotes") # acceso a datos de github
remotes::install_github("c-matos/ineptR") # acceso a datos del INEP

# Datos espaciales
install.packages("sf") # Manejo de datos espaciales en simple features
install.packages("geodata") # Descarga datos geograficos
install.packages("terra") # Manejo de datos espaciales

#Cargar libreria
library(tidyverse)
library(skimr)
library(openxlsx)

library(fixest) 
library(lmtest) 
library(dynlm) 
library(tseries)
library(broom)
library(spdep)
library(spatialreg)
library(splm)
library(ineptR)

library(sf)
library(geodata)
library(terra)

install.packages("plm")
library(plm)

install.packages("car")  
library(car)

install.packages("systemfit")
library(systemfit)

install.packages("modelsummary")
library(modelsummary)

install.packages("stargazer")
library(stargazer)

install.packages("pandoc")
library(pandoc)