# Llamamos a los paquetes
source("1. Data/00. Sources.R")

# Mensuales y municipales
medianprices_m <- readRDS("1. Data/databases/medianprices_m.rds")
sales_m <- readRDS("1. Data/databases/sales_m.rds")
loans_m <- readRDS("1. Data/databases/loans_m.rds")

# Mensuales y nacionales
ir <- readRDS("1. Data/databases/ir.rds")
cost <- readRDS("1. Data/databases/cost.rds")
CPI <- readRDS("1. Data/databases/CPI.rds")

# Anuales y municipales
medianprices_a <- readRDS("1. Data/databases/medianprices_a.rds")
licensed_dwellings_new_construction_a <- readRDS("1. Data/databases/licensed_dwellings_new_construction_a.rds")
sales_a <- readRDS("1. Data/databases/sales_a.rds")
loans_a <- readRDS("1. Data/databases/loans_a.rds")
nights_a <- readRDS("1. Data/databases/nights_a.rds")

wages <- readRDS("1. Data/databases/wages.rds")
pop_density <- readRDS("1. Data/databases/pop_density.rds")
pop_increase <- readRDS("1. Data/databases/pop_increase.rds")
pop <- readRDS("1. Data/databases/population.rds")
ageing_ratio <- readRDS("1. Data/databases/ageing_ratio.rds")
housestock <- readRDS("1. Data/databases/housestock.rds")
crime_rate <- readRDS("1. Data/databases/crime_rate.rds")

georeferencia <- readRDS("1. Data/databases/georeferencia.rds")

#Homogeneizamos las distintas bbdd
medianprices_m <- medianprices_m %>% 
  filter(dim_3_t == "Total") %>%
  select(c(dim_1, geodsg, valor)) %>%
  rename(precios = valor) %>%
  distinct()

medianprices_a <- medianprices_a %>%
  filter(dim_3_t == "Total") %>%
  select(c(dim_1, geodsg, valor)) %>%
  rename(precios = valor) %>%
  distinct()

licensed_dwellings_new_construction_a <- licensed_dwellings_new_construction_a %>% 
  filter(dim_3_t == "Total") %>%
  select(c(dim_1, geodsg, valor)) %>%
  rename(dwellings = valor) %>%
  distinct()

sales_m <- sales_m %>% 
  filter(dim_3_t == "Total") %>%
  select(c(dim_1, geodsg, valor)) %>%
  rename(sales = valor) %>%
  distinct()

sales_a <- sales_a %>% 
  filter(dim_3_t == "Total") %>%
  select(c(dim_1, geodsg, valor)) %>%
  rename(sales = valor) %>%
  distinct()

loans_m <- loans_m %>%
  filter(dim_3_t == "Total") %>%
  select(c(dim_1, geodsg, valor)) %>%
  rename(loans = valor) %>%
  distinct()

loans_a <- loans_a %>%
  filter(dim_3_t == "Total") %>%
  select(c(dim_1, geodsg, valor)) %>%
  rename(loans = valor) %>%
  distinct()

ir <- ir %>% 
  filter(dim_3_t == "Total" &
         dim_4_t == "Total" &
         dim_5_t == "Total" ) %>%
  select(c(dim_1, valor)) %>%
  rename(ir = valor) %>%
  distinct()

cost <- cost %>% 
  filter(dim_3_t == "Total") %>%
  select(c(dim_1, valor)) %>%
  rename(cost = valor) %>%
  distinct()

housestock <- housestock %>%
  select(c(dim_1, geodsg, valor)) %>%
  rename(housestock = valor) %>%
  distinct()

pop_density <-pop_density %>%
  select(c(dim_1, geodsg, valor)) %>%
  rename(popdensity = valor) %>%
  distinct()

pop_increase <- pop_increase %>%
  select(c(dim_1, geodsg, valor)) %>%
  rename(popincrease = valor) %>%
  distinct()

pop <- pop %>%
  filter(dim_3_t == "HM",
         dim_4_t == "Total") %>%
  rename(population = valor) %>%
  select(c(dim_1, geodsg, population)) %>%
  distinct()

wages <- wages %>%
  select(c(dim_1, geodsg, valor)) %>%
  rename(wages = valor) %>%
  distinct()

ageing_ratio <- ageing_ratio %>%
  select(c(dim_1, geodsg, valor)) %>%
  rename(ageing_ratio = valor) %>%
  distinct()

crime_rate <- crime_rate %>%
  filter(dim_3_t == "Total") %>%
  select(c(dim_1, geodsg, valor)) %>%
  rename(crime_rate = valor) %>%
  distinct()

CPI <- CPI %>%
  filter(dim_3_t == "Total") %>%
  filter(geodsg == "Área Metropolitana de Lisboa") %>%
  select(c(dim_1, valor)) %>%
  rename(CPI = valor) %>%
  distinct()

nights_a <- nights_a %>%
  filter(dim_3_t == "Total") %>%
  select(c(dim_1, geodsg, valor)) %>%
  rename(nights = valor) %>%
  distinct()

georeferencia <- georeferencia %>%
  rename(municipio = NAME_2)

# Unimos bbdd por tipos de datos referidos a la fecha, primero mensuales, luego
# anuales
data1 <- medianprices_m %>%
  left_join(sales_m, by = c("dim_1", "geodsg")) %>%
  distinct(dim_1, geodsg, .keep_all = TRUE) %>%
  left_join(loans_m, by = c("dim_1", "geodsg")) %>%
  distinct(dim_1, geodsg, .keep_all = TRUE) %>%
  left_join(ir, by = "dim_1") %>%
  distinct(dim_1, geodsg, .keep_all = TRUE) %>%
  left_join(cost, by = "dim_1") %>%
  distinct(dim_1, geodsg, .keep_all = TRUE) %>%
  left_join(CPI, by = "dim_1") %>%
  distinct(dim_1, geodsg, .keep_all = TRUE)

# Datos anuales que se proceden a transformar a mensuales
data2 <- wages %>%
  left_join(pop_density, by = c("dim_1", "geodsg")) %>%
  left_join(pop_increase, by = c("dim_1", "geodsg")) %>%
  left_join(pop, by = c("dim_1", "geodsg")) %>%
  left_join(housestock, by = c("dim_1", "geodsg")) %>%
  left_join(ageing_ratio, by = c("dim_1", "geodsg")) %>%
  left_join(crime_rate, by = c("dim_1", "geodsg")) %>%
  left_join(licensed_dwellings_new_construction_a, by = c("dim_1", "geodsg")) %>%
  distinct(dim_1, geodsg, .keep_all = TRUE) 

# Series anuales
data3 <- medianprices_a %>%
  left_join(licensed_dwellings_new_construction_a, by = c("dim_1", "geodsg")) %>%
  distinct(dim_1, geodsg, .keep_all = TRUE) %>%
  left_join(loans_a, by = c("dim_1", "geodsg")) %>%
  distinct(dim_1, geodsg, .keep_all = TRUE) %>%
  left_join(nights_a, by = c("dim_1", "geodsg")) %>%
  distinct(dim_1, geodsg, .keep_all = TRUE) %>%
  left_join(sales_a, by = c("dim_1", "geodsg")) %>%
  distinct(dim_1, geodsg, .keep_all = TRUE) %>%
  left_join(data2, by = c("dim_1", "geodsg")) %>%
  distinct(dim_1, geodsg, .keep_all = TRUE) 

# Cambiamos los nombres en relacion a fecha y lugar
data1 <- data1 %>%
  rename(fecha = dim_1, municipio = geodsg)

data2 <- data2 %>%
  rename(fecha = dim_1, municipio = geodsg)

data3 <- data3 %>%
  rename(fecha = dim_1, municipio = geodsg)

# Aplicamos el formato fecha a los dataframes
meses_pt <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", 
              "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")

mes_a_numero <- function(mes) {
  which(tolower(meses_pt) == tolower(mes))
}

data1$fecha <- as.Date(sapply(data1$fecha, function(x) {
  partes <- strsplit(x, " de ")[[1]]
  mes <- mes_a_numero(partes[1])
  ano <- as.numeric(partes[2])
  paste(ano, sprintf("%02d", mes), "01", sep = "-")
}), format = "%Y-%m-%d")

data2 <- data2 %>%
  mutate(fecha = as.Date(paste(fecha, "01", "01", sep = "-"))) %>%
  uncount(12, .id = "mes") %>%
  mutate(fecha= fecha %m+% months(mes - 1)) %>%
  arrange(fecha) %>%
  select(-mes)

# Unificamos
df_mensual <- data1 %>%
  left_join(data2, by = c("fecha", "municipio"))

#Nos aseguramos que los datos sean de tipo numerico
df_mensual <- df_mensual %>%
  mutate(
    across(!c(fecha, municipio),
      ~ as.numeric(as.character(.))))

str(df_mensual)

data3 <- data3 %>%
  mutate(across(!c(municipio), ~ as.numeric(as.character(.))))

str(data3)

#Creamos las correspondencias de municipios con sus respectivas comunidades
correspondencia <- data.frame(
  
  pais = c(
    rep("Portugal", 308)
  ),
  
  distrito = c(
    rep("Norte", 86),
    rep("Centro", 100),
    rep("Área Metropolitana de Lisboa", 18),
    rep("Alentejo", 58),
    rep("Algarve", 16),
    rep("Região Autónoma dos Açores", 19),
    rep("Região Autónoma da Madeira", 11)
  ),
  
  comunidad = c(
    rep("Alto Minho", 10),
    rep("Cávado", 6),
    rep("Ave", 8),
    rep("Área Metropolitana do Porto", 17),
    rep("Alto Tâmega", 6),
    rep("Tâmega e Sousa", 11),
    rep("Douro", 19),
    rep("Terras de Trás-os-Montes", 9),
    rep("Oeste", 12),
    rep("Região de Aveiro", 11),
    rep("Região de Coimbra", 19),
    rep("Região de Leiria", 10),
    rep("Viseu Dão Lafões", 14),
    rep("Beira Baixa", 6),
    rep("Médio Tejo", 13),
    rep("Beiras e Serra da Estrela", 15),
    rep("Área Metropolitana de Lisboa", 18),
    rep("Alentejo Litoral", 5),
    rep("Baixo Alentejo", 13),
    rep("Lezíria do Tejo", 11),
    rep("Alto Alentejo", 15),
    rep("Alentejo Central", 14),
    rep("Algarve", 16),
    rep("Região Autónoma dos Açores", 19),
    rep("Região Autónoma da Madeira", 11)
  ),
  
  municipio = c(
    #Alto Minho
    'Arcos de Valdevez', 'Caminha', 'Melgaço', 'Monção', 'Paredes de Coura',
    'Ponte da Barca', 'Ponte de Lima', 'Valença', 'Viana do Castelo', 
    'Vila Nova de Cerveira',
    
    #Cávado
    'Amares', 'Barcelos', 'Braga', 'Esposende', 'Terras de Bouro', 'Vila Verde',
    
    #Ave
    'Cabeceiras de Basto', 'Fafe', 'Guimarães', 'Mondim de Basto', 
    'Póvoa de Lanhoso','Vieira do Minho', 'Vila Nova de Famalicão', 'Vizela',
    
    #Área Metropolitana do Porto
    'Arouca', 'Espinho', 'Gondomar', 'Maia', 'Matosinhos', 'Oliveira de Azeméis',
    'Paredes', 'Porto', 'Póvoa de Varzim', 'Santa Maria da Feira', 'Santo Tirso',
    'São João da Madeira', 'Trofa', 'Vale de Cambra', 'Valongo', 'Vila do Conde',
    'Vila Nova de Gaia',
    
    #Alto Tâmega
    'Boticas', 'Chaves', 'Montalegre', 'Ribeira de Pena', 'Valpaços', 
    'Vila Pouca de Aguiar',
    
    #Tâmega e Sousa
    'Amarante', 'Baião', 'Castelo de Paiva', 'Celorico de Basto', 'Cinfães',
    'Felgueiras', 'Lousada', 'Marco de Canaveses', 'Paços de Ferreira', 
    'Penafiel','Resende',
    
    #Douro
    'Alijó', 'Armamar', 'Carrazeda de Ansiães', 'Freixo de Espada à Cinta', 
    'Lamego','Mesão Frio', 'Moimenta da Beira', 'Murça', 'Penedono', 
    'Peso da Régua', 'Sabrosa','Santa Marta de Penaguião', 'São João da Pesqueira', 
    'Sernancelhe', 'Tabuaço','Tarouca', 'Torre de Moncorvo', 'Vila Nova de Foz Côa', 
    'Vila Real',
    
    #Terras de Trás-os-Montes
    'Alfândega da Fé', 'Bragança', 'Macedo de Cavaleiros', 'Miranda do Douro',
    'Mirandela', 'Mogadouro', 'Vila Flor', 'Vimioso', 'Vinhais',
    
    #Oeste
    'Alcobaça', 'Alenquer', 'Arruda dos Vinhos', 'Bombarral', 'Cadaval',
    'Caldas da Rainha', 'Lourinhã', 'Nazaré', 'Óbidos', 'Peniche',
    'Sobral de Monte Agraço', 'Torres Vedras',
    
    #Região de Aveiro
    'Águeda', 'Albergaria-a-Velha', 'Anadia', 'Aveiro', 'Estarreja', 'Ílhavo',
    'Murtosa', 'Oliveira do Bairro', 'Ovar', 'Sever do Vouga', 'Vagos',
    
    #Região de Coimbra
    'Arganil', 'Cantanhede', 'Coimbra', 'Condeixa-a-Nova', 'Figueira da Foz', 
    'Góis','Lousã', 'Mealhada', 'Mira', 'Miranda do Corvo', 'Montemor-o-Velho', 
    'Mortágua','Oliveira do Hospital', 'Pampilhosa da Serra', 'Penacova', 'Penela', 
    'Soure','Tábua', 'Vila Nova de Poiares',
    
    #Região de Leiria
    'Alvaiázere', 'Ansião', 'Batalha', 'Castanheira de Pêra', 'Figueiró dos Vinhos',
    'Leiria', 'Marinha Grande', 'Pedrógão Grande', 'Pombal', 'Porto de Mós',
    
    #Viseu Dão Lafões
    'Aguiar da Beira', 'Carregal do Sal', 'Castro Daire', 'Mangualde', 'Nelas',
    'Oliveira de Frades', 'Penalva do Castelo', 'Santa Comba Dão', 'São Pedro do Sul',
    'Sátão', 'Tondela', 'Vila Nova de Paiva', 'Viseu', 'Vouzela',
    
    #Beira Baixa
    'Castelo Branco', 'Idanha-a-Nova', 'Oleiros', 'Penamacor', 'Proença-a-Nova',
    'Vila Velha de Ródão',
    
    #Médio Tejo
    'Abrantes', 'Alcanena', 'Constância', 'Entroncamento', 'Ferreira do Zêzere',
    'Mação', 'Ourém', 'Sardoal', 'Sertã', 'Tomar', 'Torres Novas', 'Vila de Rei',
    'Vila Nova da Barquinha',
    
    #Beiras e Serra da Estrela
    'Almeida', 'Belmonte', 'Celorico da Beira', 'Covilhã', 'Figueira de Castelo Rodrigo',
    'Fornos de Algodres', 'Fundão', 'Gouveia', 'Guarda', 'Manteigas', 'Mêda', 
    'Pinhel','Sabugal', 'Seia', 'Trancoso',
    
    #Área Metropolitana de Lisboa
    'Alcochete', 'Almada', 'Amadora', 'Barreiro', 'Cascais', 'Lisboa', 'Loures', 
    'Mafra','Moita', 'Montijo', 'Odivelas', 'Oeiras', 'Palmela', 'Seixal', 
    'Sesimbra', 'Setúbal','Sintra', 'Vila Franca de Xira',
    
    #Alentejo Litoral
    'Alcácer do Sal', 'Grândola', 'Odemira', 'Santiago do Cacém', 'Sines',
    
    #Baixo Alentejo
    'Aljustrel', 'Almodôvar', 'Alvito', 'Barrancos', 'Beja', 'Castro Verde', 
    'Cuba','Ferreira do Alentejo', 'Mértola', 'Moura', 'Ourique', 'Serpa', 
    'Vidigueira',
    
    #Lezíria do Tejo
    'Almeirim', 'Alpiarça', 'Azambuja', 'Benavente', 'Cartaxo', 'Chamusca', 
    'Coruche','Golegã', 'Rio Maior', 'Salvaterra de Magos', 'Santarém',
    
    #Alto Alentejo
    'Alter do Chão', 'Arronches', 'Avis', 'Campo Maior', 'Castelo de Vide', 
    'Crato','Elvas', 'Fronteira', 'Gavião', 'Marvão', 'Monforte', 'Nisa', 
    'Ponte de Sor','Portalegre', 'Sousel',
    
    #Alentejo Central
    'Alandroal', 'Arraiolos', 'Borba', 'Estremoz', 'Évora', 'Montemor-o-Novo', 
    'Mora','Mourão', 'Portel', 'Redondo', 'Reguengos de Monsaraz', 'Vendas Novas',
    'Viana do Alentejo', 'Vila Viçosa',
    
    #Algarve
    'Albufeira', 'Alcoutim', 'Aljezur', 'Castro Marim', 'Faro', 'Lagoa', 'Lagos', 
    'Loulé','Monchique', 'Olhão', 'Portimão', 'São Brás de Alportel', 'Silves', 
    'Tavira','Vila do Bispo', 'Vila Real de Santo António',
    
    #Região Autónoma dos Açores
    'Angra do Heroísmo', 'Calheta', 'Corvo', 'Horta', 'Lagoa', 'Lajes das Flores',
    'Lajes do Pico', 'Madalena', 'Nordeste', 'Ponta Delgada', 'Povoação', 
    'Ribeira Grande','Santa Cruz da Graciosa', 'Santa Cruz das Flores', 
    'São Roque do Pico', 'Velas','Vila da Praia da Vitória', 'Vila do Porto', 
    'Vila Franca do Campo',
    
    #Região Autónoma da Madeira
    'Calheta', 'Câmara de Lobos', 'Funchal', 'Machico', 'Ponta do Sol', 
    'Porto Moniz','Porto Santo', 'Ribeira Brava', 'Santa Cruz', 'Santana', 
    'São Vicente'
  ),
  stringsAsFactors = TRUE
)

table(correspondencia$municipio)
# Problema: se repite Lagoa y Calheta 
# Lagoa en Algarve y dos Acores
# Calheta en Acores y Madeira
# No tendra relevancia en nuestro análisis

df_mensual1 <- df_mensual %>%
  left_join(correspondencia, by = "municipio")

saveRDS(df_mensual1, "1. Data/df_finaltodo.rds")

georeferencia <- sf::st_as_sf(georeferencia)

geounificacion <- correspondencia %>%
  left_join(georeferencia, by = "municipio")

geounificacion <- sf::st_as_sf(geounificacion)

# Vinculamos la correspondencia
df_mensual <- df_mensual %>%
  left_join(correspondencia, by = "municipio") %>%
  filter(municipio == "Cascais" |
         municipio == "Oeiras" |
         municipio == "Amadora" |
         municipio == "Lisboa" |
         municipio == "Sintra" |
         municipio == "Odivelas" |
         municipio == "Loures" |
         municipio == "Mafra" |
         municipio == "Vila Franca de Xira") %>%
  distinct(fecha, municipio, .keep_all = TRUE)

df_mensualgeo <- df_mensual %>%
  left_join(geounificacion, by = "municipio") %>%
  distinct(fecha, municipio, .keep_all = TRUE)

df_mensualgeo <- sf::st_as_sf(df_mensualgeo)


data3 <- data3 %>%
  left_join(correspondencia, by = "municipio") %>%
  distinct(fecha, municipio, .keep_all =  TRUE)

saveRDS(df_mensualgeo, "1. Data/df_final.rds")
saveRDS(data3, "1. Data/df_finalanual.rds")
rm(list = ls())
