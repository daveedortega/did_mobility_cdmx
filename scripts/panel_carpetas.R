####################################################.
## Generador de BdD en formato Panel para Carpetas de Investigación con Colonias del IECM: 
## DAOA - 19/10/2023
####################################################.

# Preparar Espacio --------------------------------------------------------

pacman::p_load(tidyverse,sf,janitor,scales)
rm(list=setdiff(ls(), "inviales_panel"))

# Cargar Bases ------------------------------------------------------------
carpetas_18 <- read_csv("input/carpetas_fgj/carpetas_2016-2018.csv")
carpetas_21 <- read_csv("input/carpetas_fgj/carpetas_2019-2021.csv")
carpetas_22 <- read_csv("input/carpetas_fgj/carpetas_2022_2023.csv") 
carpetas_22 <- carpetas_22 %>% filter(ao_inicio == 2022)# marzo 2023, fix for 2022 and paste all 2023 next
carpetas_23 <- read_csv("input/carpetas_fgj/carpetas_2023.csv") # julio 2023

# Name changes in last DB, fix

carpetas_23 <- carpetas_23 %>% rename(ao_hechos = anio_hecho, mes_hechos = mes_hecho, fecha_hechos = fecha_hecho, 
                                      hora_hechos = hora_hecho, ao_inicio = anio_inicio, alcaldia_hechos = alcaldia_hecho, 
                                      municipio_hechos = municipio_hecho, colonia_datos = colonia_hecho, 
                                      fgj_colonia_registro = colonia_catalogo, categoria_delito = categoria)
# Single db
carpetas_completa <-rbind(carpetas_18,carpetas_21,carpetas_22,carpetas_23)
# Clean space
rm(carpetas_18,carpetas_21, carpetas_22, carpetas_23)

# Mapa CDMX

colonias_cdmx <- read_sf("input/mapas/colonias_iecm/mgpc_2019.shp") %>% st_make_valid()

# Cleaning Panel ----------------------------------------------------------

# puntos_carpetas %>% count(delito) %>% View()
puntos_carpetas <- carpetas_completa %>% select(fecha_inicio, categoria_delito,delito, longitud,latitud) %>% 
  na.omit() %>% # No acepta registros sin valores, los quitamos
  st_as_sf(coords = c("longitud", "latitud"),crs = 4326)

colonias_cdmx <- st_transform(colonias_cdmx,st_crs(puntos_carpetas))

# Selección de Delitos ----------------------------------------------------

# Filter non-crimes
carpetas_completa %>%  filter(categoria_delito != "HECHO NO DELICTIVO") %>% count(delito) %>% filter(!delito %in% todos) %>% View()
# 322 Categories

# Robberies clasification
robos <- c("ROBO DE OBJETOS", 
           "ROBO A TRANSEUNTE EN VIA PUBLICA CON VIOLENCIA", 
           "ROBO A NEGOCIO SIN VIOLENCIA", 
           "ROBO DE ACCESORIOS DE AUTO", 
           "ROBO DE OBJETOS DEL INTERIOR DE UN VEHICULO", 
           "ROBO DE VEHICULO DE SERVICIO PARTICULAR SIN VIOLENCIA", 
           "ROBO A CASA HABITACION SIN VIOLENCIA", 
           "ROBO A NEGOCIO CON VIOLENCIA", 
           "ROBO A PASAJERO / CONDUCTOR DE VEHICULO CON VIOLENCIA", 
           "ROBO DE VEHICULO DE SERVICIO PARTICULAR CON VIOLENCIA", 
           "ROBO A NEGOCIO SIN VIOLENCIA POR FARDEROS (TIENDAS DE AUTOSERVICIO)", 
           "ROBO A TRANSEUNTE DE CELULAR CON VIOLENCIA", 
           "ROBO A TRANSEUNTE DE CELULAR SIN VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE METRO SIN VIOLENCIA", 
           "ROBO A TRANSEUNTE EN VIA PUBLICA SIN VIOLENCIA", 
           "ROBO A REPARTIDOR CON VIOLENCIA", 
           "ROBO DE VEHICULO DE PEDALES", 
           "ROBO DE DINERO", 
           "ROBO DE MOTOCICLETA SIN VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE TRANSPORTE PÚBLICO CON VIOLENCIA",
           "ROBO DE DOCUMENTOS", 
           "ROBO DE PLACA DE AUTOMOVIL", 
           "ROBO DE MOTOCICLETA CON VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE PESERO COLECTIVO CON VIOLENCIA", 
           "ROBO DE VEHICULO DE SERVICIO PÚBLICO CON VIOLENCIA", 
           "ROBO DE VEHICULO DE SERVICIO PÚBLICO SIN VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE METROBUS SIN VIOLENCIA", 
           "ROBO A TRANSEUNTE EN PARQUES Y MERCADOS CON VIOLENCIA", 
           "ROBO A CASA HABITACION CON VIOLENCIA", 
           "ROBO A REPARTIDOR SIN VIOLENCIA", 
           "ROBO A TRANSEUNTE EN NEGOCIO CON VIOLENCIA", 
           "ROBO S/V DENTRO DE NEGOCIOS, AUTOSERVICIOS, CONVENIENCIA", 
           "ROBO DE OBJETOS A ESCUELA", 
           "ROBO A PASAJERO A BORDO DE TRANSPORTE PÚBLICO SIN VIOLENCIA",
           "ROBO A NEGOCIO SIN VIOLENCIA POR FARDEROS", 
           "ROBO A TRANSEUNTE SALIENDO DEL BANCO CON VIOLENCIA", 
           "TENTATIVA DE ROBO", 
           "ROBO A NEGOCIO SIN VIOLENCIA POR FARDEROS (TIENDAS DE CONVENIENCIA)", 
           "ROBO A NEGOCIO CON VIOLENCIA POR FARDEROS (TIENDAS DE CONVENIENCIA)", 
           "ROBO A TRANSEUNTE CONDUCTOR DE TAXI PUBLICO Y PRIVADO CON VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE METRO CON VIOLENCIA", 
           "ROBO A TRANSEUNTE A BORDO DE TAXI PUBLICO Y PRIVADO SIN VIOLENCIA", 
           "ROBO A TRANSEUNTE A BORDO DE TAXI PÚBLICO Y PRIVADO CON VIOLENCIA", 
           "ROBO A OFICINA PÚBLICA SIN VIOLENCIA", 
           "POSESION DE VEHICULO ROBADO", 
           "ROBO A PASAJERO A BORDO DE PESERO COLECTIVO SIN VIOLENCIA", 
           "ROBO DE ANIMALES", 
           "ROBO A REPARTIDOR Y VEHICULO CON VIOLENCIA", 
           "ROBO DE ARMA",
           "ROBO A PASAJERO / CONDUCTOR DE TAXI CON VIOLENCIA", 
           "ROBO EN EVENTOS MASIVOS (DEPORTIVOS, CULTURALES, RELIGIOSOS Y ARTISTICOS) S/V", 
           "ROBO A TRANSPORTISTA Y VEHICULO PESADO CON VIOLENCIA", 
           "ROBO A TRANSEUNTE EN RESTAURANT CON VIOLENCIA", 
           "ROBO A PASAJERO EN TROLEBUS SIN VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE TAXI CON VIOLENCIA", 
           "ROBO A TRANSEUNTE SALIENDO DEL CAJERO CON VIOLENCIA", 
           "ROBO DE FLUIDOS", 
           "TENTATIVA DE ROBO DE VEHICULO", 
           "ROBO DE ALHAJAS", 
           "ROBO A CASA HABITACION Y VEHICULO SIN VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE TAXI SIN VIOLENCIA", 
           "ROBO DE VEHICULO DE SERVICIO DE TRANSPORTE SIN VIOLENCIA", 
           "ROBO A CASA HABITACION Y VEHICULO CON VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE METROBUS CON VIOLENCIA",
           "ROBO A PASAJERO EN TREN LIGERO SIN VIOLENCIA",
           "ROBO A LOCALES SEMIFIJOS (PUESTOS DE ALIMENTOS,BEBIDAS, ENSERES, PERIODICOS,LOTERIA, OTROS)", 
           "ROBO A PASAJERO EN RTP CON VIOLENCIA", 
           "ROBO DE VEHICULO DE SERVICIO OFICIAL SIN VIOLENCIA", 
           "ROBO A NEGOCIO CON VIOLENCIA POR FARDEROS (TIENDAS DE AUTOSERVICIO)", 
           "ROBO A NEGOCIO Y VEHICULO CON VIOLENCIA", 
           "DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A VIAS DE COMUNICACION", 
           "ROBO DE VEHICULO DE SERVICIO DE TRANSPORTE CON VIOLENCIA", 
           "ROBO A NEGOCIO Y VEHICULO SIN VIOLENCIA", 
           "ROBO A REPARTIDOR Y VEHICULO SIN VIOLENCIA", 
           "ROBO DE MAQUINARIA CON VIOLENCIA", 
           "ROBO A PASAJERO EN TROLEBUS CON VIOLENCIA",
           "ROBO A PASAJERO EN RTP SIN VIOLENCIA", 
           "ROBO A TRANSPORTISTA Y VEHICULO PESADO SIN VIOLENCIA", 
           "ROBO A PASAJERO EN AUTOBÚS FORÁNEO CON VIOLENCIA", 
           "ROBO A TRANSEUNTE EN HOTEL CON VIOLENCIA", 
           "ROBO A PASAJERO EN TREN SUBURBANO SIN VIOLENCIA", 
           "ROBO DE CONTENEDORES DE TRAILERS S/V", 
           "ROBO A OFICINA PÚBLICA CON VIOLENCIA", 
           "ROBO A SUCURSAL BANCARIA CON VIOLENCIA", 
           "ROBO DE MERCANCIA EN CONTENEDEROS EN ÁREAS FEDERALES", 
           "ROBO DE VEHICULO EN PENSION, TALLER Y AGENCIAS S/V", 
           "ROBO EN INTERIOR DE EMPRESA (NOMINA) SIN VIOLENCIA", 
           "ROBO DE VEHICULO DE SERVICIO OFICIAL CON VIOLENCIA", 
           "ROBO DE MAQUINARIA SIN VIOLENCIA", 
           "ROBO A SUCURSAL BANCARIA (SUPERMERCADO) SIN VIOLENCIA", 
           "ROBO A PASAJERO EN ECOBUS CON VIOLENCIA", 
           "ROBO A SUCURSAL BANCARIA (ASALTO BANCARIO) CON VIOLENCIA", 
           "ROBO A TRANSEUNTE EN VIA PUBLICA (NOMINA) CON VIOLENCIA", 
           "ROBO DE VEHICULO EN PENSION, TALLER Y AGENCIAS C/V", 
           "ROBO DE MERCANCIA A TRANSPORTISTA C/V", 
           "ROBO A PASAJERO EN AUTOBUS FORANEO SIN VIOLENCIA", 
           "ROBO A PASAJERO EN ECOBUS SIN VIOLENCIA", 
           "ROBO A SUCURSAL BANCARIA SIN VIOLENCIA", 
           "ROBO A TRANSEUNTE EN TERMINAL DE PASAJEROS CON VIOLENCIA", 
           "ROBO A TRANSEUNTE Y VEHICULO CON VIOLENCIA", 
           "ROBO DE VEHICULO ELECTRICO MOTOPATIN", 
           "ROBO EN INTERIOR DE EMPRESA (NOMINA) CON VIOLENCIA", 
           "ROBO A PASAJERO EN TREN LIGERO CON VIOLENCIA", 
           "ROBO A TRANSEUNTE EN CINE CON VIOLENCIA", 
           "ROBO DURANTE TRASLADO DE VALORES (NOMINA) CON VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE PESERO Y VEHICULO CON VIOLENCIA", 
           "ROBO A SUCURSAL BANCARIA DENTRO DE  TIENDAS DE AUTOSERVICIO CON VIOLENCIA", 
           "ROBO A SUCURSAL BANCARIA (SUPERMERCADO) CON VIOLENCIA", 
           "ROBO DURANTE TRASLADO DE VALORES (NOMINA) SIN VIOLENCIA", 
           "ROBO A PASAJERO EN TREN SUBURBANO CON VIOLENCIA", 
           "ROBO A SUCURSAL BANCARIA DENTRO DE  TIENDAS DE AUTOSERVICIO S/V", 
           "ROBO A PASAJERO A BORDO DE CABLEBUS CON VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE CABLEBUS SIN VIOLENCIA", 
           "ROBO A TRANSEUNTE EN VIA PUBLICA (NOMINA) SIN VIOLENCIA",
           "ROBO DE VEHICULO Y NOMINA CON VIOLENCIA")

delitos_sexuales <- c("VIOLACION EQUIPARADA",
                      "ABUSO SEXUAL", 
                      "VIOLACION",
                      "ACOSO SEXUAL", 
                      "VIOLACION DE CORRESPONDENCIA", 
                      "VIOLACION TUMULTUARIA", 
                      "ESTUPRO", 
                      "VIOLACION EQUIPARADA POR CONOCIDO", 
                      "VIOLACION TUMULTUARIA EQUIPARADA", 
                      "TENTATIVA DE VIOLACION", 
                      "VIOLACION TUMULTUARIA EQUIPARADA POR CONOCIDO", 
                      "ACOSO SEXUAL AGRAVADO EN CONTRA DE MENORES",
                      "VIOLACION EQUIPARADA Y ROBO DE VEHICULO", 
                      "VIOLACION Y ROBO DE VEHICULO",
                      "CONTRA LA INTIMIDAD SEXUAL",
                      "CORRUPCION DE MENORES E INCAPACES",
                      "LENOCINIO")

lesiones_danio <- c("LESIONES INTENCIONALES POR GOLPES", 
              "LESIONES CULPOSAS POR TRANSITO VEHICULAR EN COLISION", 
              "LESIONES INTENCIONALES POR ARMA DE FUEGO", 
              "LESIONES INTENCIONALES POR ARMA BLANCA",
              "LESIONES CULPOSAS POR TRANSITO VEHICULAR", 
              "LESIONES CULPOSAS", 
              "LESIONES INTENCIONALES",
              "LESIONES CULPOSAS POR CAIDA", 
              "LESIONES CULPOSAS POR CAIDA DE VEHÍCULO EN MOVIMIENTO", 
              "LESIONES CULPOSAS POR QUEMADURAS",
              "LESIONES DOLOSAS POR QUEMADURAS", 
              "LESIONES CULPOSAS ACCIDENTE LABORAL", 
              "LESIONES CULPOSAS CON EXCLUYENTES DE RESPONSABILIDAD", 
              "LESIONES INTENCIONALES Y ROBO DE VEHICULO", 
              "DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A AUTOMOVIL", 
              "DAÑO EN PROPIEDAD AJENA INTENCIONAL", 
              "DAÑO EN PROPIEDAD AJENA CULPOSA", 
              "DAÑO EN PROPIEDAD AJENA INTENCIONAL A AUTOMOVIL",
              "DAÑO EN PROPIEDAD AJENA INTENCIONAL A BIENES INMUEBLES", 
              "DAÑO EN PROPIEDAD AJENA INTENCIONAL A CASA HABITACION", 
              "DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A BIENES INMUEBLES",
              "DAÑO EN PROPIEDAD AJENA INTENCIONAL A NEGOCIO", 
              "DAÑO EN PROPIEDAD AJENA INTENCIONAL A VIAS DE COMUNICACION",
              "DAÑO SUELO (ACTIVIDAD, INVASIÓN O EXTRACCIÓN)")

asesinatos <- c("TENTATIVA DE HOMICIDIO", 
                "HOMICIDIO POR ARMA DE FUEGO", 
                "HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (ATROPELLADO)", 
                "HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (COLISION)", 
                "HOMICIDIO POR ARMA BLANCA", 
                "HOMICIDIO POR GOLPES", 
                "HOMICIDIOS INTENCIONALES (OTROS)", 
                "HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR", 
                "HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (CAIDA)", 
                "HOMICIDIO CULPOSO",
                "HOMICIDIO CULPOSO POR ARMA DE FUEGO",
                "HOMICIDIO CULPOSO CON EXCLUYENTES DE RESPONSABILIDAD", 
                "HOMICIDIO POR AHORCAMIENTO", 
                "HOMICIDIO CULPOSO POR INSTRUMENTO PUNZO CORTANTE", 
                "HOMICIDIO DOLOSO", 
                "HOMICIDIO INTENCIONAL Y ROBO DE VEHICULO", 
                "HOMICIDIO POR INMERSION",
                "TENTATIVA DE FEMINICIDIO",
                "FEMINICIDIO", 
                "FEMINICIDIO POR ARMA BLANCA", 
                "FEMINICIDIO POR DISPARO DE ARMA DE FUEGO", 
                "FEMINICIDIO POR GOLPES",
                "HOMICIDIO CULPOSO FUERA DEL D.F (ATROPELLADO)",
                "HOMICIDIO CULPOSO FUERA DEL D.F (COLISION)")



# Single most reported crime
violfam <- "VIOLENCIA FAMILIAR"

todos <- c(asesinatos,delitos_sexuales,lesiones_danio,robos,violfam)


# Filter DB ---------------------------------------------------------------

# Robberies

fecha_robos <- puntos_carpetas %>% filter(delito %in% robos) %>%  mutate(c_delito = "ROBOS") %>% 
  mutate(ano_mes = format(fecha_inicio,format = "%Y-%m")) %>% select(ano_mes,c_delito,geometry) %>% 
  group_split(ano_mes, c_delito)

# Homicides

fecha_asesinatos <- puntos_carpetas %>% filter(delito %in% asesinatos) %>%  mutate(c_delito = "ASESINATOS") %>% 
  mutate(ano_mes = format(fecha_inicio,format = "%Y-%m"))%>% select(ano_mes,c_delito,geometry) %>% 
  group_split(ano_mes, c_delito)

# Sex Crimes

fecha_sexuales <- puntos_carpetas %>% filter(delito %in% delitos_sexuales) %>%  mutate(c_delito = "SEXUALES") %>% 
  mutate(ano_mes = format(fecha_inicio,format = "%Y-%m"))%>% select(ano_mes,c_delito,geometry) %>% 
  group_split(ano_mes, c_delito)

# Violencia familiar

fecha_vf <- puntos_carpetas %>% filter(delito == violfam) %>%  mutate(c_delito = "VIOLENCIA FAMILIAR") %>% 
  mutate(ano_mes = format(fecha_inicio,format = "%Y-%m")) %>% select(ano_mes,c_delito,geometry) %>% 
  group_split(ano_mes, c_delito)

# Lesiones & Daños

fecha_lesiones <- puntos_carpetas %>% filter(delito %in% lesiones_danio) %>%  mutate(c_delito = "LESIONES") %>% 
  mutate(ano_mes = format(fecha_inicio,format = "%Y-%m")) %>% select(ano_mes,c_delito,geometry) %>% 
  group_split(ano_mes, c_delito)

# All Crimes

fechas_delitos <- puntos_carpetas %>% mutate(c_delito = "TODOS") %>%
  mutate(ano_mes = format(fecha_inicio,format = "%Y-%m")) %>% select(ano_mes,c_delito,geometry) %>% 
  group_split(ano_mes, c_delito)


# Intersect with Map ------------------------------------------------------

source("scripts/iterated_intersection.R")

test <- data.frame(id = 1:length(colonias_cdmx$geometry))# La función necesita saber cuántas tiene
colonias_cdmx <- st_transform(colonias_cdmx,st_crs(fecha_robos[[1]]$geometry))
colonias_cdmx <- colonias_cdmx %>% st_make_valid()

tok <- Sys.time()
# Core Crimes
robos_fechas_intersecciones<- iterated_intersection(fecha_robos,test,colonias_cdmx) 
asesinatos_fechas_intersecciones<- iterated_intersection(fecha_asesinatos,test,colonias_cdmx) 
sexuales_fechas_intersecciones <- iterated_intersection(fecha_sexuales,test,colonias_cdmx)
delitos_general <- iterated_intersection(fechas_delitos,test,colonias_cdmx)
# Complementary
vf_fechas_intersecciones <- iterated_intersection(fecha_vf,test,colonias_cdmx)
fecha_lesiones_intersecciones <- iterated_intersection(fecha_lesiones,test,colonias_cdmx)
tik <- Sys.time()
print(tik-tok)


### Dummy Panel 

dates_complete <- seq(as.Date("2016-01-01"),as.Date("2023-07-01"),by = "month") %>% rep(1815) # 1,815 neighborhoods

colonias_key <- colonias_cdmx$CVEUT %>% 
  rep(length(seq(as.Date("2016-01-01"),as.Date("2023-07-01"),by = "month"))) %>% sort() # Must be same length as the sequence of dates

dummy_df <- tibble(dates_complete,CVEUT = colonias_key)
complete_df <- dummy_df %>% left_join(colonias_cdmx,by = "CVEUT")

# Last DBs

# Delitos Totales
delitos_totales_final <- delitos_general %>% cbind(CVEUT = colonias_cdmx$CVEUT) %>%  select(!id) %>% 
  pivot_longer(!CVEUT,names_to = "delito_fecha",values_to = "total_delitos") %>% 
  mutate(delito_fecha = str_remove(delito_fecha, "list\\(c\\(.*\\s")) %>% 
  separate(delito_fecha, into = c("delito", "ano_mes"),sep = " ") %>% mutate(dates_complete = as.Date(paste0(ano_mes, "-01")))

#Robos
robos_final <- robos_fechas_intersecciones %>% cbind(CVEUT = colonias_cdmx$CVEUT) %>%  select(!id) %>% 
  pivot_longer(!CVEUT,names_to = "delito_fecha",values_to = "robos") %>% 
  mutate(delito_fecha = str_remove(delito_fecha, "list\\(c\\(.*\\s")) %>% 
  separate(delito_fecha, into = c("delito", "ano_mes"),sep = " ") %>% mutate(dates_complete = as.Date(paste0(ano_mes, "-01")))

#Asesinatos
asesinatos_final <- asesinatos_fechas_intersecciones %>% cbind(CVEUT = colonias_cdmx$CVEUT) %>%  select(!id) %>% 
  pivot_longer(!CVEUT,names_to = "delito_fecha",values_to = "asesinatos") %>% 
  mutate(delito_fecha = str_remove(delito_fecha, "list\\(c\\(.*\\s")) %>% 
  separate(delito_fecha, into = c("delito", "ano_mes"),sep = " ") %>% mutate(dates_complete = as.Date(paste0(ano_mes, "-01")))

# Sexuales
sexuales_final <-sexuales_fechas_intersecciones %>% cbind(CVEUT = colonias_cdmx$CVEUT) %>%  select(!id) %>% 
  pivot_longer(!CVEUT,names_to = "delito_fecha",values_to = "d_sexuales") %>% 
  mutate(delito_fecha = str_remove(delito_fecha, "list\\(c\\(.*\\s")) %>% 
  separate(delito_fecha, into = c("delito", "ano_mes"),sep = " ") %>% mutate(dates_complete = as.Date(paste0(ano_mes, "-01")))


# Violencia Familiar Totales
violfam_final <- vf_fechas_intersecciones %>% cbind(CVEUT = colonias_cdmx$CVEUT) %>%  select(!id) %>% 
  pivot_longer(!CVEUT,names_to = "delito_fecha",values_to = "viol_fam") %>% 
  mutate(delito_fecha = str_remove(delito_fecha, "list\\(c\\(.*\\s")) %>% 
  separate(delito_fecha, into = c("delito", "ano_mes"),sep = "R ") %>% mutate(dates_complete = as.Date(paste0(ano_mes, "-01")))

# Lesiones - Daños
lesiones_final <- fecha_lesiones_intersecciones %>% cbind(CVEUT = colonias_cdmx$CVEUT) %>%  select(!id) %>% 
  pivot_longer(!CVEUT,names_to = "delito_fecha",values_to = "lesiones_danios") %>% 
  mutate(delito_fecha = str_remove(delito_fecha, "list\\(c\\(.*\\s")) %>% 
  separate(delito_fecha, into = c("delito", "ano_mes"),sep = " ") %>% mutate(dates_complete = as.Date(paste0(ano_mes, "-01")))

# Final Panel 

complete_df <- complete_df %>% left_join(delitos_totales_final%>% select(-delito, -ano_mes), by = c("CVEUT","dates_complete")) %>% 
  left_join(robos_final %>% select(-delito, -ano_mes), by = c("CVEUT","dates_complete")) %>% 
  left_join(sexuales_final%>% select(-delito, -ano_mes), by = c("CVEUT","dates_complete")) %>% 
  left_join(asesinatos_final%>% select(-delito, -ano_mes), by = c("CVEUT","dates_complete")) %>% 
  left_join(violfam_final%>% select(-delito, -ano_mes), by = c("CVEUT","dates_complete")) %>% 
  left_join(lesiones_final%>% select(-delito, -ano_mes), by = c("CVEUT","dates_complete"))

# Unclutter space
rm(list = setdiff(ls(), c('inviales_panel', 'complete_df')))

# Intersections with lines ------------------------------------------------

# Colonias IECM
colonias_cdmx <- read_sf("input/mapas/colonias_iecm/mgpc_2019.shp") %>% st_make_valid()

# Mapas de líneas Y buffers:
#CB L1
cablebus_1b <- read_sf("input/mapas/cablebus_l1/cb_l1_b500.shp")
#CB L2
cablebus_2b <- read_sf("input/mapas/cablebus_l2/cb_l2_b500.shp")
#TR L9
trole_l9b <- read_sf("input/mapas/trolebus_l9/trolebus_l9_b500.shp")
#TR E
trole_elevadob <- read_sf("input/mapas/trolebus_elevado/tr_e_b500_v.shp")
# L12
l12b <- read_sf("input/mapas/stc_l12/stc_l1_l12_b500.shp") %>% filter(LINEA == 12) # Toda la línea
# L1
l1b <-  read_sf("input/mapas/stc_l1/stc_l1_ec500m.shp") # Estaciones Cerradas

colonias_cdmx <- st_transform(colonias_cdmx,st_crs(cablebus_1b)) # Cambiamos mapa a mismo CRS de otros, ahora todos tienen el mismo
colonias_cdmx <- colonias_cdmx %>% st_make_valid()

# Count Intersections
cb_1b <- lengths(st_intersects(colonias_cdmx,cablebus_1b))

cb_2b <- lengths(st_intersects(colonias_cdmx,cablebus_2b))

tr_eb <- lengths(st_intersects(colonias_cdmx,trole_elevadob))

tr_9b <- lengths(st_intersects(colonias_cdmx,trole_l9b))

l_1b <- lengths(st_intersects(colonias_cdmx,l1b))

l_12b <- lengths(st_intersects(colonias_cdmx,l12b))

# Make binary 0,1 Treatment

colonias_cdmx <- colonias_cdmx %>% cbind(cb_1b,cb_2b,tr_9b,tr_eb,l_1b,l_12b) %>% 
  mutate(cb_1b = ifelse(cb_1b>0,1,0), 
         cb_2b = ifelse(cb_2b>0,1,0), 
         tr_9b = ifelse(tr_9b>0,1,0), 
         tr_eb = ifelse(tr_eb>0,1,0),
         l_1b = ifelse(l_1b>0,1,0), 
         l_12b = ifelse(l_12b>0,1,0))

rm(cb_1b,cb_2b,tr_9b,tr_eb,l_1b,l1b,l_12b, l12b, cablebus_1b, cablebus_2b, trole_elevadob, trole_l9b) # Clear Space

# Paste into Panel
complete_df$geometry = NULL # Taxing process

# Complete Panel
complete_df <- complete_df %>%  left_join(colonias_cdmx %>% select(-geometry, -ENT, -CVEDT, -NOMDT, -DTTOLOC, -NOMUT, -POB2010, -ID))

# DiD Package Panel formating ---------------------------------------------

dummy_id <- tibble(CVEUT = unique(complete_df$CVEUT), id_col = 1:length(unique(complete_df$CVEUT)))
numeric_date <- tibble( dates_complete = unique(complete_df$dates_complete), numeric_date = 1:length(unique(complete_df$dates_complete)))

carpetas_panel <- complete_df %>% left_join(dummy_id) %>% left_join(numeric_date) %>% 
  select(id_col, numeric_date, CVEUT, dates_complete, 
         POB2010, total_delitos, robos, d_sexuales, asesinatos, viol_fam, lesiones_danios, 
         cb_1b, cb_2b, tr_9b, tr_eb, l_1b, l_12b)

# Sanity check

carpetas_panel %>% count(numeric_date) %>% count(n)
carpetas_panel %>% count(id_col) %>% count(n)


rm(list = setdiff(ls(), c('inviales_panel', 'carpetas_panel')))



