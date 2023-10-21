####################################################.
## Generador de BdD en formato Panel para Incidentes Viales con Colonias del IECM: 
## DAOA - 18/10/2023
####################################################.

# Preparar Espacio --------------------------------------------------------

pacman::p_load(tidyverse,sf,janitor,scales)
rm(list=ls())

# Cargar Bases ------------------------------------------------------------

# Incidentes Viales
inviales_15 <- read_csv("input/incidentes_viales/inViales_2014_2015.csv")
inviales_18 <- read_csv("input/incidentes_viales/inViales_2016_2018.csv")
inviales_21 <- read_csv("input/incidentes_viales/inViales_2019_2021.csv")
# Actualizado hasta julio 2023
inviales_22 <- read_csv("input/incidentes_viales/inViales_2022_2023_7.csv") # Además dice alcaldía no colonia
inviales_22 <- inviales_22 %>% rename(delegacion_inicio=alcaldia_inicio,delegacion_cierre=alcaldia_cierre) %>% 
  select(!c(colonia))
# Juntar Incidentes Viales en una sola BdD
inviales_18_22 <- rbind(inviales_15,inviales_18,inviales_21,inviales_22)
rm(inviales_15,inviales_18,inviales_21,inviales_22) # Eliminamos cosas que no necesitamos

# Mapa CDMX
colonias_cdmx <- read_sf("input/mapas/colonias_iecm/mgpc_2019.shp") %>% st_make_valid()

# Making Panel -------------------------------------------------------

# Monthly Grouping
inviales_18_22 <- inviales_18_22 %>% mutate(ano_mes = format(fecha_cierre,format = "%Y-%m")) %>% 
  select(ano_mes,tipo_incidente_c4,incidente_c4,latitud,longitud,tipo_entrada)

# Convert into st
inviales_18_22 <- inviales_18_22 %>% na.omit() %>% # No acepta cosas sin valores, los quitamos
  st_as_sf(coords = c("longitud", "latitud"),crs = 4326)

# Paste categories 
inviales_18_22 <- inviales_18_22 %>% mutate(incidente_tipificado = paste(incidente_c4,tipo_incidente_c4)) 

# Filter incidents to investigate

# Define categories
choque_sl_accidente <- inviales_18_22 %>% filter(incidente_tipificado == 'Choque sin lesionados Accidente')
choque_cl_accidente <- inviales_18_22 %>% filter(incidente_tipificado == 'Choque con lesionados Accidente')
atropellado_lesionado <- inviales_18_22 %>% filter(incidente_tipificado == 'Atropellado Lesionado')
moto_accidente <- inviales_18_22 %>% filter(incidente_tipificado == 'Motociclista Accidente')

## Crear Panel Vacío 

fechas_secuencia <- seq(as.Date("2013-12-01"),as.Date("2023-07-01"),by = "month")
dates_complete <- fechas_secuencia %>% rep(1815) # each county has full dates, 1,815 colonias IECM
colonias_key <- colonias_cdmx$CVEUT %>% rep(length(fechas_secuencia)) %>% sort() # Must be same length as the sequence of dates
dummy_df <- tibble(dates_complete,CVEUT = colonias_key)

# Panel Completo

complete_df <- dummy_df %>% left_join(colonias_cdmx,by = "CVEUT")

# Contar Incidentes por mes por polígono ----------------------------------

choque_sl_accidente <- choque_sl_accidente %>% group_split(ano_mes)
choque_cl_accidente <- choque_cl_accidente %>% group_split(ano_mes)
atropellado_lesionado <- atropellado_lesionado %>% group_split(ano_mes)
moto_accidente <- moto_accidente %>% group_split(ano_mes)
# Añadimos total
total_incidentes <- inviales_18_22 %>% group_by(ano_mes) %>% group_split()

# Helping Function: 
source("scripts/iterated_intersection.R")

test <- data.frame(id = 1:length(colonias_cdmx$geometry))
colonias_cdmx <- colonias_cdmx %>% st_transform(crs = st_crs(choque_sl_accidente[[1]]))
colonias_cdmx <- colonias_cdmx %>% st_make_valid()

# Intersectamos 

choque_sl_accidente_final<- iterated_intersection(choque_sl_accidente,test,colonias_cdmx)
choque_cl_accidente_final <- iterated_intersection(choque_cl_accidente,test,colonias_cdmx)
atropellado_lesionado_final<- iterated_intersection(atropellado_lesionado,test,colonias_cdmx)
moto_accidente_final <-  iterated_intersection(moto_accidente,test,colonias_cdmx)
total_intersected<- iterated_intersection(total_incidentes,test,colonias_cdmx)


# Cleaning Panel ----------------------------------------------------------

# Paste names
choque_sl_accidente_final <- cbind(CVEUT = colonias_cdmx$CVEUT,choque_sl_accidente_final) %>% select(!id)
choque_cl_accidente_final <- cbind(CVEUT = colonias_cdmx$CVEUT,choque_cl_accidente_final) %>% select(!id)
atropellado_lesionado_final <- cbind(CVEUT = colonias_cdmx$CVEUT,atropellado_lesionado_final) %>% select(!id)
moto_accidente_final <- cbind(CVEUT = colonias_cdmx$CVEUT,moto_accidente_final) %>% select(!id)
total_intersected <- cbind(CVEUT = colonias_cdmx$CVEUT,total_intersected) %>% select(!id)

# pivot

choque_sl_accidente_final <- choque_sl_accidente_final %>% pivot_longer(!CVEUT,names_to = "incidente_fecha",values_to = "choque_sl") 
choque_cl_accidente_final <- choque_cl_accidente_final %>% pivot_longer(!CVEUT,names_to = "incidente_fecha",values_to = "choque_cl") 
atropellado_lesionado_final <- atropellado_lesionado_final %>% pivot_longer(!CVEUT,names_to = "incidente_fecha",values_to = "atropellados") 
moto_accidente_final <- moto_accidente_final %>% pivot_longer(!CVEUT,names_to = "incidente_fecha",values_to = "moto") 
total_intersected <- total_intersected %>% pivot_longer(!CVEUT,names_to = "incidente_fecha",values_to = "total_inviales") 

# Remove everything starting with list(c( and until a whitespace (\s)

choque_sl_accidente_final$incidente_fecha <- str_remove(choque_sl_accidente_final$incidente_fecha, "list\\(c\\(.*\\s")
choque_cl_accidente_final$incidente_fecha <- str_remove(choque_cl_accidente_final$incidente_fecha, "list\\(c\\(.*\\s")
atropellado_lesionado_final$incidente_fecha <- str_remove(atropellado_lesionado_final$incidente_fecha, "list\\(c\\(.*\\s")
moto_accidente_final$incidente_fecha <- str_remove(moto_accidente_final$incidente_fecha, "list\\(c\\(.*\\s")

# Separate dates into columns
choque_sl_accidente_final <- choque_sl_accidente_final %>% separate(incidente_fecha,into = c("incidente", "ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
choque_cl_accidente_final <- choque_cl_accidente_final %>% separate(incidente_fecha,into = c("incidente", "ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
atropellado_lesionado_final <- atropellado_lesionado_final %>% separate(incidente_fecha,into = c("incidente", "ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
moto_accidente_final <- moto_accidente_final %>% separate(incidente_fecha,into = c("incidente", "ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
total_intersected <- total_intersected %>% separate(incidente_fecha,into = c("incidente", "ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")

# Make date and order 

choque_sl_accidente_final <- choque_sl_accidente_final %>% mutate(dates_complete = as.Date(paste0(ano_mes,"-01"))) 
choque_cl_accidente_final <- choque_cl_accidente_final %>% mutate(dates_complete = as.Date(paste0(ano_mes,"-01"))) 
atropellado_lesionado_final <- atropellado_lesionado_final %>% mutate(dates_complete = as.Date(paste0(ano_mes,"-01")))
moto_accidente_final <- moto_accidente_final %>% mutate(dates_complete = as.Date(paste0(ano_mes,"-01"))) 
total_intersected <- total_intersected %>% mutate(dates_complete = as.Date(paste0(ano_mes,"-01"))) 


# check numbers


# Final Panel 

final_panel <- dummy_df %>% left_join(choque_sl_accidente_final %>% select(-ano_mes, -incidente)) %>% 
  left_join(choque_cl_accidente_final %>% select(-ano_mes, -incidente)) %>% 
  left_join(atropellado_lesionado_final %>% select(-ano_mes, -incidente)) %>% 
  left_join(moto_accidente_final %>% select(-ano_mes, -incidente)) %>% 
  left_join(total_intersected %>% select(-ano_mes, -incidente)) 

# check numbers

final_panel %>% summarise(choque_sl = sum(choque_sl, na.rm = T), 
                          choque_cl = sum(choque_cl, na.rm = T), 
                          atropellados = sum(atropellados, na.rm = T), 
                          moto = sum(moto, na.rm = T),
                          total_inviales = sum(total_inviales, na.rm = T))

# Unclutter space 

rm(list = setdiff(ls(),c('final_panel')))

final_panel

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
l12b <- read_sf("input/mapas/stc_l12/stc_l1_l12_b500.shp") %>% filter(LINEA == 12) # Toda la línea 12
# L1
l1b <-  read_sf("input/mapas/stc_l1/stc_l1_ec500m.shp") # Estaciones Cerradas

colonias_cdmx <- st_transform(colonias_cdmx,st_crs(cablebus_1b)) # Cambiamos mapa a mismo CRS de otros, ahora todos tienen el mismo
colonias_cdmx <- colonias_cdmx %>% st_make_valid()

cb_1b <- lengths(st_intersects(colonias_cdmx,cablebus_1b))

cb_2b <- lengths(st_intersects(colonias_cdmx,cablebus_2b))

tr_eb <- lengths(st_intersects(colonias_cdmx,trole_elevadob))

tr_9b <- lengths(st_intersects(colonias_cdmx,trole_l9b))

l_1b <- lengths(st_intersects(colonias_cdmx,l1b))

l_12b <- lengths(st_intersects(colonias_cdmx,l12b))

# 0,1 Treatment

colonias_cdmx <- colonias_cdmx %>% cbind(cb_1b,cb_2b,tr_9b,tr_eb,l_1b,l_12b) %>% 
  mutate(cb_1b = ifelse(cb_1b>0,1,0), 
         cb_2b = ifelse(cb_2b>0,1,0), 
         tr_9b = ifelse(tr_9b>0,1,0), 
         tr_eb = ifelse(tr_eb>0,1,0),
         l_1b = ifelse(l_1b>0,1,0), 
         l_12b = ifelse(l_12b>0,1,0))

rm(cb_1b,cb_2b,tr_9b,tr_eb,l_1b,l1b,l_12b, cablebus_1b, cablebus_2b, trole_elevadob, trole_l9b) # Clear Space

# Panel with intersections
final_panel <- final_panel %>% left_join(colonias_cdmx %>% select(CVEUT, POB2010, cb_1b, cb_2b, tr_9b, tr_eb, l_1b, l_12b))

# did package panel formating  --------------------------------------------

dummy_id <- tibble(CVEUT = unique(final_panel$CVEUT), id_col = 1:length(unique(final_panel$CVEUT)))
numeric_date <- tibble( dates_complete = unique(final_panel$dates_complete), numeric_date = 1:length(unique(final_panel$dates_complete)))

inviales_panel <- final_panel %>% left_join(dummy_id) %>% left_join(numeric_date)

# Sanity check

inviales_panel %>% count(numeric_date) %>% count(n)
inviales_panel %>% count(id_col) %>% count(n)

# Final panel
rm(list = setdiff(ls(), c("inviales_panel")))






