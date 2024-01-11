## Borusyak et al. did imputation
# Using didimputation CRAN 
# DAOA - 22/10/2023

# Prepare Space -----------------------------------------------------------
rm(list = setdiff(ls(), c('carpetas_panel', 'inviales_panel')))
dev.off()
pacman::p_load(didimputation)


# Filter into start stop --------------------------------------------------

# Start Panel

start_panel <- inviales_panel %>% filter(stop == 0)

# Stop Panel

stop_panel <- inviales_panel %>% filter(start == 0)


# DiD Start Imputation -------------------------------------------------------------

# Total Inviales
did_total_start <- did_imputation(
  yname = "total_inviales",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  data = start_panel
)
# Choques con Lesionados
did_choque_cl_start <- did_imputation(
  yname = "choque_cl",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  data = start_panel
)

# Choques Sin Lesionados
did_choque_sl_start <- did_imputation(
  yname = "choque_sl",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  data = start_panel
)

# Atropellados
did_atropellados_start <- did_imputation(
  yname = "atropellados",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  data = start_panel
)

# Accidentes en Moto (Empiezan a registrarse en el periodo 38)
did_moto_start <- did_imputation(
  yname = "moto",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  data = start_panel
)

# Results

did_total_start
did_choque_cl_start
did_choque_sl_start
did_atropellados_start
did_moto_start


# Stop effects -----------------------------------------------------------


# Total Inviales
did_total_stop <- did_imputation(
  yname = "total_inviales",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  data = stop_panel
)
# Choques con Lesionados
did_choque_cl_stop <- did_imputation(
  yname = "choque_cl",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  data = stop_panel
)

# Choques Sin Lesionados
did_choque_sl_stop <- did_imputation(
  yname = "choque_sl",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  data = stop_panel
)

# Atropellados
did_atropellados_stop <- did_imputation(
  yname = "atropellados",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  data = stop_panel
)

# Accidentes en Moto (Empiezan a registrarse en el periodo 38)
did_moto_stop <- did_imputation(
  yname = "moto",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  data = stop_panel
)


# Results

did_total_stop
did_choque_cl_stop
did_choque_sl_stop
did_atropellados_stop
did_moto_stop


# Filtered Stop effects -----------------------------------------------------------


# Total Inviales
did_total_stop <- did_imputation(
  yname = "total_inviales",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  data = stop_panel %>% filter(numeric_date<109)
)
# Choques con Lesionados
did_choque_cl_stop <- did_imputation(
  yname = "choque_cl",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  data = stop_panel%>% filter(numeric_date<109)
)

# Choques Sin Lesionados
did_choque_sl_stop <- did_imputation(
  yname = "choque_sl",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  data = stop_panel%>% filter(numeric_date<109)
)

# Atropellados
did_atropellados_stop <- did_imputation(
  yname = "atropellados",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  data = stop_panel%>% filter(numeric_date<109)
)

# Accidentes en Moto (Empiezan a registrarse en el periodo 38)
did_moto_stop <- did_imputation(
  yname = "moto",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  data = stop_panel%>% filter(numeric_date<109)
)


# Results

did_total_stop
did_choque_cl_stop
did_choque_sl_stop
did_atropellados_stop
did_moto_stop

# Crime Record Effects ----------------------------------------------------

# Filter into start stop --------------------------------------------------

# Start Panel

start_panel <- carpetas_panel %>% filter(stop == 0)

# Stop Panel

stop_panel <- carpetas_panel %>% filter(start == 0)


# Start Effects -----------------------------------------------------------


start_panel

# Total Delitos
did_total_start <- did_imputation(
  yname = "total_delitos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  data = start_panel
)
# Robos
did_robos_start <- did_imputation(
  yname = "robos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  data = start_panel
)
# Delitos Sexuales
did_dsex_start <- did_imputation(
  yname = "d_sexuales",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  data = start_panel
)
# Asesinatos
did_asesinatos_start <- did_imputation(
  yname = "asesinatos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  data = start_panel
)
# Violencia Familiar
did_violfam_start <- did_imputation(
  yname = "viol_fam",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  data = start_panel
)
# Lesiones - Daños
did_lesiones_start <- did_imputation(
  yname = "lesiones_danios",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  data = start_panel
)

# Results
did_total_start
did_robos_start
did_dsex_start
did_asesinatos_start
did_violfam_start
did_lesiones_start


# Stop DiD ----------------------------------------------------------------


# Total Delitos
did_total_stop <- did_imputation(
  yname = "total_delitos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  data = stop_panel
)
# Robos
did_robos_stop <- did_imputation(
  yname = "robos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  data = stop_panel
)
# Delitos Sexuales
did_dsex_stop <- did_imputation(
  yname = "d_sexuales",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  data = stop_panel
)
# Asesinatos
did_asesinatos_stop <- did_imputation(
  yname = "asesinatos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  data = stop_panel
)
# Violencia Familiar
did_violfam_stop <- did_imputation(
  yname = "viol_fam",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  data = stop_panel
)
# Lesiones - Daños
did_lesiones_stop <- did_imputation(
  yname = "lesiones_danios",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  data = stop_panel
)

# Results
did_total_stop
did_robos_stop
did_dsex_stop
did_asesinatos_stop
did_violfam_stop
did_lesiones_stop

# Filtered Stop DiD ----------------------------------------------------------------

# Total Delitos
did_total_stop <- did_imputation(
  yname = "total_delitos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  data = stop_panel%>% filter(numeric_date<=85)
)
# Robos
did_robos_stop <- did_imputation(
  yname = "robos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  data = stop_panel%>% filter(numeric_date<=85)
)
# Delitos Sexuales
did_dsex_stop <- did_imputation(
  yname = "d_sexuales",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  data = stop_panel%>% filter(numeric_date<=85)
)
# Asesinatos
did_asesinatos_stop <- did_imputation(
  yname = "asesinatos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  data = stop_panel%>% filter(numeric_date<=85)
)
# Violencia Familiar
did_violfam_stop <- did_imputation(
  yname = "viol_fam",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  data = stop_panel%>% filter(numeric_date<=85)
)
# Lesiones - Daños
did_lesiones_stop <- did_imputation(
  yname = "lesiones_danios",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  data = stop_panel%>% filter(numeric_date<=85)
)

# Results
did_total_stop
did_robos_stop
did_dsex_stop
did_asesinatos_stop
did_violfam_stop
did_lesiones_stop






