####################################################.
## Callaway-Sant'Anna DiD model with Inviales 
## DAOA - 19/10/2023
####################################################.

# Preparar Espacio --------------------------------------------------------

pacman::p_load(did)
rm(list=setdiff(ls(), c("inviales_panel", 'carpetas_panel')))

# Splitting into treatment groups  -------------------------------------------------

# Starts:
# CB L1: 91
# CB L2: 92
# TR L9: 85
# TR L10: 106

# Stops: 
# L1: 103
# L12: 89
# L12 Reopening of 1st strech: 109
# Reopening of 2nd Strech: 116 (Last obs for now)

inviales_panel <- inviales_panel %>% 
  mutate(start =  cb_1b * 91 + cb_2b * 92 + tr_eb * 106 + tr_9b * 85) %>% # Start 
  mutate(start = ifelse(start == 198, 92,start)) %>% # Treated before
  mutate(stop = l_12b * 89 + l_1b * 103) %>% # Complete stops until january
  mutate(closed_l12 = ifelse(CVEUT %in% c('07-253', '07-007', '03-015', '07-260', '07-261', '03-029', 
                                         '03-031', '03-033', '07-037', '07-053', '07-059', '11-007', '07-066',
                                         '07-073', '07-077', '11-011', '07-083', '07-087', '11-056', '11-057', '11-019',
                                         '03-067', '11-021', '07-284', '07-128', '07-285', '07-286', '11-022', '11-024', 
                                         '07-134', '07-135', '11-025', '11-026', '07-142', '11-027', '07-176', '07-177',
                                         '07-178', '03-101', '11-032', '07-190', '07-191', '13-054', '11-038', '07-204', '11-041', 
                                         '07-212', '07-213', '07-214', '11-042', '11-045', '03-112', '07-234', '11-055',
                                         '11-058', '07-236', '07-237', '07-238', '07-240'),1,0)) %>% # Complete stops until July
  mutate(complete_stop = (l_1b * 103 + closed_l12 * 89)) %>% 
  select(-CVEUT) %>%
  select(id_col, numeric_date, start, stop, complete_stop, everything()) %>% 
  mutate(POB2010 = as.numeric(POB2010))

inviales_panel %>% count(complete_stop)

# Start Panel

start_panel <- inviales_panel %>% filter(stop == 0)

# Stop Panel

stop_panel <- inviales_panel %>% filter(start == 0)

# DiD Start Objects -------------------------------------------------------------

# Total Inviales
did_total_start <- att_gt(
  yname = "total_inviales",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  xformla = ~POB2010,
  data = inviales_panel
)
# Choques con Lesionados
did_choque_cl_start <- att_gt(
  yname = "choque_cl",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  xformla = ~POB2010,
  data = inviales_panel
)
# Choques Sin Lesionados
did_choque_sl_start <- att_gt(
  yname = "choque_sl",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  xformla = ~POB2010,
  data = inviales_panel
)
# Atropellados
did_atropellados_start <- att_gt(
  yname = "atropellados",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  xformla = ~POB2010,
  data = inviales_panel
)
# Accidentes en Moto (Empiezan a registrarse en el periodo 38)
did_moto_start <- att_gt(
  yname = "moto",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  xformla = ~POB2010,
  data = inviales_panel
)

# Start Effects -----------------------------------------------------------------

# ATT

agg_start_complete <- aggte(did_total_start, type = 'simple', na.rm = T)
agg_start_choquecl <- aggte(did_choque_cl_start, type = 'simple', na.rm = T)
agg_start_choquesl <- aggte(did_choque_sl_start, type = 'simple', na.rm = T)
agg_start_atropellados <- aggte(did_atropellados_start, type = 'simple', na.rm = T)
agg_start_moto <- aggte(did_moto_start, type = 'simple', na.rm = T)


# ATT starts
agg_start_complete 
agg_start_choquecl
agg_start_choquesl
agg_start_atropellados
agg_start_moto

# ATT(t)

agg_start_complete <- aggte(did_total_start, type = 'dynamic', na.rm = T)
agg_start_choquecl <- aggte(did_choque_cl_start, type = 'dynamic', na.rm = T)
agg_start_choquesl <- aggte(did_choque_sl_start, type = 'dynamic', na.rm = T)
agg_start_atropellados <- aggte(did_atropellados_start, type = 'dynamic', na.rm = T)
agg_start_moto <- aggte(did_moto_start, type = 'dynamic', na.rm = T)


# ATT(t) Plots
agg_start_complete %>% summary()
ggdid(agg_start_complete, title = "ATT(t) Start effect on complete Inviales")

agg_start_choquecl %>% summary()
ggdid(agg_start_choquecl, title = "ATT(t) Start effect on Choques con Lesionados")

agg_start_choquesl %>% summary()
ggdid(agg_start_choquesl, title = "ATT(t) Start effect on Choques sin Lesionados")

agg_start_atropellados %>% summary()
ggdid(agg_start_atropellados, title = "ATT(t) Start effect on Atropellados")

agg_start_moto %>% summary()
ggdid(agg_start_moto, title = "ATT(t) Start effect on Moto accidents")

# ATT(g)

agg_start_complete <- aggte(did_total_start, type = 'group', na.rm = T)
agg_start_choquecl <- aggte(did_choque_cl_start, type = 'group', na.rm = T)
agg_start_choquesl <- aggte(did_choque_sl_start, type = 'group', na.rm = T)
agg_start_atropellados <- aggte(did_atropellados_start, type = 'group', na.rm = T)
agg_start_moto <- aggte(did_moto_start, type = 'group', na.rm = T)

# ATT(g) Plots
agg_start_complete %>% summary()
ggdid(agg_start_complete, title = "ATT(g) Start effect on complete Inviales")

agg_start_choquecl %>% summary()
ggdid(agg_start_choquecl, title = "ATT(g) Start effect on Choques con Lesionados")

ggdid(agg_start_choquesl, title = "ATT(g) Start effect on Choques sin Lesionados")
agg_start_choquesl %>% summary()

agg_start_atropellados %>% summary()
ggdid(agg_start_atropellados, title = "ATT(g) Start effect on Atropellados")

agg_start_moto %>% summary()
ggdid(agg_start_moto, title = "ATT(g) Start effect on Moto accidents")

# DiD Complete Stop -------------------------------------------------------

# Total Inviales
did_total_stop <- att_gt(
  yname = "total_inviales",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  xformla = ~POB2010,
  data = inviales_panel
)
# Choques con Lesionados
did_choque_cl_stop <- att_gt(
  yname = "choque_cl",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  xformla = ~POB2010,
  data = inviales_panel
)
# Choques Sin Lesionados
did_choque_sl_stop <- att_gt(
  yname = "choque_sl",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  xformla = ~POB2010,
  data = inviales_panel
)
# Atropellados
did_atropellados_stop <- att_gt(
  yname = "atropellados",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  xformla = ~POB2010,
  data = inviales_panel
)

# Accidentes en Moto (Empiezan a registrarse en el periodo 38)
did_moto_stop <- att_gt(
  yname = "moto",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  xformla = ~POB2010,
  data = inviales_panel
)

# Stop Effects ------------------------------------------------------------

# ATT

agg_stop_complete <- aggte(did_total_stop, type = 'simple', na.rm = T)
agg_stop_choquecl <- aggte(did_choque_cl_stop, type = 'simple', na.rm = T)
agg_stop_choquesl <- aggte(did_choque_sl_stop, type = 'simple', na.rm = T)
agg_stop_atropellados <- aggte(did_atropellados_stop, type = 'simple', na.rm = T)
agg_stop_moto <- aggte(did_moto_stop, type = 'simple', na.rm = T)


# ATT starts
agg_stop_complete
agg_stop_choquecl
agg_stop_choquesl
agg_stop_atropellados
agg_stop_moto

# ATT(t)

agg_stop_complete <- aggte(did_total_stop, type = 'dynamic', na.rm = T)
agg_stop_choquecl <- aggte(did_choque_cl_stop, type = 'dynamic', na.rm = T)
agg_stop_choquesl <- aggte(did_choque_sl_stop, type = 'dynamic', na.rm = T)
agg_stop_atropellados <- aggte(did_atropellados_stop, type = 'dynamic', na.rm = T)
agg_stop_moto <- aggte(did_moto_stop, type = 'dynamic', na.rm = T)


# ATT(t) Plots
agg_stop_complete %>% summary()
ggdid(agg_stop_complete, title = "ATT(t) Stop effect on complete Inviales")

agg_stop_choquecl %>% summary()
ggdid(agg_stop_choquecl, title = "ATT(t) Stop effect on Choques con Lesionados")

agg_stop_choquesl %>% summary()
ggdid(agg_stop_choquesl, title = "ATT(t) Stop effect on Choques sin Lesionados")

agg_stop_atropellados %>% summary()
ggdid(agg_stop_atropellados, title = "ATT(t) Stop effect on Atropellados")

agg_stop_moto %>% summary()
ggdid(agg_stop_moto, title = "ATT(t) Stop effect on Moto accidents")

# ATT(g)

agg_stop_complete <- aggte(did_total_stop, type = 'group', na.rm = T)
agg_stop_choquecl <- aggte(did_choque_cl_stop, type = 'group', na.rm = T)
agg_stop_choquesl <- aggte(did_choque_sl_stop, type = 'group', na.rm = T)
agg_stop_atropellados <- aggte(did_atropellados_stop, type = 'group', na.rm = T)
agg_stop_moto <- aggte(did_moto_stop, type = 'group', na.rm = T)

# ATT(g) Plots
agg_stop_complete %>% summary()
ggdid(agg_stop_complete, title = "ATT(g) Stop effect on complete Inviales")

agg_stop_choquecl %>% summary()
ggdid(agg_stop_choquecl, title = "ATT(g) Stop effect on Choques con Lesionados")

agg_stop_choquesl %>% summary()
ggdid(agg_stop_choquesl, title = "ATT(t) Stop effect on Choques sin Lesionados")

agg_stop_atropellados %>% summary()
ggdid(agg_stop_atropellados, title = "ATT(t) Stop effect on Atropellados")

agg_stop_moto %>% summary()
ggdid(agg_stop_moto, title = "ATT(t) Stop effect on Moto accidents")

# DiD Filtered Stop -------------------------------------------------------

# Total Inviales
did_total_stop <- att_gt(
  yname = "total_inviales",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  xformla = ~POB2010,
  data = inviales_panel %>% filter(numeric_date<109)
)
# Choques con Lesionados
did_choque_cl_stop <- att_gt(
  yname = "choque_cl",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  xformla = ~POB2010,
  data = inviales_panel %>% filter(numeric_date<109)
)
# Choques Sin Lesionados
did_choque_sl_stop <- att_gt(
  yname = "choque_sl",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  xformla = ~POB2010,
  data = inviales_panel %>% filter(numeric_date<109)
)
# Atropellados
did_atropellados_stop <- att_gt(
  yname = "atropellados",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  xformla = ~POB2010,
  data = inviales_panel %>% filter(numeric_date<109)
)
# Accidentes en Moto (Empiezan a registrarse en el periodo 38)
did_moto_stop <- att_gt(
  yname = "moto",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  xformla = ~POB2010,
  data = inviales_panel %>% filter(numeric_date<109)
)

# Filtered Stop Effects ---------------------------------------------------

# ATT

agg_stop_complete <- aggte(did_total_stop, type = 'simple', na.rm = T)
agg_stop_choquecl <- aggte(did_choque_cl_stop, type = 'simple', na.rm = T)
agg_stop_choquesl <- aggte(did_choque_sl_stop, type = 'simple', na.rm = T)
agg_stop_atropellados <- aggte(did_atropellados_stop, type = 'simple', na.rm = T)
agg_stop_moto <- aggte(did_moto_stop, type = 'simple', na.rm = T)


# ATT starts
agg_stop_complete
agg_stop_choquecl
agg_stop_choquesl
agg_stop_atropellados
agg_stop_moto

# ATT(t)

agg_stop_complete <- aggte(did_total_stop, type = 'dynamic', na.rm = T)
agg_stop_choquecl <- aggte(did_choque_cl_stop, type = 'dynamic', na.rm = T)
agg_stop_choquesl <- aggte(did_choque_sl_stop, type = 'dynamic', na.rm = T)
agg_stop_atropellados <- aggte(did_atropellados_stop, type = 'dynamic', na.rm = T)
agg_stop_moto <- aggte(did_moto_stop, type = 'dynamic', na.rm = T)


# ATT(t) Plots
agg_stop_complete %>% summary()
ggdid(agg_stop_complete, title = "ATT(t) Stop effect on complete Inviales")

agg_stop_choquecl %>% summary()
ggdid(agg_stop_choquecl, title = "ATT(t) Stop effect on Choques con Lesionados")

agg_stop_choquesl %>% summary()
ggdid(agg_stop_choquesl, title = "ATT(t) Stop effect on Choques sin Lesionados")

agg_stop_atropellados %>% summary()
ggdid(agg_stop_atropellados, title = "ATT(t) Stop effect on Atropellados")

agg_stop_moto %>% summary()
ggdid(agg_stop_moto, title = "ATT(t) Stop effect on Moto accidents")

# ATT(g)

agg_stop_complete <- aggte(did_total_stop, type = 'group', na.rm = T)
agg_stop_choquecl <- aggte(did_choque_cl_stop, type = 'group', na.rm = T)
agg_stop_choquesl <- aggte(did_choque_sl_stop, type = 'group', na.rm = T)
agg_stop_atropellados <- aggte(did_atropellados_stop, type = 'group', na.rm = T)
agg_stop_moto <- aggte(did_moto_stop, type = 'group', na.rm = T)
 
# ATT(g) Plots
agg_stop_complete %>% summary()
ggdid(agg_stop_complete, title = "ATT(g) Stop effect on complete Inviales")

agg_stop_choquecl %>% summary()
ggdid(agg_stop_choquecl, title = "ATT(g) Stop effect on Choques con Lesionados")

agg_stop_choquesl %>% summary()
ggdid(agg_stop_choquesl, title = "ATT(t) Stop effect on Choques sin Lesionados")

agg_stop_atropellados %>% summary()
ggdid(agg_stop_atropellados, title = "ATT(t) Stop effect on Atropellados")

agg_stop_moto %>% summary()
ggdid(agg_stop_moto, title = "ATT(t) Stop effect on Moto accidents")

























