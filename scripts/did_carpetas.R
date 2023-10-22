####################################################.
## Callaway-Sant'Anna DiD model with Inviales 
## DAOA - 20/10/2023
####################################################.

# Preparar Espacio --------------------------------------------------------

pacman::p_load(did)
rm(list=setdiff(ls(), c("inviales_panel", 'carpetas_panel')))

# Data Intersection -------------------------------------------------------

# Starts:
# CB L1: 67
# CB L2: 68
# TR L9: 62
# TR L10: 82

# Stops: 
# L1: 79
# L12: 65
# L12 Reopening of 1st strech: 85
# Reopening of 2nd Strech: 91 (Last obs for now)
carpetas_panel %>% count(numeric_date, dates_complete) %>% View()
carpetas_panel <- carpetas_panel %>% 
  mutate(start =  cb_1b * 67 + cb_2b * 68 + tr_eb * 82 + tr_9b * 62) %>% # Start 
  mutate(start = ifelse(start == 150, 68,start)) %>% # Treated before
  mutate(stop = l_12b * 65 + l_1b * 79) %>% # Complete stops until january
  mutate(closed_l12 = ifelse(CVEUT %in% c('07-253', '07-007', '03-015', '07-260', '07-261', '03-029', 
                                          '03-031', '03-033', '07-037', '07-053', '07-059', '11-007', '07-066',
                                          '07-073', '07-077', '11-011', '07-083', '07-087', '11-056', '11-057', '11-019',
                                          '03-067', '11-021', '07-284', '07-128', '07-285', '07-286', '11-022', '11-024', 
                                          '07-134', '07-135', '11-025', '11-026', '07-142', '11-027', '07-176', '07-177',
                                          '07-178', '03-101', '11-032', '07-190', '07-191', '13-054', '11-038', '07-204', '11-041', 
                                          '07-212', '07-213', '07-214', '11-042', '11-045', '03-112', '07-234', '11-055',
                                          '11-058', '07-236', '07-237', '07-238', '07-240'),1,0)) %>% # Complete stops until July
  mutate(complete_stop = (l_1b * 79 + closed_l12 * 65)) %>% 
  select(-CVEUT, -dates_complete) %>%
  select(id_col, numeric_date, start, stop, complete_stop, everything()) %>% 
  mutate(POB2010 = as.numeric(POB2010))

# Start Panel

start_panel <- carpetas_panel %>% filter(stop == 0)

# Stop Panel

stop_panel <- carpetas_panel %>% filter(start == 0)

# DiD Start Object --------------------------------------------------------

# Total Delitos
did_total_start <- att_gt(
  yname = "total_delitos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  xformla = ~POB2010,
  data = start_panel
)
# Robos
did_robos_start <- att_gt(
  yname = "robos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  xformla = ~POB2010,
  data = start_panel
)
# Delitos Sexuales
did_dsex_start <- att_gt(
  yname = "d_sexuales",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  xformla = ~POB2010,
  data = start_panel
)
# Asesinatos
did_asesinatos_start <- att_gt(
  yname = "asesinatos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  xformla = ~POB2010,
  data = start_panel
)
# Violencia Familiar
did_violfam_start <- att_gt(
  yname = "viol_fam",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  xformla = ~POB2010,
  data = start_panel
)
# Lesiones - Daños
did_lesiones_start <- att_gt(
  yname = "lesiones_danios",
  tname = "numeric_date",
  idname = "id_col",
  gname = "start",
  xformla = ~POB2010,
  data = start_panel
)

# Start Effects -----------------------------------------------------------------

# ATT

agg_start_complete <- aggte(did_total_start, type = 'simple', na.rm = T)
agg_robos_start <- aggte(did_robos_start, type = 'simple', na.rm = T)
agg_dsex_start <- aggte(did_dsex_start, type = 'simple', na.rm = T)
agg_asesinatos_start <- aggte(did_asesinatos_start, type = 'simple', na.rm = T)
agg_violfam_start <- aggte(did_violfam_start, type = 'simple', na.rm = T)
agg_lesiones_start <- aggte(did_lesiones_start, type = 'simple', na.rm = T)

# ATT starts
agg_start_complete
agg_robos_start
agg_dsex_start
agg_asesinatos_start
agg_violfam_start
agg_lesiones_start

# ATT(t)

agg_start_complete <- aggte(did_total_start, type = 'dynamic', na.rm = T)
agg_robos_start <- aggte(did_robos_start, type = 'dynamic', na.rm = T)
agg_dsex_start <- aggte(did_dsex_start, type = 'dynamic', na.rm = T)
agg_asesinatos_start <- aggte(did_asesinatos_start, type = 'dynamic', na.rm = T)
agg_violfam_start <- aggte(did_violfam_start, type = 'dynamic', na.rm = T)
agg_lesiones_start <- aggte(did_lesiones_start, type = 'dynamic', na.rm = T)

# ATT(t) Plots
agg_start_complete %>% summary()
ggdid(agg_start_complete, title = "ATT(t) Start effect on complete reported crimes")

agg_robos_start %>% summary()
ggdid(agg_robos_start, title = "ATT(t) Start effect on theft reports")

agg_dsex_start %>% summary()
ggdid(agg_dsex_start, title = "ATT(t) Start effect on Sex crimes")

agg_asesinatos_start %>% summary()
ggdid(agg_asesinatos_start, title = "ATT(t) Start effect on homicides")

agg_violfam_start %>% summary()
ggdid(agg_violfam_start, title = "ATT(t) Start effect on family violence")

agg_lesiones_start %>% summary()
ggdid(agg_lesiones_start, title = "ATT(t) Start effect on injuries and damages")

# ATT(g)

agg_start_complete <- aggte(did_total_start, type = 'group', na.rm = T)
agg_robos_start <- aggte(did_robos_start, type = 'group', na.rm = T)
agg_dsex_start <- aggte(did_dsex_start, type = 'group', na.rm = T)
agg_asesinatos_start <- aggte(did_asesinatos_start, type = 'group', na.rm = T)
agg_violfam_start <- aggte(did_violfam_start, type = 'group', na.rm = T)
agg_lesiones_start <- aggte(did_lesiones_start, type = 'group', na.rm = T)

# ATT(g) Plots
agg_start_complete %>% summary()
ggdid(agg_start_complete, title = "ATT(g) Start effect on complete reported crimes")

agg_robos_start %>% summary()
ggdid(agg_robos_start, title = "ATT(g) Start effect on theft reports") # Diminishes significantly CB L1

agg_dsex_start %>% summary()
ggdid(agg_dsex_start, title = "ATT(g) Start effect on Sex crimes")

agg_asesinatos_start %>% summary()
ggdid(agg_asesinatos_start, title = "ATT(g) Start effect on homicides")

agg_violfam_start %>% summary()
ggdid(agg_violfam_start, title = "ATT(g) Start effect on family violence")

agg_lesiones_start %>% summary()
ggdid(agg_lesiones_start, title = "ATT(g) Start effect on injuries and damages")


# DiD Stop Object ---------------------------------------------------------

# Total Delitos
did_total_stop <- att_gt(
  yname = "total_delitos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  xformla = ~POB2010,
  data = stop_panel
)
# Robos
did_robos_stop <- att_gt(
  yname = "robos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  xformla = ~POB2010,
  data = stop_panel
)
# Delitos Sexuales
did_dsex_stop <- att_gt(
  yname = "d_sexuales",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  xformla = ~POB2010,
  data = stop_panel
)
# Asesinatos
did_asesinatos_stop <- att_gt(
  yname = "asesinatos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  xformla = ~POB2010,
  data = stop_panel
)
# Violencia Familiar
did_violfam_stop <- att_gt(
  yname = "viol_fam",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  xformla = ~POB2010,
  data = stop_panel
)
# Lesiones - Daños
did_lesiones_stop <- att_gt(
  yname = "lesiones_danios",
  tname = "numeric_date",
  idname = "id_col",
  gname = "complete_stop",
  xformla = ~POB2010,
  data = stop_panel
)


# Complete Stop Effects ---------------------------------------------------


# ATT

agg_stop_complete <- aggte(did_total_stop, type = 'simple', na.rm = T)
agg_robos_stop <- aggte(did_robos_stop, type = 'simple', na.rm = T)
agg_dsex_stop <- aggte(did_dsex_stop, type = 'simple', na.rm = T)
agg_asesinatos_stop <- aggte(did_asesinatos_stop, type = 'simple', na.rm = T)
agg_violfam_stop <- aggte(did_violfam_stop, type = 'simple', na.rm = T)
agg_lesiones_stop <- aggte(did_lesiones_stop, type = 'simple', na.rm = T)

# ATT starts
agg_stop_complete
agg_robos_stop
agg_dsex_stop
agg_asesinatos_stop
agg_violfam_stop
agg_lesiones_stop

# ATT(t)

agg_stop_complete <- aggte(did_total_stop, type = 'dynamic', na.rm = T)
agg_robos_stop <- aggte(did_robos_stop, type = 'dynamic', na.rm = T)
agg_dsex_stop <- aggte(did_dsex_stop, type = 'dynamic', na.rm = T)
agg_asesinatos_stop <- aggte(did_asesinatos_stop, type = 'dynamic', na.rm = T)
agg_violfam_stop <- aggte(did_violfam_stop, type = 'dynamic', na.rm = T)
agg_lesiones_stop <- aggte(did_lesiones_stop, type = 'dynamic', na.rm = T)

# ATT(t) Plots
agg_stop_complete %>% summary()
ggdid(agg_stop_complete, title = "ATT(t) Stop effect on complete reported crimes")

agg_robos_stop %>% summary()
ggdid(agg_robos_stop, title = "ATT(t) Stop effect on theft reports")

agg_dsex_stop %>% summary()
ggdid(agg_dsex_stop, title = "ATT(t) Stop effect on Sex crimes")

agg_asesinatos_stop %>% summary()
ggdid(agg_asesinatos_stop, title = "ATT(t) Stop effect on homicides")

agg_violfam_stop %>% summary()
ggdid(agg_violfam_stop, title = "ATT(t) Stop effect on family violence")

agg_lesiones_stop %>% summary()
ggdid(agg_lesiones_stop, title = "ATT(t) Stop effect on injuries and damages")

# ATT(g)

agg_stop_complete <- aggte(did_total_stop, type = 'group', na.rm = T)
agg_robos_stop <- aggte(did_robos_stop, type = 'group', na.rm = T)
agg_dsex_stop <- aggte(did_dsex_stop, type = 'group', na.rm = T)
agg_asesinatos_stop <- aggte(did_asesinatos_stop, type = 'group', na.rm = T)
agg_violfam_stop <- aggte(did_violfam_stop, type = 'group', na.rm = T)
agg_lesiones_stop <- aggte(did_lesiones_stop, type = 'group', na.rm = T)

# ATT(g) Plots
agg_stop_complete %>% summary()
ggdid(agg_stop_complete, title = "ATT(g) Stop effect on complete reported crimes")

agg_robos_stop %>% summary()
ggdid(agg_robos_stop, title = "ATT(g) Stop effect on theft reports") # Diminishes significantly CB L1

agg_dsex_stop %>% summary()
ggdid(agg_dsex_stop, title = "ATT(g) Stop effect on Sex crimes")

agg_asesinatos_stop %>% summary()
ggdid(agg_asesinatos_stop, title = "ATT(g) Stop effect on homicides")

agg_violfam_stop %>% summary()
ggdid(agg_violfam_stop, title = "ATT(g) Stop effect on family violence")

agg_lesiones_stop %>% summary()
ggdid(agg_lesiones_stop, title = "ATT(g) Stop effect on injuries and damages")

# DiD Stop Object ---------------------------------------------------------

# Total Delitos
did_total_stop <- att_gt(
  yname = "total_delitos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  xformla = ~POB2010,
  data = stop_panel %>% filter(numeric_date<=85)
)
# Robos
did_robos_stop <- att_gt(
  yname = "robos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  xformla = ~POB2010,
  data = stop_panel%>% filter(numeric_date<=85)
)
# Delitos Sexuales
did_dsex_stop <- att_gt(
  yname = "d_sexuales",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  xformla = ~POB2010,
  data = stop_panel%>% filter(numeric_date<=85)
)
# Asesinatos
did_asesinatos_stop <- att_gt(
  yname = "asesinatos",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  xformla = ~POB2010,
  data = stop_panel%>% filter(numeric_date<=85)
)
# Violencia Familiar
did_violfam_stop <- att_gt(
  yname = "viol_fam",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  xformla = ~POB2010,
  data = stop_panel%>% filter(numeric_date<=85)
)
# Lesiones - Daños
did_lesiones_stop <- att_gt(
  yname = "lesiones_danios",
  tname = "numeric_date",
  idname = "id_col",
  gname = "stop",
  xformla = ~POB2010,
  data = stop_panel%>% filter(numeric_date<=85)
)

# Filtered Stop Effects ---------------------------------------------------

# ATT

agg_stop_complete <- aggte(did_total_stop, type = 'simple', na.rm = T)
agg_robos_stop <- aggte(did_robos_stop, type = 'simple', na.rm = T)
agg_dsex_stop <- aggte(did_dsex_stop, type = 'simple', na.rm = T)
agg_asesinatos_stop <- aggte(did_asesinatos_stop, type = 'simple', na.rm = T)
agg_violfam_stop <- aggte(did_violfam_stop, type = 'simple', na.rm = T)
agg_lesiones_stop <- aggte(did_lesiones_stop, type = 'simple', na.rm = T)

# ATT starts
agg_stop_complete
agg_robos_stop
agg_dsex_stop
agg_asesinatos_stop
agg_violfam_stop
agg_lesiones_stop

# ATT(t)

agg_stop_complete <- aggte(did_total_stop, type = 'dynamic', na.rm = T)
agg_robos_stop <- aggte(did_robos_stop, type = 'dynamic', na.rm = T)
agg_dsex_stop <- aggte(did_dsex_stop, type = 'dynamic', na.rm = T)
agg_asesinatos_stop <- aggte(did_asesinatos_stop, type = 'dynamic', na.rm = T)
agg_violfam_stop <- aggte(did_violfam_stop, type = 'dynamic', na.rm = T)
agg_lesiones_stop <- aggte(did_lesiones_stop, type = 'dynamic', na.rm = T)

# ATT(t) Plots
agg_stop_complete %>% summary()
ggdid(agg_stop_complete, title = "ATT(t) Stop effect on complete reported crimes")

agg_robos_stop %>% summary()
ggdid(agg_robos_stop, title = "ATT(t) Stop effect on theft reports")

agg_dsex_stop %>% summary()
ggdid(agg_dsex_stop, title = "ATT(t) Stop effect on Sex crimes")

agg_asesinatos_stop %>% summary()
ggdid(agg_asesinatos_stop, title = "ATT(t) Stop effect on homicides")

agg_violfam_stop %>% summary()
ggdid(agg_violfam_stop, title = "ATT(t) Stop effect on family violence")

agg_lesiones_stop %>% summary()
ggdid(agg_lesiones_stop, title = "ATT(t) Stop effect on injuries and damages")

# ATT(g)

agg_stop_complete <- aggte(did_total_stop, type = 'group', na.rm = T)
agg_robos_stop <- aggte(did_robos_stop, type = 'group', na.rm = T)
agg_dsex_stop <- aggte(did_dsex_stop, type = 'group', na.rm = T)
agg_asesinatos_stop <- aggte(did_asesinatos_stop, type = 'group', na.rm = T)
agg_violfam_stop <- aggte(did_violfam_stop, type = 'group', na.rm = T)
agg_lesiones_stop <- aggte(did_lesiones_stop, type = 'group', na.rm = T)

# ATT(g) Plots
agg_stop_complete %>% summary()
ggdid(agg_stop_complete, title = "ATT(g) Stop effect on complete reported crimes")

agg_robos_stop %>% summary()
ggdid(agg_robos_stop, title = "ATT(g) Stop effect on theft reports") # Diminishes significantly CB L1

agg_dsex_stop %>% summary()
ggdid(agg_dsex_stop, title = "ATT(g) Stop effect on Sex crimes")

agg_asesinatos_stop %>% summary()
ggdid(agg_asesinatos_stop, title = "ATT(g) Stop effect on homicides")

agg_violfam_stop %>% summary()
ggdid(agg_violfam_stop, title = "ATT(g) Stop effect on family violence")

agg_lesiones_stop %>% summary()
ggdid(agg_lesiones_stop, title = "ATT(g) Stop effect on injuries and damages")
















