######################### Construcción de la base de datos ##############################

#### Carga de paquetes ####

library(tidyverse)
library(haven)
library(modelsummary)
library(countrycode)


#### Carga de datos ####

juicios <- read_dta("datos/barsbargains_replication/barsbargains.dta") %>% rename("break_d" = "break")

load("datos/TCs_replication/data/dataset_cy.RData")

##################### Recodificación de variables ##############################

#### cow code in tc data #####

data$ccode <- countrycode::countrycode(data$country, origin = "country.name", destination = "cown")

data$ccode[data$country == "Serbia"] <- 345

jt_datos <- juicios %>%
  left_join(data %>%
              select(ccode, year, dem_time, contains("tc_"), region, latentmean_fariss, cumulative_fariss, TC_reg_density_sample, trans_type, internal), by = c("ccode", "year"))

golpes <- read_csv("datos/golpes.csv")

jt_datos <- jt_datos %>%
  left_join(golpes %>%
              select(ccode = cowcode, year, intento_golpe = coup, golpe = successful) %>%
              group_by(year, ccode) %>%
              summarise(intento_golpe = sum(intento_golpe), golpe = max(golpe)) %>%
              ungroup(),
            by = c("ccode", "year"))


#### creando base de datos_tesis ####


jt_datos %>%
  select(country, year, trtpros)

# Selección y creación de variables de interés

datos_tesis <- jt_datos %>%
  select(
    id_transicion = dtr_id,
    codigo_pais = ccode,
    pais = country,
    region,
    anos_transicion = dtr_t2,
    anos_democracia = dem_time,
    ano = year,
    proteccion_ddhh = latentmean_fariss,
    enjuiciamientos = trtpros,
    sentencias_acum_l1 = trtgsum_l1,
    amnistias = amnesty_fit,
    amnistias_dummy = ambin,
    comisiones = tc_ongoing_sum,
    comisiones_dummy = tc_hashadany,
    comisiones_finalizadas_acum = tc_past_sum,
    comisiones_reporte = tc_finalreport,
    comisiones_reporte_dummy = tc_finalreport_first,
    comisiones_reportepublico = tc_publicreport,
    comisiones_reportepublico_dummy = tc_publicreport_first,
    comisiones_recomendaciones = tc_recommendations,
    comisiones_recomendaciones_dummy = tc_recommendations_first,
    comisiones_reco_enjuiciamiento = tc_prosecute,
    comisiones_reco_enjuiciamiento_dummy = tc_prosecute_first,
    comisiones_reco_reforma = tc_reform,
    comisiones_reco_reforma_dummy = tc_reform_first,
    conflicto_armado = internal,
    polity2,
    poblacion_log = poplog,
    proteccion_previa_ddhh = cumulative_fariss,
    pib_log = loggdp,
    tipo_transicion = trans_type,
    juicios_regionales = rtper,
    comisiones_regionales = TC_reg_density_sample,
    independencia_pjud = lsji,
    intento_golpe,
    golpe,
    trtst
  )

# recodificación de variables de interés

datos_tesis <- datos_tesis %>%
  mutate(intento_golpe = replace_na(intento_golpe,0),
         golpe = replace_na(golpe,0),
         conflicto_armado = as.double(conflicto_armado),
         conflicto_armado = factor(conflicto_armado, labels = c("none", "conflict", "war")),
         tipo_transicion = if_else(tipo_transicion == "negotiated", 1, 0),
         tipo_transicion = factor(tipo_transicion, labels = c("other", "negotiated")))

datos_tesis <- datos_tesis %>%
  group_by(id_transicion) %>%
  mutate(
    enjuiciamientos_acum = cumsum(enjuiciamientos),
    enjuiciamientos_dummy = if_else(max(enjuiciamientos_acum) > 0, 1, 0),
    enjuiciamientos_acum_l1 = dplyr::lag(enjuiciamientos_acum, 1),
    enjuiciamientos_acum_l2 = dplyr::lag(enjuiciamientos_acum, 2),
    amnistias_acum = cumsum(amnistias),
    amnistias_acum_l1 = dplyr::lag(amnistias_acum, 1),
    amnistias_acum_l2 = dplyr::lag(amnistias_acum, 2),
    comisiones_acum = cumsum(comisiones),
    comisiones_acum_l1 = dplyr::lag(comisiones_acum, 1),
    comisiones_acum_l2 = dplyr::lag(comisiones_acum, 2),
    comisiones_dummyl1 = dplyr::lag(comisiones_dummy, 1),
    comisiones_dummyl2 = dplyr::lag(comisiones_dummy, 2),
    comisiones_finalizadas_acum_l1 = dplyr::lag(comisiones_finalizadas_acum, 1),
    comisiones_finalizadas_acum_l2 = dplyr::lag(comisiones_finalizadas_acum, 2),
    comisiones_finalizadas_dummy = if_else(max(comisiones_finalizadas_acum) > 0, 1, 0),
    comisiones_reco_reforma_l1 = dplyr::lag(comisiones_reco_reforma, 1),
    comisiones_reco_enjuiciamiento_l1 = dplyr::lag(comisiones_reco_enjuiciamiento, 1),
    sentencias_acum = dplyr::lead(sentencias_acum_l1, 1),
    sentencias_acum_l2 = dplyr::lag(sentencias_acum_l1, 1),
    proteccion_ddhh_l1 = dplyr::lag(proteccion_ddhh, 1)) %>%
  ungroup()


# Estiquetas

library(labelled)

datos_tesis$golpe <- labelled(datos_tesis$golpe, labels = c("No" = 0, "Sí" = 1))
datos_tesis$enjuiciamientos_dummy <- labelled(datos_tesis$enjuiciamientos_dummy, labels = c("No" = 0, "Sí" = 1))
datos_tesis$amnistias_dummy <- labelled(datos_tesis$amnistias_dummy, labels = c("No" = 0, "Sí" = 1))
datos_tesis$comisiones_dummy <- labelled(datos_tesis$comisiones_dummy, labels = c("No" = 0, "Sí" = 1))
datos_tesis$comisiones_finalizadas_dummy <- labelled(datos_tesis$comisiones_finalizadas_dummy, labels = c("No" = 0, "Sí" = 1))
datos_tesis$comisiones_reporte <- labelled(datos_tesis$comisiones_reporte, labels = c("No" = 0, "Sí" = 1))
datos_tesis$comisiones_reporte_dummy <- labelled(datos_tesis$comisiones_reporte_dummy, labels = c("No" = 0, "Sí" = 1))
datos_tesis$comisiones_reportepublico <- labelled(datos_tesis$comisiones_reportepublico, labels = c("No" = 0, "Sí" = 1))
datos_tesis$comisiones_reportepublico_dummy <- labelled(datos_tesis$comisiones_reportepublico_dummy, labels = c("No" = 0, "Sí" = 1))
datos_tesis$comisiones_recomendaciones <- labelled(datos_tesis$comisiones_recomendaciones, labels = c("No" = 0, "Sí" = 1))
datos_tesis$comisiones_recomendaciones_dummy <- labelled(datos_tesis$comisiones_recomendaciones_dummy, labels = c("No" = 0, "Sí" = 1))
datos_tesis$comisiones_reco_enjuiciamiento <- labelled(datos_tesis$comisiones_reco_enjuiciamiento, labels = c("No" = 0, "Sí" = 1))
datos_tesis$comisiones_reco_enjuiciamiento_dummy <- labelled(datos_tesis$comisiones_reco_enjuiciamiento_dummy, labels = c("No" = 0, "Sí" = 1))
datos_tesis$comisiones_reco_reforma <- labelled(datos_tesis$comisiones_reco_reforma, labels = c("No" = 0, "Sí" = 1))
datos_tesis$comisiones_reco_reforma_dummy <- labelled(datos_tesis$comisiones_reco_reforma_dummy, labels = c("No" = 0, "Sí" = 1))

var_label(datos_tesis$id_transicion) <- "ID de transición"
var_label(datos_tesis$pais) <- "País"
var_label(datos_tesis$intento_golpe) <- "Intentos de golpes de estado"
var_label(datos_tesis$golpe) <- "Golpe de estado completo"
var_label(datos_tesis$region) <- "Región"
var_label(datos_tesis$anos_transicion) <- "Años de transición"
var_label(datos_tesis$ano) <- "Año"
var_label(datos_tesis$proteccion_ddhh) <- "Índice de protección de DDHH"
var_label(datos_tesis$proteccion_ddhh_l1) <- "Índice de protección de DDHH T-1"
var_label(datos_tesis$enjuiciamientos) <- "Enjuiciamientos"
var_label(datos_tesis$enjuiciamientos_acum) <- "Enjuiciamientos acumulados"
var_label(datos_tesis$enjuiciamientos_acum_l1) <- "Enjuiciamientos acumulados T-1"
var_label(datos_tesis$enjuiciamientos_acum_l2) <- "Enjuiciamientos acumulados T-2"
var_label(datos_tesis$enjuiciamientos_dummy) <- "Al menos un enjuiciamiento"
var_label(datos_tesis$sentencias_acum) <- "Sentencias acumuladas"
var_label(datos_tesis$sentencias_acum_l1) <- "Sentencias acumuladas T-1"
var_label(datos_tesis$sentencias_acum_l2) <- "Sentencias acumuladas T-2"
var_label(datos_tesis$amnistias) <- "Amnistías"
var_label(datos_tesis$amnistias_acum) <- "Amnistías acumuladas"
var_label(datos_tesis$amnistias_acum_l1) <- "Amnistías acumuladas T-1"
var_label(datos_tesis$amnistias_acum_l2) <- "Amnistías acumuladas T-2"
var_label(datos_tesis$amnistias_dummy) <- "Al menos una amnistía"
var_label(datos_tesis$comisiones) <- "Comisiones de verdad en funcionamiento"
var_label(datos_tesis$comisiones_acum) <- "Comisiones de verdad en funcionamiento acumuladas"
var_label(datos_tesis$comisiones_acum_l1) <- "Comisiones de verdad en funcionamiento acumuladas T-1"
var_label(datos_tesis$comisiones_acum_l2) <- "Comisiones de verdad en funcionamiento acumuladas T-2"
var_label(datos_tesis$comisiones_dummy) <- "Al menos una comisión de verdad en funcionamiento"
var_label(datos_tesis$comisiones_finalizadas_acum) <- "Comisiones finalizadas acumuladas"
var_label(datos_tesis$comisiones_finalizadas_acum_l1) <- "Comisiones finalizadas acumuladas T-1"
var_label(datos_tesis$comisiones_finalizadas_acum_l2) <- "Comisiones finalizadas acumuladas T-2"
var_label(datos_tesis$comisiones_finalizadas_dummy) <- "Al menos una comisión finalizada"
var_label(datos_tesis$comisiones_reco_enjuiciamiento_l1) <- "Comisión con recomendación de enjuiciamientos T-1"
var_label(datos_tesis$comisiones_reco_reforma_l1) <- "Comisión con recomendación de reformas T-1"
var_label(datos_tesis$conflicto_armado) <- "Guerra civil"
var_label(datos_tesis$tipo_transicion) <- "Tipo de transición democrática"
var_label(datos_tesis$proteccion_previa_ddhh) <- "Protección previa de DDHH"
var_label(datos_tesis$comisiones_reporte) <- "¿Esa comisión elaboró un reporte?"
var_label(datos_tesis$comisiones_reporte_dummy) <- "Al menos una comisión elaboró un reporte"
var_label(datos_tesis$comisiones_reportepublico) <- "¿Esa comisión publicó un informe con los resultados?"
var_label(datos_tesis$comisiones_reportepublico_dummy) <- "Al menos una comisión publicó un informe"
var_label(datos_tesis$comisiones_recomendaciones) <- "¿Esa comisión hizo recomendaciones?"
var_label(datos_tesis$comisiones_recomendaciones_dummy) <- "Al menos una comisión hizo recomendaciones"
var_label(datos_tesis$comisiones_reco_enjuiciamiento) <- "¿Esa comisión recomendó el enjuiciamiento de violadores de DDHH?"
var_label(datos_tesis$comisiones_reco_enjuiciamiento_dummy) <- "Al menos una comisión recomendó el enjuiciamiento de violadores de DDHH"
var_label(datos_tesis$comisiones_reco_reforma) <- "¿Esa comisión recomendó reformas?"
var_label(datos_tesis$comisiones_reco_reforma_dummy) <- "Al menos una comisión recomendó reformas"
var_label(datos_tesis$conflicto_armado) <- "Conflicto armado interno"
var_label(datos_tesis$pib_log) <- "PIB per cápita (logaritmo natunal)"
var_label(datos_tesis$poblacion_log) <- "Población del país (logaritmo natural)"


tabla_variables <- look_for(datos_tesis)

tabla_variables <- tabla_variables %>%
  select(!levels)

openxlsx::write.xlsx(tabla_variables, file = "variables.xlsx")

write_rds(datos_tesis, file = "datos/datos_tesis.rds")
