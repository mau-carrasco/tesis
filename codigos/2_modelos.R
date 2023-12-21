#### Carga de paquetes ####

library(tidyverse)
library(tidymodels)
library(plm)
library(haven)
library(sandwich)
library(survival)
library(flextable)
library(modelsummary)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(ggvenn)


datos_tesis <- read_rds("datos/datos_tesis.rds")


#### Cap 3. Metodología ####

# Mapa muestra

world <- ne_countries(scale = "medium", returnclass = "sf")

world <- world %>% 
  select(iso_a3, geometry)

world$codigo_pais <- countrycode(world$iso_a3, "iso3c", "cown")

transiciones_pais <- datos_tesis %>% group_by(codigo_pais) %>%
  summarise(n = n_distinct(id_transicion))

png("resultados/figura_1.png", bg = 'transparent', width = 600, height = 400)

world %>%
  left_join(transiciones_pais) %>%
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_gradient("Total de casos", high = "red", low = "yellow") +
  labs(caption = "Nota: Los países en gris no registran transiciones democráticas en el periodo") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title.align=0.5) +
  guides(fill = guide_colorbar(title.position = "top",
                               title = "Número de transiciones democráticas por país",
                               barwidth = 15, 
                               barheight = 0.5)) +
  coord_sf(ylim = c(-65.65, 80), expand = FALSE)

dev.off()

# Tabla con estadísticos descriptivos de las variables

datasummary_skim(datos_tesis %>%
                   select("Protección de los DDHH" = proteccion_ddhh,
                          "Intentos de golpe de Estado" = intento_golpe,
                          "Golpe de Estado (Dummy)" = golpe),
                 title = "Estadísticos descriptivos de las variables dependientes",
                 output = "flextable") %>%
  theme_zebra() %>%
  autofit()

datasummary_skim(datos_tesis %>%
              select("Enjuiciamientos" = enjuiciamientos_acum,
                     "Sentencias" = sentencias_acum,
                     "Amnistías" = amnistias_acum,
                     "Comisiones" = comisiones_acum,
                     "Comisiones finalizadas" = comisiones_finalizadas_acum,
                     "Comisión con recomendación de reforma (Dummy)" = comisiones_reco_reforma,
                     "Comisión con recomendación de enjuiciamiento (Dummy)" = comisiones_reco_enjuiciamiento),
              title = "Estadísticos desriptivos de las variables predictoras",
              output = "flextable") %>%
  theme_zebra() %>%
  autofit()

datasummary_skim(datos_tesis %>%
                   select("Nivel de democracia (Polity II)" = polity2,
                          "PIB per cápita (log)" = pib_log,
                          "Población (log)" = poblacion_log,
                          "Conflicto armado (Dummy)" = conflicto_armado),
                 output = "flextable") %>%
  theme_zebra() %>%
  autofit()

#### Cap 4. Descriptivos ####

# diagrama de venn con el número de transiciones democráticas que implementaron juicios, amnistías y comisiones de verdad (1970-2011)

diagrama <- list(Juicios = datos_tesis$id_transicion[datos_tesis$enjuiciamientos_acum > 0], Amnistías = datos_tesis$id_transicion[datos_tesis$amnistias_dummy > 0], Comisiones = datos_tesis$id_transicion[datos_tesis$comisiones_acum > 0])

png("resultados/figura_2.png", bg = 'transparent', width = 600, height = 400)

ggvenn(
  diagrama, 
  show_percentage = FALSE,
  fill_color = c("blue", "yellow", "red")
)

dev.off()


#### Cap 5. Justicia transicional y democracia ####
library(pglm)

m1 <- pglm(intento_golpe ~ enjuiciamientos_acum_l1 + amnistias_acum_l1  + comisiones_acum_l1 + conflicto_armado + polity2 + pib_log + poblacion_log + 0,
           data = datos_tesis, index = c("id_transicion", "anos_transicion"), family = poisson,
           model = "within", effect = "individual")

m2 <- pglm(intento_golpe ~ enjuiciamientos_acum_l1 + amnistias_acum_l1  + comisiones_acum_l1 + conflicto_armado + polity2 + pib_log + poblacion_log + 0,
           data = datos_tesis, index = c("id_transicion", "anos_transicion"), family = negbin,
           model = "within", effect = "individual")


fitted.values(m2)

install.packages("pscl")

MASS::glm.nb(intento_golpe ~ enjuiciamientos_acum_l1 + amnistias_acum_l1  + comisiones_acum_l1 + conflicto_armado + polity2 + pib_log + poblacion_log + as.factor(id_transicion) + as.factor(anos_transicion), data = datos_tesis)

glm(intento_golpe ~ enjuiciamientos_acum_l1 + amnistias_acum_l1  + comisiones_acum_l1 + conflicto_armado + polity2 + pib_log + poblacion_log + as.factor(id_transicion) + as.factor(anos_transicion), data = datos_tesis)

pscl::zeroinfl(intento_golpe ~ enjuiciamientos_acum_l1 + amnistias_acum_l1  + comisiones_acum_l1 + conflicto_armado + polity2 + pib_log + poblacion_log | as.factor(id_transicion) + as.factor(anos_transicion), data = datos_tesis)

m2 <- pglm(intento_golpe ~ sentencias_acum_l1 + amnistias_acum_l1  + comisiones_reco_enjuiciamiento_l1 + conflicto_armado + polity2 + pib_log + poblacion_log,
           data = datos_tesis, index = c("id_transicion", "anos_transicion"), family = negbin,
           model = "within")

m3 <- pglm(intento_golpe ~ enjuiciamientos_acum_l1 + amnistias_acum_l1  + comisiones_acum_l1 + conflicto_armado + polity2 + pib_log + poblacion_log,
           data = datos_tesis %>% drop_na(), index = c("id_transicion", "anos_transicion"), family = poisson,
           model = "within")

modelsummary::modelsummary(list(m1, m2),
                           coef_rename = c("enjuiciamientos_acum_l1" = "Enjuiciamientos T-1",
                                           "amnistias_acum_l1" = "Amnistías T-1",
                                           "comisiones_acum_l1" = "Comisiones T-1",
                                           "conflicto_armado" = "Conflicto armado",
                                           "polity2" = "Polity II",
                                           "pib_log" = "PIB pc (log)",
                                           "poblacion_log" = "Población (log)",
                                           "sentencias_acum_l1" = "Sentencias T-1",
                                           "comisiones_reco_enjuiciamiento_l1" = "Comisiones con recomendación de enjuiciamiento"),
                           stars = c('*' = .1, '**' = .05, '***' = .01),
                           title = "Contribución de los mecanismos de justicia transicional a la ocurrencia golpes de estado en los procesos de transición democrática",
                           output = "flextable") %>%
  theme_zebra() %>%
  autofit()

datos_tesis$tipo_transicion

datos_tesis %>%
  group_by(id_transicion) %>%
  summarise(intentos_golpe = max(intento_golpe),
            golpe = max(golpe)) %>%
  ungroup() %>%
  group_by(intentos_golpe) %>%
  summarise(tranisciones = n(),
            golpe = sum(golpe))



#### Cap 6. Justicias transicional y protección de los derechos humanos ####

m3 <- plm(proteccion_ddhh ~ proteccion_ddhh_l1  + enjuiciamientos_acum_l1 + amnistias_acum_l1 + comisiones_acum_l1 + anos_transicion,
          data = datos_tesis,
          index = c("id_transicion"),
          model="within")

m4 <- plm(proteccion_ddhh ~ proteccion_ddhh_l1  + enjuiciamientos_acum_l1 + amnistias_acum_l1 + comisiones_acum_l1 + anos_transicion + tipo_transicion + polity2 + pib_log + poblacion_log + conflicto_armado  + intento_golpe,
          data = datos_tesis,
          index = c("id_transicion"),
          model="within")

m5 <- plm(proteccion_ddhh ~ proteccion_ddhh_l1 + sentencias_acum_l1 + amnistias_acum_l1 + comisiones_acum_l1 + anos_transicion + tipo_transicion + polity2 + pib_log + poblacion_log + conflicto_armado + intento_golpe,
          data = datos_tesis,
          index = c("id_transicion"),
          model="within")

m6 <- plm(proteccion_ddhh ~ proteccion_ddhh_l1 + enjuiciamientos_acum_l1 * amnistias_acum_l1 + comisiones_acum_l1 + anos_transicion + tipo_transicion + polity2 + pib_log + poblacion_log + conflicto_armado + intento_golpe,
          data = datos_tesis,
          index = c("id_transicion"),
          model="within")

modelsummary(list(m3, m4, m5, m6),
             coef_rename = c("enjuiciamientos_acum_l1" = "Enjuiciamientos T-1",
                             "proteccion_ddhh_l1" = "Protección de DDHH T-1",
                             "amnistias_acum_l1" = "Amnistías T-1",
                             "comisiones_acum_l1" = "Comisiones T-1",
                             "conflicto_armado" = "Conflicto armado",
                             "polity2" = "Polity II",
                             "pib_log" = "PIB pc (log)",
                             "poblacion_log" = "Población (log)",
                             "sentencias_acum_l1" = "Sentencias T-1",
                             "intento_golpe" = "intentos de golpe de estado",
                             "comisiones_finalizadas_acum_l1" = "Comisiones finalizadas",
                             "comisiones_reco_enjuiciamiento_l1" = "Comisiones con recomendación de enjuiciamiento",
                             "enjuiciamientos_acum_l1:amnistias_acum_l1" = "Enjuiciamientos x Amnistías"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             vcov = function(x){vcovBK(x, type = "HC1", cluster = "time")},
             title = "Contribución de los mecanismos de justicia transicional a la protección de los DDHH en las transición democrática",
             output = "flextable") %>%
  autofit() %>%
  theme_zebra()
