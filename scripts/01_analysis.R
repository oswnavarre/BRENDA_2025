library(tidyverse)
library(readxl)
library(jtools)

sampling1 <- read_excel("data/rawdata.xlsx", sheet = "sampling1_data")

# Eliminar las columnas con NAs

sampling1 <- sampling1 |>
 drop_na() 

# Crear una variable Minute a partir de Timepoint debe ser numérica, se puede obtener quitando la T de la variable Timepoint
sampling1 <- sampling1 |>
  mutate(
    Minute = as.numeric(str_sub(Timepoint, 2, -1)),
    Timepoint = str_sub(Timepoint, 2, -1)
  )

# Crear una variable Trt a partir de Trt_num, añadiendo una T a la variable

sampling1 <- sampling1 |>
  mutate(Trt = paste0("T", Trt_num))

# Acumulados

sampling1 <- sampling1 |>
  group_by(Planter_Trt, Residue_Trt, Trt, Rep) |>
  mutate(
    cum_co2 = cumsum(CO2_ppm),
    cum_ch4 = cumsum(CH4_ppm),
    cum_n2o = cumsum(N2O_ppm),
  )

# Estadísticas por tratamiento


# Gráficas

g1 <- ggplot(sampling1, aes(x = Minute, y = CO2_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "C02_ppm") +
  theme_apa() +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme(
    legend.position = "bottom"
  )
g1

g1_1 <- ggplot(sampling1, aes(x = Minute, y = CO2_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Planter_Trt) +
  labs(x = "Minute", y= "C02_ppm") +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g1_1

g1_2 <- ggplot(sampling1, aes(x = Minute, y = CO2_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Residue_Trt) +
  labs(x = "Minute", y= "C02_ppm") +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g1_2

g1_3 <- ggplot(sampling1, aes(x = Minute, y = CO2_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Trt) +
  labs(x = "Minute", y= "CO2_ppm") +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g1_3


g2 <- ggplot(sampling1, aes(x = Minute, y = cum_co2 )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "Cummulative C02_ppm") +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g2

g2_1 <- ggplot(sampling1, aes(x = Minute, y = cum_co2 )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "Cummulative C02_ppm") +
  facet_wrap(.~ Planter_Trt) +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g2_1

g2_2 <- ggplot(sampling1, aes(x = Minute, y = cum_co2 )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "Cummulative C02_ppm") +
  facet_wrap(.~ Residue_Trt) +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g2_2

g2_3 <- ggplot(sampling1, aes(x = Minute, y = cum_co2 )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "Cummulative C02_ppm") +
  facet_wrap(.~ Trt) +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g2_3



# Análisis de la segunda variable. May 23rd viernes. 

b1 <- ggplot(sampling1, aes(x = Minute, y = cum_n2o, fill = Trt)) +
  geom_boxplot() +
  labs(x = "Minute", y = "Cummulative N20_ppm") +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
b1

b2 <- ggplot(sampling1, aes(x = Minute, y = cum_n2o)) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "Cummulative N20_ppm") +
  facet_wrap(.~ Planter_Trt) +
  scale_x_continuous(breaks = c(0,30,60)) +
  scale_y_continuous(breaks = c(0,0.1,0.2)) +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
b2

b2_1 <- ggplot(sampling1, aes(x = Minute, y = cum_n2o)) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "Cummulative N20_ppm") +
  facet_wrap(.~ Planter_Trt) +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
b2_1

# Hacer g1 y g1_2 para todas las variables.
# Para todos los muestreos y por separado cada muestreo

