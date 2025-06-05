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
# Hacer g1 y g1_2 para todas las variables.

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
ggsave("graficos/sampling1/CO2.png", g1, width = 5, height = 5)

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
ggsave("graficos/sampling1/CO2R.png", g1_2, width = 5, height = 5)


g2 <- ggplot(sampling1, aes(x = Minute, y = CH4_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "CH4_ppm") +
  theme_apa() +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme(
    legend.position = "bottom"
  )
g2
ggsave("graficos/sampling1/CH4.png", g2, width = 5, height = 5)

g2_2 <- ggplot(sampling1, aes(x = Minute, y = CH4_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Residue_Trt) +
  labs(x = "Minute", y= "CH4_ppm") +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g2_2
ggsave("graficos/sampling1/CH4R.png", g2_2, width = 5, height = 5)

g3 <- ggplot(sampling1, aes(x = Minute, y = N2O_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "N2O_ppm") +
  theme_apa() +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme(
    legend.position = "bottom"
  )
g3
ggsave("graficos/sampling1/N2O.png", g3, width = 5, height = 5)

g3_2 <- ggplot(sampling1, aes(x = Minute, y = N2O_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Residue_Trt) +
  labs(x = "Minute", y= "N2O_ppm") +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g3_2
ggsave("graficos/sampling1/N2OR.png", g3_2, width = 5, height = 5)

# Para todos los muestreos y por separado cada muestreo

#Sampling 2

sampling2 <- read_excel("data/rawdata.xlsx", sheet = "sampling2_data")

sampling2 <- sampling2 |>
  drop_na()

sampling2 <- sampling2 |>
  mutate(
    Minute = as.numeric(str_sub(Timepoint, 2, -1)),
    Timepoint = str_sub(Timepoint, 2, -1)
  )

sampling2 <- sampling2 |>
  mutate(Trt = paste0("T", Trt_num))

g1samp2 <- ggplot(sampling2, aes(x = Minute, y = CO2_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "C02_ppm") +
  theme_apa() +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme(
    legend.position = "bottom"
  )
g1samp2
ggsave("graficos/sampling2/CO2.png", g1samp2, width = 5, height = 5)

g11samp2 <- ggplot(sampling2, aes(x = Minute, y = CO2_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Residue_Trt) +
  labs(x = "Minute", y= "C02_ppm") +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g11samp2
ggsave("graficos/sampling2/CO2R.png", g11samp2, width = 5, height = 5)


g2samp2 <- ggplot(sampling2, aes(x = Minute, y = CH4_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "CH4_ppm") +
  theme_apa() +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme(
    legend.position = "bottom"
  )
g2samp2
ggsave("graficos/sampling2/CH4.png", g2samp2, width = 5, height = 5)

g21samp2 <- ggplot(sampling2, aes(x = Minute, y = CH4_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Residue_Trt) +
  labs(x = "Minute", y= "CH4_ppm") +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g21samp2
ggsave("graficos/sampling2/CH4R.png", g21samp2, width = 5, height = 5)

g3samp2 <- ggplot(sampling2, aes(x = Minute, y = N2O_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "N2O_ppm") +
  theme_apa() +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme(
    legend.position = "bottom"
  )
g3samp2
ggsave("graficos/sampling2/N2O.png", g3samp2, width = 5, height = 5)

g31samp2 <- ggplot(sampling2, aes(x = Minute, y = N2O_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Residue_Trt) +
  labs(x = "Minute", y= "N2O_ppm") +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g31samp2
ggsave("graficos/sampling2/N2OR.png", g31samp2, width = 5, height = 5)

#Sampling 3
sampling3 <- read_excel("data/rawdata.xlsx", sheet = "sampling3_data")

sampling3 <- sampling3 |>
  drop_na()

sampling3 <- sampling3 |>
  mutate(
    Minute = as.numeric(str_sub(Timepoint, 2, -1)),
    Timepoint = str_sub(Timepoint, 2, -1)
  )

sampling3 <- sampling3 |>
  mutate(Trt = paste0("T", Trt_num))

g1s3 <- ggplot(sampling3, aes(x = Minute, y = CO2_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "C02_ppm") +
  theme_apa() +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme(
    legend.position = "bottom"
  )
g1s3
ggsave("graficos/sampling3/CO2.png", g1s3, width = 5, height = 5)

g1_1s3 <- ggplot(sampling3, aes(x = Minute, y = CO2_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Residue_Trt) +
  labs(x = "Minute", y= "C02_ppm") +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g1_1s3
ggsave("graficos/sampling3/CO2R.png", g1_1s3, width = 5, height = 5)


g2s3 <- ggplot(sampling3, aes(x = Minute, y = CH4_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "CH4_ppm") +
  theme_apa() +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme(
    legend.position = "bottom"
  )
g2s3
ggsave("graficos/sampling3/CH4.png", g2s3, width = 5, height = 5)

g2_1s3 <- ggplot(sampling3, aes(x = Minute, y = CH4_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Residue_Trt) +
  labs(x = "Minute", y= "CH4_ppm") +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g2_1s3
ggsave("graficos/sampling3/CH4R.png", g2_1s3, width = 5, height = 5)

g3s3 <- ggplot(sampling3, aes(x = Minute, y = N2O_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "N2O_ppm") +
  theme_apa() +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme(
    legend.position = "bottom"
  )
g3s3
ggsave("graficos/sampling3/N2O.png", g3s3, width = 5, height = 5)

g3_1s3 <- ggplot(sampling3, aes(x = Minute, y = N2O_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Residue_Trt) +
  labs(x = "Minute", y= "N2O_ppm") +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g3_1s3
ggsave("graficos/sampling3/N2OR.png", g3_1s3, width = 5, height = 5)

#Sampling 4

sampling4 <- read_excel("data/rawdata.xlsx", sheet = "sampling4_data")

sampling4 <- sampling4 |>
  drop_na()

sampling4 <- sampling4 |>
  mutate(
    Minute = as.numeric(str_sub(Timepoint, 2, -1)),
    Timepoint = str_sub(Timepoint, 2, -1)
  )

sampling4 <- sampling4 |>
  mutate(Trt = paste0("T", Trt_num))

g1s4 <- ggplot(sampling4, aes(x = Minute, y = CO2_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "C02_ppm") +
  theme_apa() +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme(
    legend.position = "bottom"
  )
g1s4
ggsave("graficos/sampling4/CO2.png", g1s4, width = 5, height = 5)

g1_1s4 <- ggplot(sampling4, aes(x = Minute, y = CO2_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Residue_Trt) +
  labs(x = "Minute", y= "C02_ppm") +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g1_1s4
ggsave("graficos/sampling4/CO2R.png", g1_1s4, width = 5, height = 5)


g2s4 <- ggplot(sampling4, aes(x = Minute, y = CH4_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "CH4_ppm") +
  theme_apa() +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme(
    legend.position = "bottom"
  )
g2s4
ggsave("graficos/sampling4/CH4.png", g2s3, width = 5, height = 5)

g2_1s4 <- ggplot(sampling4, aes(x = Minute, y = CH4_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Residue_Trt) +
  labs(x = "Minute", y= "CH4_ppm") +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g2_1s4
ggsave("graficos/sampling4/CH4R.png", g2_1s4, width = 5, height = 5)

g3s4 <- ggplot(sampling4, aes(x = Minute, y = N2O_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "N2O_ppm") +
  theme_apa() +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme(
    legend.position = "bottom"
  )
g3s4
ggsave("graficos/sampling4/N2O.png", g3s4, width = 5, height = 5)

g3_1s4 <- ggplot(sampling4, aes(x = Minute, y = N2O_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Residue_Trt) +
  labs(x = "Minute", y= "N2O_ppm") +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g3_1s4
ggsave("graficos/sampling4/N2OR.png", g3_1s4, width = 5, height = 5)

#Sampling 5

sampling5 <- read_excel("data/rawdata.xlsx", sheet = "sampling5_data")

sampling5 <- sampling5 |>
  drop_na()

sampling5 <- sampling5 |>
  mutate(
    Minute = as.numeric(str_sub(Timepoint, 2, -1)),
    Timepoint = str_sub(Timepoint, 2, -1)
  )

sampling5 <- sampling5 |>
  mutate(Trt = paste0("T", Trt_num))

g1s5 <- ggplot(sampling5, aes(x = Minute, y = CO2_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "C02_ppm") +
  theme_apa() +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme(
    legend.position = "bottom"
  )
g1s5
ggsave("graficos/sampling5/CO2.png", g1s5, width = 5, height = 5)

g1_1s5 <- ggplot(sampling5, aes(x = Minute, y = CO2_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Residue_Trt) +
  labs(x = "Minute", y= "C02_ppm") +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g1_1s5
ggsave("graficos/sampling5/CO2R.png", g1_1s5, width = 5, height = 5)


g2s5 <- ggplot(sampling5, aes(x = Minute, y = CH4_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "CH4_ppm") +
  theme_apa() +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme(
    legend.position = "bottom"
  )
g2s5
ggsave("graficos/sampling5/CH4.png", g2s5, width = 5, height = 5)

g2_1s5 <- ggplot(sampling5, aes(x = Minute, y = CH4_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Residue_Trt) +
  labs(x = "Minute", y= "CH4_ppm") +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g2_1s5
ggsave("graficos/sampling5/CH4R.png", g2_1s5, width = 5, height = 5)

g3s5 <- ggplot(sampling5, aes(x = Minute, y = N2O_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  labs(x = "Minute", y= "N2O_ppm") +
  theme_apa() +
  scale_x_continuous(breaks = c(0,30,60)) +
  theme(
    legend.position = "bottom"
  )
g3s5
ggsave("graficos/sampling5/N2O.png", g3s5, width = 5, height = 5)

g3_1s5 <- ggplot(sampling5, aes(x = Minute, y = N2O_ppm )) +
  geom_point(stat = "summary", fun = "mean", aes(col = Trt)) +
  geom_line(stat = "summary", fun = "mean", aes(col = Trt)) +
  facet_wrap(.~ Residue_Trt) +
  labs(x = "Minute", y= "N2O_ppm") +
  theme_apa() +
  theme(
    legend.position = "bottom"
  )
g3_1s5
ggsave("graficos/sampling5/N2OR.png", g3_1s5, width = 5, height = 5)
