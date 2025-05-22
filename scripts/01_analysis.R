library(tidyverse)
library(readxl)

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

unique(sampling1[,5:7])

# Estadísticas por Trt_num, Planter_Trt y Residue_Trt

# Boxplots 

# Line plots (gráficos de línea o evolución)

