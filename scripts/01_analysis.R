library(tidyverse)
library(readxl)

sampling1 <- read_excel("data/rawdata.xlsx", sheet = "sampling1_data")

# Eliminar las columnas con NAs

sampling1 <- sampling1 |>
 drop_na() 

# Crear una variable Minute a partir de Timepoint debe ser numérica, se puede obtener quitando la T de la variable Timepoint
# Crear una variable Trt a partir de Trt_num, añadiendo una T a la variable
# Estadísticas por tratamiento