# Instalar y cargar las librerías necesarias
install.packages("tidyverse")
install.packages("scales")
install.packages("caTools")  # Para dividir el conjunto de datos
library(tidyverse)
library(scales)
library(caTools)

###### 1. Adquisición de Datos ###### 
# Cargar el archivo de datos
datos <- read_csv("F:/excel/datos_bienesraices.csv")

###### 2. Preprocesamiento de Datos ###### 
# Exploración y limpieza de datos
datos <- datos %>% 
  distinct() %>%  # Eliminar duplicados
  drop_na()       # Eliminar filas con valores faltantes

# Visualizar la estructura y algunas filas después de la limpieza
glimpse(datos)
head(datos)

###### 3. Análisis de Datos Exploratorio (EDA) ###########
# 1. Distribución de los precios de venta
ggplot(datos, aes(x = Precio_Venta)) +
  geom_histogram(bins = 30, color = "black", fill = "lightblue") +
  labs(title = "Distribución de los Precios de Venta", x = "Precio de Venta", y = "Frecuencia")

# 2. Relación entre el tamaño y el precio de venta
ggplot(datos, aes(x = Tamaño_m2, y = Precio_Venta)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Relación entre Tamaño y Precio de Venta", x = "Tamaño (m2)", y = "Precio de Venta")

# 3. Boxplot del precio de venta por estado de la propiedad con más números en el eje Y
ggplot(datos, aes(x = Estado, y = Precio_Venta)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Precio de Venta por Estado de la Propiedad", x = "Estado", y = "Precio de Venta") +
  scale_y_continuous(labels = scales::comma, breaks = seq(100000, 500000, by = 50000))

###### 4. Ingeniería de Características ########
# 1. Crear una nueva característica basada en el tamaño: "Categoría_Tamaño"
datos <- datos %>%
  mutate(Categoría_Tamaño = case_when(
    Tamaño_m2 < 100 ~ "Pequeño",
    Tamaño_m2 >= 100 & Tamaño_m2 < 200 ~ "Mediano",
    Tamaño_m2 >= 200 ~ "Grande"
  ))

# 2. Codificación de características categóricas
datos <- datos %>%
  mutate(
    Estado_Codificado = as.numeric(factor(Estado)),  # Convertir "Estado" en valores numéricos
    Ubicación_Codificada = as.numeric(factor(Ubicación))  # Convertir "Ubicación" en valores numéricos
  )

# Visualizar la estructura después de la ingeniería de características
glimpse(datos)
head(datos)

###### 5. División de Datos ##########
set.seed(123)  # Fijar semilla para reproducibilidad
split <- sample.split(datos$Precio_Venta, SplitRatio = 0.7)  # 70% para entrenamiento y 30% para prueba
datos_entrenamiento <- subset(datos, split == TRUE)
datos_prueba <- subset(datos, split == FALSE)

# Imprimir la cantidad de datos en cada conjunto
cat("Datos de entrenamiento: ", nrow(datos_entrenamiento), "\n")
cat("Datos de prueba: ", nrow(datos_prueba), "\n")

# Guardar el conjunto de datos limpio
write_csv(datos, "datos_bienesraices_limpios.csv")

