#required to read csv files
library(data.table)
#required to plot graphs
library(ggplot2)
#required to manipulate dataframes
library(dplyr)

# read csv file and load to a table data
file_path <- "./enoe_n_2022_trim4_csv (2)/ENOEN_SDEMT422.csv"
data <- fread(file_path)


# print data type of data var
print(typeof(data))

# Get records quantity
total_records <- nrow(data)

# Identify columns with at least one NA value
columns_with_na <- colnames(data)[ apply(data, 2, anyNA) ]

# Print the column names
print(columns_with_na)



# Contar el número de registros con cero usando sum() e ifelse()
numero_registros_cero_sum <- sum(ifelse(datos$cs_p12 == 0, 1, 0))

# Contar valores NA
cantidad_NA <- sum(is.na(datos$cs_p12))

# Obtener la moda de "variable1" usando table()
moda_tabla <- table(datos$cs_p12)

# Identificar el valor (o valores) con mayor frecuencia
moda_valor <- names(which.max(moda_tabla))

moda_frecuencia <- max(moda_tabla)

print(moda_valor)

print(moda_frecuencia)

# Obtener la moda, como este campo tiene na, el resultado lo genera como texto
datos$cs_p12 <- as.integer(datos$cs_p12)

moda_valores <- mode(datos$cs_p12)

print(moda_valores)

print(typeof(datos$cs_p12))

# Obtener la moda de "mi_variable" excluyendo NA
moda_valores <- mode(datos$cs_p12, na.rm = TRUE)

?mode

print(moda_valores)

# Filter data excluding NA
data_filtered <- datos %>% filter(!is.na(cs_p12))

moda_valores <- mode(data_filtered$cs_p12)

print(moda_valores)

# Crear un histograma de "variable1"
hist(datos$cs_p12, main = "Histograma de variable1")

# Agregar anotación para indicar la moda
abline(v = moda_valor, col = "red", linetype = "dashed", 
       label = paste0("Moda:", moda_valor))


ggplot(data_filtered, aes(x = cs_p12)) + 
  geom_freqpoly() + 
  labs(title = "Distribución de frecuencia de variable1")

# Calcular la suma de cada fila
suma_filas <- rowSums(datos)

# Eliminar la columna
datos <- select(datos, -t_loc_men)

distribucion_frecuencias <- table(datos$est_d_men)
print(distribucion_frecuencias)

