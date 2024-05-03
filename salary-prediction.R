#required to read csv files
library(data.table)
#required to plot graphs
library(ggplot2)
#required to manipulate dataframes
library(dplyr)
#for data manipulation
library(tidyr)

# read csv file and load to a table data
file_path <- "./enoe_n_2022_trim4_csv (2)/ENOEN_SDEMT422.csv"
data <- fread(file_path)


# print data type of data var
print(typeof(data))

# Get records quantity
total_records <- nrow(data)

# Identify columns with at least one NA value
columns_with_na <- colnames(data)[ apply(data, 2, anyNA)]

print(columns_with_na[2])
# Sort and print column names with na values
sorted_na_columns <- sort(columns_with_na)

print(typeof(columns_with_na))

# Loop through unique values in the grouping column
for (col in sorted_na_columns) {
  cat("Column:", col, "\n")
}

print(sorted_na_columns)

# Write the variable with columns with at least one missing value to a CSV file
write.csv(sorted_na_columns, "columns_with_missing_values.csv", row.names = FALSE)

# Calculate mode for categorical data
c_Mode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

c_Median <- function(x) {
  median(x, na.rm = TRUE)
}

# Calculate median (both continuous and categorical data)
median_value <- c_Median(data$cs_ad_des, na.rm = TRUE)
mode_value = c_Mode(data$cs_ad_des)

for (col in sorted_na_columns) {
  cat("Column:", col, "\n")
  cat("Median: ", c_Median(data[[col]]), "\n")
  cat("Mode: ", c_Mode(data[[col]]), "\n")
  cat("\n")
}

?median

# Print the results
cat("Median:", median_value)
print(mode_value)

#count by a given value
column_value <- sum(data$cs_ad_des == 9, na.rm = TRUE)

print(column_value)

# dplyr: Group by values in 'my_column', and count occurrences
grouped_counts <- data %>%
  group_by(cs_ad_des) %>%  # Group by values in 'my_column'
  count()  # Count occurrences of each group

# Print the results
print(grouped_counts)

# Function to group data by column values and count occurrences, including NA
group_and_count_na <- function(df, column_name) {
  df %>%
    #filter(!is.na(!!column_name)) %>%  # Filter out rows with NA using !! for non-standard evaluation
    #group_by(!!column_name) %>%  # Group by values in 'column_name' using !! for non-standard evaluation
    #filter(!is.na({{column_name}})) %>%
    group_by(df[[column_name]]) %>%
    count()
}

grouped_counts <- group_and_count_na(data, "cs_ad_des")
print(grouped_counts)

for (col in sorted_na_columns) {
  cat("Column:", col, "\n")
  print(group_and_count_na(data, col))
  cat("\n")
}

#Calculate percentages for grouped data
percent_by_group <- function(df, col, total_count = nrow(df)) {
  df %>%
    group_by(df[,..col]) %>%  # Group by values in 'col' (# Using column selection)
    summarise(
      n = n(),  # Count observations (including missing values)
      pct = n / total_count * 100  # Calculate percentage relative to total count
    )
}

grouped_counts_percentage <- percent_by_group(data, col="cs_ad_des")

print(grouped_counts_percentage)

for (col in sorted_na_columns) {
  cat("Column:", col, "\n")
  print(percent_by_group(data, col))
  cat("\n")
}

#next step get the percentage of missing values for sorted_na_columns


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

