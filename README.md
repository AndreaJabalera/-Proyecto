# Paso 1: Definición de vectores
energia <- c(rep("Renovable", 10), rep("No Renovable", 10))  # Tipo de energía (Renovable y No Renovable)
consumo <- c(20, 22, NA, 19, 25, 21, NA, 23, 18, 20, 15, 17, 20, 22, 24, 18, NA, 21, 19, 22, 23)  # Consumo diario (en kWh)
costo_kwh <- c(rep(0.15, 10), rep(0.25, 10))  # Costo por kWh (diferente para renovable y no renovable)

# Paso 2: Limpieza de datos (Reemplazo de NA por la mediana de consumo por tipo de energía)
# Calcular la mediana de consumo por tipo de energía
mediana_renovable <- median(consumo[energia == "Renovable"], na.rm = TRUE)
mediana_no_renovable <- median(consumo[energia == "No Renovable"], na.rm = TRUE)

# Reemplazar los NA con la mediana correspondiente
consumo[is.na(consumo) & energia == "Renovable"] <- mediana_renovable
consumo[is.na(consumo) & energia == "No Renovable"] <- mediana_no_renovable

# Paso 3: Crear el dataframe df_consumo
df_consumo <- data.frame(
  energia = energia,
  consumo = consumo,
  costo_kwh = costo_kwh
)

# Paso 4: Cálculos
# Crear la columna costo_total (consumo * costo por kWh)
df_consumo$costo_total <- df_consumo$consumo * df_consumo$costo_kwh

# Calcular el total de consumo y el costo total por cada tipo de energía
total_consumo <- tapply(df_consumo$consumo, df_consumo$energia, sum)
total_costo <- tapply(df_consumo$costo_total, df_consumo$energia, sum)

# Calcular la media del consumo diario para cada tipo de energía
media_consumo <- tapply(df_consumo$consumo, df_consumo$energia, mean)

# Agregar una columna llamada ganancia (simular un aumento del 10% en el costo)
df_consumo$ganancia <- df_consumo$costo_total * 1.1

# Paso 5: Resumen
# Crear la lista resumen_energia
resumen_energia <- list()

# Ordenar el dataframe por costo_total de forma descendente
df_consumo_ordenado <- df_consumo[order(-df_consumo$costo_total),]

# Calcular el total de consumo y costo total por tipo de energía
resumen_energia$total_consumo <- total_consumo
resumen_energia$total_costo <- total_costo

# Extraer las tres filas con el mayor costo_total del dataframe ordenado
top_3_costos <- head(df_consumo_ordenado, 3)

# Almacenar en la lista resumen_energia
resumen_energia$df_ordenado <- df_consumo_ordenado
resumen_energia$top_3_costos <- top_3_costos

# Mostrar la lista resumen_energia
print(resumen_energia)
