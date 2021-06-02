# Carga el paquete específico del método Random Forest
library(randomForest)

# Carga de datos inicial, tipos de flores con diferentes características 
data(iris)
datos <- iris
View(datos)

# Selección de una submuestra del 70% de los datos
tamano.total <- nrow(datos)
tamano.entreno <- round(tamano.total*0.7)
datos.indices <- sample(1:tamano.total , size=tamano.entreno)
datos.entreno <- datos[datos.indices,]
datos.test <- datos[-datos.indices,]

# Ajustar modelo
modelo <- randomForest(Species~., data=datos.entreno)

# Resumen del ajuste del modelo
modelo

# Hacer predicciones
predicciones <- predict(modelo, datos.test)
# Matriz de confusión
(mc <- with(datos.test,table(predicciones, Species)))

# % correcto
100 * sum(diag(mc)) / sum(mc)
