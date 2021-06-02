# Carga el paquete espec�fico del m�todo Random Forest
library(randomForest)

# Carga de datos inicial, tipos de flores con diferentes caracter�sticas 
data(iris)
datos <- iris
View(datos)

# Selecci�n de una submuestra del 70% de los datos
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
# Matriz de confusi�n
(mc <- with(datos.test,table(predicciones, Species)))

# % correcto
100 * sum(diag(mc)) / sum(mc)