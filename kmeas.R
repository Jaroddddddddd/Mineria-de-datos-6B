library(tidyverse)
library(ggpubr)
set.seed(101)
# Se simulan datos aleatorios con dos dimensiones
datos <- matrix(rnorm(n = 100*2), nrow = 100, ncol = 2,
                dimnames = list(NULL,c("x", "y")))
datos <- as.data.frame(datos)

# Se determina la media que va a tener cada grupo en cada una de las dos
# dimensiones. En total 2*4 medias. Este valor se utiliza para separar
# cada grupo de los demás.
media_grupos <- matrix(rnorm(n = 8, mean = 0, sd = 4), nrow = 4, ncol = 2,
                       dimnames = list(NULL, c("media_x", "media_y")))
media_grupos <- as.data.frame(media_grupos)
media_grupos <- media_grupos %>% mutate(grupo = c("a","b","c","d"))

# Se genera un vector que asigne aleatoriamente cada observación a uno de
# los 4 grupos
datos <- datos %>% mutate(grupo = sample(x = c("a","b","c","d"),
                                         size = 100,
                                         replace = TRUE))

# Se incrementa el valor de cada observación con la media correspondiente al
# grupo asignado.
datos <- left_join(datos, media_grupos, by = "grupo")
datos <- datos %>% mutate(x = x + media_x,
                          y = y + media_y)

ggplot(data = datos, aes(x = x, y = y, color = grupo)) +
  geom_point(size = 2.5) +
  theme_bw()


#Aplicación del Análisis Clúster
set.seed(101)
km_clusters <- kmeans(x = datos[, c("x", "y")], centers = 4, nstart = 50)
km_clusters


# Se representa el número de cluster al que se ha asignado cada observación y
# se muestra con un código de color el grupo real al que pertenece.

datos <- datos %>% mutate(cluster = km_clusters$cluster)
datos <- datos %>% mutate(cluster = as.factor(cluster),
                          grupo   = as.factor(grupo))

ggplot(data = datos, aes(x = x, y = y, color = grupo)) +
  geom_text(aes(label = cluster), size = 5) +
  theme_bw() +
  theme(legend.position = "none")

#Matriz de Confusión
table(km_clusters$cluster, datos[, "grupo"],
      dnn = list("cluster", "grupo real"))
