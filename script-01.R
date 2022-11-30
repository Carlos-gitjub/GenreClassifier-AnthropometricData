library(dplyr)
library(ggplot2)
library(class)

# Modelo de clasificación de k-vecinos para predecir el género en base a datos 
# antropométricos. Usaremos cuatro: "Ankle_min_girth","Wrist_min_girth",
# "Chest_depth","Biacromial_diameter".
# Arreglo de los datos
body <- read.table("http://ww2.amstat.org/publications/jse/datasets/body.dat.txt")
BodyMeasurements <- c("Biacromial_diameter","Biiliac_diameter","Bitrochanteric_diameter","Chest_depth","Chest_diameter","Elbow_diameter","Wrist_diameter","Knee_diameter","Ankle_diameter","Shoulder_girth","Chest_girth","Waist_girth","Navel_girth","Hip_girth","Thigh_girth","Bicep_girth","Forearm_girth","Knee_girth","Calf_max_girth","Ankle_min_girth","Wrist_min_girth","Age","Weight","Height","Gender")    
names(body) <- BodyMeasurements
body$Gender = factor(body$Gender)
save(body, file = "body.Rdata")


# Predicción k-vecinos con k=10. División en conjunto de test y entrenamiento.
vars=c("Ankle_min_girth","Wrist_min_girth","Chest_depth","Biacromial_diameter")
x=body[,vars]
n=nrow(body)
train=sample(1:n, round(n/2))
test=-train
g=body$Gender[train]
k=10
mod.train<-class::knn(x[train,], x[train,], k=k, cl=g)
data<-body[train,]
data$class=mod.train
# Gráfica de los resultados
ggplot(data) + geom_point(aes(Ankle_min_girth, Wrist_min_girth, shape=Gender, color=class), size=3) +
  facet_grid(ntile(Chest_depth,4) ~ ntile(Biacromial_diameter,4))
# NOTA: La gráfica se refiere a dos variables: Ankle_min_girth y Wrist_min_girth.
# Las otras dos variables, han sido trozeadas y representadas en facets en 
# el gráfico.


# Cálculo de error de Clasificación para conjunto de test y entramiento.
k=10
g=body$Gender[train]
cltest=body$Gender[test]
mod.train <- class::knn(x[train,],x[train,],k=k, cl=g)
mod.test <- class::knn(x[train,],x[test,], k=k, cl=g)
(err.train <- 1 - sum(mod.train==factor(g))/length(g) )
(err.test <- 1 - sum(mod.test==cltest)/length(cltest) )


# Predicción de modelo k-vecinos para distintas k.
g<-body$Gender[train]
cltest<-body$Gender[test]
errors<-NULL
for (k in c(1,3,5,10,20,30,40,50,75,100)) {
  mod.train<-class::knn(x[train,], x[train,], k=k, cl=g)
  mod.test<-class::knn(x[train,], x[test,], k=k, cl=g)
  err.train <- 1 - sum(mod.train==factor(g))/length(g)
  err.test <- 1 - sum(mod.test==cltest)/length(cltest) 
  errors<-rbind(errors, data.frame(tipo="train", k=k, error=err.train))
  errors<-rbind(errors, data.frame(tipo="test", k=k, error=err.test))
}
# Grafica de los errores
ggplot(errors) + geom_point(aes(k,error, color=tipo)) +
  geom_line(aes(k,error, color=tipo), linetype=2, size=0.5) +
  scale_x_continuous(breaks = c(1,3,5,10,20,30,40,50,75,100))
# NOTA: los mejores  resultados en el conjunto de test suelen obtenerse para
# k=10 y k=20