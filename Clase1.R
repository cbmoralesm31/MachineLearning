#cargo paquetes
library(tidyverse)
library(caret)
library(MuMIn)
#Cargo base de datos
data("mtcars")

#Divido mi base de datos en entrenamiento y testeo
set.seed(2019)
Index <- sample(1:nrow(mtcars), round(nrow(mtcars)/2))

Train <- mtcars[Index,]

Test <- mtcars[-Index,]


###Generar mis modelos
Modelo1 <- lm(mpg ~ hp, data = Train)
Modelo2 <- lm(mpg ~ hp + I(hp^2), data = Train)
Modelo3 <- lm(mpg ~ hp + I(hp^2) + wt, data = Train)
Modelo4 <- lm(mpg ~ hp + I(hp^2) + wt + I(wt^2), data = Train)


Modelos <- list(Modelo1, Modelo2, Modelo3, Modelo4)

###Creo base de datos solicitado

# AICc
# K
# R2 Exploratorio
# R2 Predictivo
# Id modelo

Template <- data.frame(AICc = NA, K = NA, R2_Exp = NA, R2_Predictivo = NA, ID_modelo = NA)

## Genero data con template


DFs <- list()

for(i in 1:length(Modelos)){
  Temp <- Template
  ## Lleno el dataframe
  
  Temp$AICc <- AICc(Modelos[[i]])
  Temp$K <-broom::glance(Modelos[[i]])$df
  Temp$R2_Exp <- postResample(pred = predict(Modelos[[i]], newdata = Train), obs = Train$mpg)[2]
  Temp$R2_Predictivo <- postResample(pred = predict(Modelos[[i]], newdata = Test), obs = Test$mpg)[2]
  Temp$ID_modelo <- i
  DFs[[i]] <- Temp
}

DF <- bind_rows(DFs) %>% arrange(AICc) %>% mutate(DeltaAICc = AICc - min(AICc))

write_csv(DF, "Resultados.csv")
