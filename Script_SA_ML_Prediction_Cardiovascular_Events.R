#### Eventos cardiovasculares - Comparacion de modelos por matriz de costos ####

#Definir el espacio de trabajo
setwd("C:/Users/sjara11/Desktop/Santiago Jaramillo/Universidad EIA/Proyecto de Especialización")

#importar los datos
data <- read.csv("C:\\Users\\sjara11\\Desktop\\Santiago Jaramillo\\Universidad EIA\\Proyecto de Especialización\\Procesado_final.csv", stringsAsFactors = T)
data <- data[,c(-4,-22,-21)]
str(data)
head(data)
summary(data)

#renombrar la variable objetivo por facilidad de uso 
names(data)[ncol(data)] = 'objetivo'
names(data)
data$objetivo <- as.factor(data$objetivo)
# Visualizar la variable de interés
table(data$objetivo)


#install.packages("ggplot2")
#### Estadistica descriptiva ####

#Analisis de correlaciones
psych::describe(data)
library(psych)
round(cor(data[2:20]),2)

#devtools::install_github("laresbernardo/lares")
library(lares)
corr_cross(data[2:20], # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 15 couples of variables (by correlation coefficient)
)

#histogramas de variables continuas
for (i in 2:20){
  hist(data[,i], main = names(data)[i] )
}

d <- density(data$IMC)
plot(d)
polygon(d, col="red", border="blue")

library(sm)
# plot densities
sm.density.compare(data$IMC, data$objetivo,
                   xlab="IMC vs Objetivo")
colfill<-c(2:(2+length(levels(as.factor
                              ((data$objetivo))))))
legend("topright",locator(1), levels(as.factor
                                     ((data$objetivo))), fill=colfill) 

#Bloxplot de las variables cuantitativas vs la objetivo
for (i in 2:20){
  aggregate(data[c(i)], by=list(data$objetivo), FUN = mean)
  aggregate(data[c(i)], by=list(data$objetivo), FUN = median)
  boxplot(data[,i]~objetivo, data=data, main=paste("objetivo vs ",names(data)[i])
          ,xlab="Resultado objetivo", ylab=names(data)[i])
}

#Analisis variables cualitativas vs objetivos

counts1 <- prop.table(table(data$objetivo,data$Sexo),2)
barplot(counts1, main="Probabilidades condicionales",
        xlab="Sexo",legend.text = c(levels(data$objetivo))) # 1 = Femenino

counts2 <- prop.table(table(data$objetivo,data$Hipertension),2)
barplot(counts2, main="Probabilidades condicionales",
        xlab="Hipertensión",legend.text = c(levels(data$objetivo)))

counts3 <- prop.table(table(data$objetivo,data$Diabetes),2)
barplot(counts3, main="Probabilidades condicionales",
        xlab="Diabetes",legend.text = c(levels(data$objetivo)))


#### Preparacion de los datos ####
#Cambiar sexo por dummy
data$Sexo <- as.character(data$Sexo)
data$Sexo[data$Sexo == "F"] <- 1
data$Sexo[data$Sexo == "M"] <- 0
data$Sexo <- as.numeric(data$Sexo)

#Analisis de componentes principales
library(factoextra)
pca <- princomp(data[c(-ncol(data))], cor = TRUE, scores = TRUE)
pca <- prcomp(data[c(-ncol(data))], scale = TRUE)
summary(pca)

pca$rotation
dim(pca$x)
pca$sdev^2
prop_varianza <- pca$sdev^2 / sum(pca$sdev^2)
prop_varianza_acum <- cumsum(prop_varianza)

ggplot(data = data.frame(prop_varianza_acum, pc = 1:22),
       aes(x = pc, y = prop_varianza_acum, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")

fviz_eig(pca)

#Funcion para normalizar las variables
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#Dataset preparado para los modelos
data_norm <- apply(data[,c(-1,-21,-22,-23)],2,normalize)
data_norm <- as.data.frame(data_norm)
data <- cbind(data_norm, data[,c(1,21,22,23)])

# Crear bases de entrenamiento y testeo
set.seed(12345)
train_ind <- sample(1:nrow(data), 
                    size = floor(0.8 * nrow(data)))

data_train <- data[train_ind, ]
data_test <- data[-train_ind, ]

#Exportan el data test para sacar el costo actual del hospital
#write.csv(data_test,'data_test.csv')

#Matriz para almacentar los resultados
resultados_modelos <- matrix(nrow = 8, ncol = 3)

#Funcion para calcular el mejor cutpointcon base a la matriz de costos
f_mejorcutpoint <- function(x,y){
  x_df <- as.data.frame(x)
  paso <- seq(from = 0.05, to = 1, by = 0.05)
  matriz_costos <- rbind(c(0,19020000),c(3350000,12525000))
  vector_comparacion <- vector(mode = "list", length = length(paso))
  vector_cutpoints <- vector(mode = "list", length = length(paso))
  z <- 1
  dim(matriz_costos)[2]
  
  for (i in paso){
    x_df$clasif <- 0
    x_df$clasif[x_df[,2]>i] <- 1
    x_df$clasif <- as.factor(x_df$clasif)
    matriz_pred = table(x_df$clasif, y[,ncol(y)])
    if (dim(matriz_pred)[1]==2){
      resultado = sum(matriz_costos*matriz_pred)
      vector_comparacion[z] <- resultado
    } else{
      vector_comparacion[z] <- NULL
    }
    vector_cutpoints[z] <- i
    z = z + 1
  }
  
  costo_min <- min( unlist(vector_comparacion))
  pos_min <-  match(costo_min,vector_comparacion)
  cutpoint <- vector_cutpoints[pos_min]
  
  lista_res <- list("costo" = costo_min, "cutpoint"= cutpoint[[1]])
  
  return(lista_res)
}

#### Random Forests #### 
library(randomForest)
set.seed(1234)
rf.mod <- randomForest(objetivo ~ ., data = data_train, importance=TRUE)
rf.mod
varImpPlot(rf.mod)

data_pred <- predict(rf.mod, data_test, type = "prob")


resultados_modelos[1,][1] <- "Random Forest" 
resultados_modelos[1,][2] <- unlist(f_mejorcutpoint(data_pred,data_test))[1]
resultados_modelos[1,][3] <- unlist(f_mejorcutpoint(data_pred,data_test))[2]

#### SVM ####
library(e1071)

mod.svm <- svm(objetivo ~ ., data= data_train, probability = TRUE)
data_pred_svm <- predict(mod.svm, data_test, probability = TRUE)
data_pred_svm_matrix <- attr(data_pred_svm, "probabilities")[,]


resultados_modelos[2,][1] <- "SVM" 
resultados_modelos[2,][2] <- unlist(f_mejorcutpoint(data_pred_svm_matrix ,data_test))[1]
resultados_modelos[2,][3] <- unlist(f_mejorcutpoint(data_pred_svm_matrix ,data_test))[2]

#### Neural Networks ####
library(neuralnet)
library(NeuralNetTools)

set.seed(1234)
mod.nn <- neuralnet(objetivo~.,data=data_train)

summary(mod.nn)
names(mod.nn)
plot(mod.nn)

data_pred_nn <- predict(mod.nn,data_test)


resultados_modelos[3,][1] <- "Neural Network" 
resultados_modelos[3,][2] <- unlist(f_mejorcutpoint(data_pred_nn ,data_test))[1]
resultados_modelos[3,][3] <- unlist(f_mejorcutpoint(data_pred_nn ,data_test))[2]

####Regresion logistica####

#Algoritmo regresion logistica
mod.log <- glm(objetivo~., 
               family= binomial(link = "logit"),
               data= data_train)
summary(mod.log)

#Marginal effects, para analizar la magnitud de los betas
library(margins)
margins(mod.log)
"como aumenta o disminuye la probabilidad del evento"

data_pred_log <- predict(mod.log, data_test, type="response")
data_pred_log_matrix <- matrix(data_pred_log)
data_pred_log_df <- as.data.frame(data_pred_log_matrix)

data_pred_log_df$V2 <- 1 - data_pred_log_df$V1
colnames(data_pred_log_df)
data_pred_log_df_ord <- data_pred_log_df[,c(2,1)]

resultados_modelos[4,][1] <- "Logistic Regression" 
resultados_modelos[4,][2] <- unlist(f_mejorcutpoint(data_pred_log_df_ord ,data_test))[1]
resultados_modelos[4,][3] <- unlist(f_mejorcutpoint(data_pred_log_df_ord ,data_test))[2]

#### Comparacion de resultados ####
resultados_modelos

mejor_costo <- min(as.numeric(resultados_modelos[,2]))

mejor_modelo <- resultados_modelos[,1][which(resultados_modelos == mejor_costo, arr.ind = TRUE)[1]]

#### Seleccion de variables significativas con Stepwise ####
mod_variables_sig <- step(glm(objetivo~., 
         family= binomial(link = "logit"),
         data= data), direction = "both")

atributo_variables <- attr(mod_variables_sig$terms,"variables")


variables_sig <- as.character(unlist(as.list(atributo_variables)))
variables_sig <- variables_sig[-1]
length(variables_sig)


data2 <- data[,c(variables_sig)]
data2 <- data2[,c(2,3,4,5,6,7,8,9,10,11,12,1)]


####Segunda corrida de modelos con variables significativas por STEPWISE####
# Crear bases de entrenamiento y testeo
set.seed(12345)
train_ind <- sample(1:nrow(data2), 
                    size = floor(0.8 * nrow(data2)))

data_train2 <- data2[train_ind, ]
data_test2 <- data2[-train_ind, ]

#### Random Forests 2 #### 
library(randomForest)
set.seed(1234)
rf.mod2 <- randomForest(objetivo ~ ., data = data_train2, importance=TRUE)

data_pred2 <- predict(rf.mod2, data_test2, type = "prob")


resultados_modelos[5,][1] <- "Random Forest 2" 
resultados_modelos[5,][2] <- unlist(f_mejorcutpoint(data_pred2,data_test2))[1]
resultados_modelos[5,][3] <- unlist(f_mejorcutpoint(data_pred2,data_test2))[2]

#### SVM 2 ####
library(e1071)

mod.svm2 <- svm(objetivo ~ ., data= data_train2, probability = TRUE)
data_pred_svm2 <- predict(mod.svm2, data_test2, probability = TRUE)
data_pred_svm_matrix2 <- attr(data_pred_svm2, "probabilities")[,]


resultados_modelos[6,][1] <- "SVM 2" 
resultados_modelos[6,][2] <- unlist(f_mejorcutpoint(data_pred_svm_matrix2 ,data_test2))[1]
resultados_modelos[6,][3] <- unlist(f_mejorcutpoint(data_pred_svm_matrix2 ,data_test2))[2]

#### Neural Networks 2 ####
set.seed(1234)
mod.nn2 <- neuralnet(objetivo~.,data=data_train2)
summary(mod.nn2)


data_pred_nn2 <- predict(mod.nn2,data_test2)


resultados_modelos[7,][1] <- "Neural Network 2" 
resultados_modelos[7,][2] <- unlist(f_mejorcutpoint(data_pred_nn2 ,data_test2))[1]
resultados_modelos[7,][3] <- unlist(f_mejorcutpoint(data_pred_nn2 ,data_test2))[2]

####Regresion logistica 2####

#Algoritmo regresion logistica
mod.log2 <- glm(objetivo~., 
               family= binomial(link = "logit"),
               data= data_train2)
summary(mod.log2)

#Marginal effects, para analizar la magnitud de los betas
margins(mod.log2)
"como aumenta o disminuye la probabilidad del evento"

data_pred_log2 <- predict(mod.log2, data_test2, type="response")
data_pred_log_matrix2 <- matrix(data_pred_log2)
data_pred_log_df2 <- as.data.frame(data_pred_log_matrix2)

data_pred_log_df2$V2 <- 1 - data_pred_log_df2$V1
colnames(data_pred_log_df2)
data_pred_log_df_ord2 <- data_pred_log_df2[,c(2,1)]

resultados_modelos[8,][1] <- "Logistic Regression 2" 
resultados_modelos[8,][2] <- unlist(f_mejorcutpoint(data_pred_log_df_ord2 ,data_test2))[1]
resultados_modelos[8,][3] <- unlist(f_mejorcutpoint(data_pred_log_df_ord2 ,data_test2))[2]

#### Comparacion de resultados ####
resultados_modelos

mejor_costo <- min(as.numeric(resultados_modelos[,2]))

mejor_modelo <- resultados_modelos[,1][which(resultados_modelos == mejor_costo, arr.ind = TRUE)[1]]
mejor_modelo


#### Metricas Adicionales mejor modelo ####
#Costo sin plan de prevención y sin herramienta de clasificación
matriz_costos <- rbind(c(0,19020000),c(3350000,12525000))
matriz_confusion_sin_plan <- rbind(c(1229,239),c(0,0))
matriz_confusion_sin_herramienta <- rbind(c(600,61),c(328,108))

costo_sin_plan <- sum(matriz_costos*matriz_confusion_sin_plan)
costo_sin_herramienta <- sum(matriz_costos*matriz_confusion_sin_herramienta)

#Performance del modelo
library(caret)
data_pred_df <- as.data.frame(data_pred)

data_pred_df$clasif <- 0
data_pred_df$clasif[data_pred_df[,2]>(resultados_modelos[,3][which(resultados_modelos == mejor_costo, arr.ind = TRUE)[1]])] <- 1
data_pred_df$clasif <- as.factor(data_pred_df$clasif)

table(data_pred_df$clasif, data_test[,ncol(data)])
mean(data_pred_df$clasif==data_test[,ncol(data)])
confusionMatrix(data_pred_df$clasif , data_test[,ncol(data)], positive = "1")

#Ahorro de la solución
ahorro_sinplan= costo_sin_plan - mejor_costo
ahorro_sin_herramienta = costo_sin_herramienta - mejor_costo
