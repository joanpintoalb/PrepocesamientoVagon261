library(bnlearn)
library(dplyr)
library(sqldf)
library(e1071)
library(caret)
library(ggplot2)
library(data.table)
library(readxl)
library(MLmetrics)
library(ROSE)


##-----------------------------------------------------------
data2 <- sqldf::sqldf("SELECT Fecha, Dia_Nombre, Rango_Horario_Venta, Nombre_Producto  FROM data_vagon261_final WHERE Nombre_Producto = 'HEINEKEN 330ML' ") #Producto
sqldf::sqldf("SELECT DISTINCT FECHA FROM data_vagon261_final ORDER BY FECHA") #Para saber todas las fechas de la data

ListaFechas <- read_excel("ListaFechas.xlsx")
ListaFechas$Fecha <-as.character(ListaFechas$Fecha)

Temporal <- sqldf::sqldf("SELECT *, SUBSTRING(fecha, 1,4) AÑO, SUBSTRING(fecha, 6,2) MES, SUBSTRING(fecha, 9,2) DIA from  ListaFechas") #Extraemos año,mes,dia
Temporal$Fecha2 <- paste(Temporal$MES,Temporal$DIA,Temporal$AÑO,sep = "/") #Juntamos en un nuevo formato m/d/y
Temporal$Fecha3 <- paste(Temporal$DIA,Temporal$MES,Temporal$AÑO,sep = "/") #Juntamos en un nuevo formato m/d/y
Temporal$Dia_Nombre <- weekdays(Temporal$Fecha2) #sacamos nombre del dia de la semana
ListaFechas <- sqldf::sqldf("SELECT  Fecha3, Rango_Horario_Venta, Dia_Nombre FROM Temporal")

#hacemos match de las ventas de Heineken con el total de los días
data <- sqldf::sqldf("SELECT DISTINCT A.Fecha3, A.Dia_Nombre, A.Rango_Horario_venta,Nombre_Producto output
             FROM ListaFechas A LEFT JOIN data2 B ON A.Fecha3 = B.Fecha AND A.Rango_Horario_Venta = B.Rango_Horario_Venta ")

data[is.na(data)] <- 0
data$output <- ifelse(data$output=='HEINEKEN 330ML',1,0)

data <- sqldf::sqldf("SELECT Dia_Nombre, Rango_Horario_Venta, output FROM data")

data$Dia_Nombre <- as.factor(data$Dia_Nombre)
data$Rango_Horario_Venta <- as.factor(data$Rango_Horario_Venta)
data$output <- as.factor(data$output)


###############BALANCEO DE LA DATA##############

sqldf::sqldf("SELECT distinct output, count(output) FROM data group by output")

# Realizar el submuestreo de los datos utilizando ovun.sample
data_balanced_underS <- ovun.sample(output ~ ., data = data, method = "under", N = 2 * table(data$output)[[2]])
data_balanced_underS <- data_balanced_underS$data

sqldf::sqldf("SELECT distinct output, count(output) FROM data_balanced_underS group by output")

# Realizar el sobremuestreo de los datos utilizando ovun.sample
data_balanced_overS <- ovun.sample(output ~ ., data = data, method = "over")
data_balanced_overS <- data_balanced_overS$data

sqldf::sqldf("SELECT distinct output, count(output) FROM data_balanced_overS group by output")


###############PARTICION DE LA DATA##############
set.seed(123)
training.samples <- data$output %>% createDataPartition(p = 0.7, list = FALSE)
train  <- data[training.samples, ]
test <- data[-training.samples, ]

#UNDER
set.seed(123)
training.samples_U <- data_balanced_underS$output %>% createDataPartition(p = 0.7, list = FALSE)
train_U <- data_balanced_underS[training.samples_U, ]
test_U <- data_balanced_underS[-training.samples_U, ]

#OVER
set.seed(123)
training.samples_O <- data_balanced_overS$output %>% createDataPartition(p = 0.7, list = FALSE)
train_O <- data_balanced_overS[training.samples_O, ]
test_O <- data_balanced_overS[-training.samples_O, ]


########### 5) MODELADO ########################### 

red <- empty.graph(c("Dia_Nombre", "Rango_Horario_Venta", "output"))
red <- set.arc(red, "Dia_Nombre", "output")
red <- set.arc(red, "Rango_Horario_Venta", "output")
plot(red)


# modelo 1.- Naive Bayes
set.seed(123)
modelo1 <- e1071::naiveBayes(output~.,data = train_data_balanced_underS$data, laplace=4)


##probabilidades
proba1 <- predict(modelo1, newdata=test,type="raw")
proba1 <- proba1[,2]

predit1 <- predict(modelo1, newdata=test,type="class")

# Indicadores

AUC_1     <- MLmetrics::AUC(proba1,as.numeric(as.character(test$output)))
GINI_1    <- 2*AUC_1-1
ks_1      <- MLmetrics::KS_Stat(proba1,as.numeric(as.character(test$output)))
LogLoss_1 <- MLmetrics::LogLoss(proba1,as.numeric(as.character(test$output)))

# Calcular los valores predichos

PRED1 <- as.factor(predit1)

# Calcular la matriz de confusion
tabla1 <- caret::confusionMatrix(PRED1,test$output,positive = "1")
tabla1
modelo1$times
# sensibilidad
Sensitivity1 <-  tabla1$byClass[1]

# Precision
Accuracy1 <- MLmetrics::Accuracy(PRED1,test$output)

# Especificidad
Especificidad1 <- tabla1$byClass[2]







# modelo 2. - Naive Bayes blearn
set.seed(123)
modelo2 <- naive.bayes(train_O, "output")

##probabilidades
proba2 <- predict(modelo2,test_O, prob=TRUE)

#obtener las probabilidades
value <- attr(proba2, "prob")
t<- as.data.frame(value)
t <- t[2,]
t <- transpose(t)
proba2 <- t[,1]

predit2 <- predict(modelo2, test_O)

# Indicadores

AUC_2     <- MLmetrics::AUC(proba2,as.numeric(as.character(test_O$output)))
GINI_2    <- 2*AUC_2-1
ks_2      <- MLmetrics::KS_Stat(proba2,as.numeric(as.character(test_O$output)))
LogLoss_2 <- MLmetrics::LogLoss(proba2,as.numeric(as.character(test_O$output)))

# Calcular los valores predichos

PRED2 <- as.factor(predit2)

# Calcular la matriz de confusion
tabla2 <- caret::confusionMatrix(PRED2,test_O$output,positive = "1")
tabla2

# sensibilidad
Sensitivity2 <-  tabla2$byClass[1]

# Precision
Accuracy2 <- MLmetrics::Accuracy(PRED2,test_O$output)

# Especificidad
Especificidad2 <- tabla2$byClass[2]




# modelo 3.- TAN blearn
set.seed(123)
tan <- tree.bayes(train_O, "output")
modelo3 = bn.fit(tan, test_O, method = "bayes")

##probabilidades
proba3 <- predict(modelo3,test_O, prob=TRUE)

#obtener las probabilidades
value <- attr(proba3, "prob")
t<- as.data.frame(value)
t <- t[2,]
t <- transpose(t)
proba3 <- t[,1]

predit3 <- predict(modelo3, test_O)

# Indicadores

AUC_3     <- MLmetrics::AUC(proba3,as.numeric(as.character(test_O$output)))
GINI_3    <- 2*AUC_3-1
ks_3      <- MLmetrics::KS_Stat(proba3,as.numeric(as.character(test_O$output)))
LogLoss_3 <- MLmetrics::LogLoss(proba3,as.numeric(as.character(test_O$output)))

# Calcular los valores predichos

PRED3 <- as.factor(predit3)

# Calcular la matriz de confusion
tabla3 <- caret::confusionMatrix(PRED3,test_O$output,positive = "1")
tabla3
modelo3$times
# sensibilidad
Sensitivity3 <-  tabla3$byClass[1]

# Precision
Accuracy3 <- MLmetrics::Accuracy(PRED3,test_O$output)

# Especificidad
Especificidad3 <- tabla3$byClass[2]




# modelo 4.- KNN

set.seed(123)
ctrl <- caret::trainControl(method="LGOCV", number=6)

modelo4 <- caret::train(output ~ ., data = train_O, 
                        method = "knn",
                        preProcess="range",
                        trControl = ctrl,
                        metric="Accuracy")

##probabilidades y clase
proba4 <- predict(modelo4,newdata = test_O, type="prob")
proba4 <- proba4[,2]

predit4 <- predict(modelo4,newdata = test_O)

# Indicadores
AUC_4     <- MLmetrics::AUC(proba4,as.numeric(as.character(test_O$output)))
GINI_4    <- 2*AUC_4-1
ks_4      <- MLmetrics::KS_Stat(proba4,as.numeric(as.character(test_O$output)))
LogLoss_4 <- MLmetrics::LogLoss(proba4,as.numeric(as.character(test_O$output)))

# Calcular los valores predichos
PRED4 <- as.factor(predit4)

# Calcular la matriz de confusion
tabla4 <- caret::confusionMatrix(PRED4,test_O$output,positive = "1")
tabla4
modelo4$times
# sensibilidad
Sensitivity4 <-  tabla4$byClass[1]

# Precision
Accuracy4 <- MLmetrics::Accuracy(PRED4,test_O$output)

# Especificidad
Especificidad4 <- tabla4$byClass[2]

Especificidad4
AUC_4
LogLoss_4






# modelo 5.- Hill-climbing blearn
set.seed(123)
ajuste_red <- hc(train_O, start = red ,score = "aic", max.iter=20, restart = 8)
modelo5 <- bn.fit(ajuste_red, train_O, method = "bayes")


##probabilidades
proba5 = predict(modelo5, node = "output", data = test_O,prob=TRUE)

#obtener las probabilidades
value <- attr(proba5, "prob")
t<- as.data.frame(value)
t <- t[2,]
t <- transpose(t)
proba5 <- t[,1]

predit5 <- predict(modelo5, node = "output", data = test_O)

# Indicadores

AUC_5     <- MLmetrics::AUC(proba5,as.numeric(as.character(test_O$output)))
GINI_5    <- 2*AUC_5-1
ks_5      <- MLmetrics::KS_Stat(proba5,as.numeric(as.character(test_O$output)))
LogLoss_5 <- MLmetrics::LogLoss(proba5,as.numeric(as.character(test_O$output)))

# Calcular los valores predichos

PRED5 <- as.factor(predit5)

# Calcular la matriz de confusion
tabla5 <- caret::confusionMatrix(PRED5,test_O$output,positive = "1")
tabla5

# sensibilidad
Sensitivity5 <-  tabla5$byClass[1]

# Precision
Accuracy5 <- MLmetrics::Accuracy(PRED5,test_O$output)

# Especificidad
Especificidad5 <- tabla5$byClass[2]
AUC_5
LogLoss_5
Especificidad5





# modelo 6.- TABU search bnlearn
set.seed(123)
tabu <- tabu(train_O, start=red, score = "bic",tabu=10)
modelo6 <- bn.fit(tabu, train_O, method = "bayes")


##probabilidades
proba6 = predict(modelo6, node = "output", data = test_O,prob=TRUE)

#obtener las probabilidades
value <- attr(proba6, "prob")
t<- as.data.frame(value)
t <- t[2,]
t <- transpose(t)
proba6 <- t[,1]

predit6 <- predict(modelo6, node = "output", data = test_O)

# Indicadores

AUC_6     <- MLmetrics::AUC(proba6,as.numeric(as.character(test_O$output)))
GINI_6    <- 2*AUC_6-1
ks_6      <- MLmetrics::KS_Stat(proba6,as.numeric(as.character(test_O$output)))
LogLoss_6 <- MLmetrics::LogLoss(proba6,as.numeric(as.character(test_O$output)))

# Calcular los valores predichos
PRED6 <- as.factor(predit6)

# Calcular la matriz de confusion
tabla6 <- caret::confusionMatrix(PRED6,test_O$output,positive = "1")
tabla6

# sensibilidad
Sensitivity6 <-  tabla6$byClass[1]

# Precision
Accuracy6 <- MLmetrics::Accuracy(PRED6,test_O$output)

# Especificidad
Especificidad6 <- tabla6$byClass[2]

Especificidad6
AUC_6
LogLoss_6




# modelo 7.- GLM
set.seed(123)
modelo7 <- glm(output ~ Dia_Nombre + Rango_Horario_Venta ,data = train_O, family = binomial(link = "logit"))


##probabilidades
proba7 = predict(modelo7, newdata = test_O, type = "response")
predit7 <- ifelse(proba7 > 0.5, 1, 0)


# Indicadores
AUC_7     <- MLmetrics::AUC(proba7,as.numeric(as.character(test_O$output)))
GINI_7    <- 2*AUC_7-1
ks_7      <- MLmetrics::KS_Stat(proba7,as.numeric(as.character(test_O$output)))
LogLoss_7 <- MLmetrics::LogLoss(proba7,as.numeric(as.character(test_O$output)))

# Calcular los valores predichos
PRED7 <- as.factor(predit7)

# Calcular la matriz de confusion
tabla7 <- caret::confusionMatrix(PRED7,test_O$output,positive = "1")
tabla7


# sensibilidad
Sensitivity7 <-  tabla7$byClass[1]

# Precision
Accuracy7 <- MLmetrics::Accuracy(PRED7,test_O$output)

# Especificidad
Especificidad7 <- tabla7$byClass[2]

AUC_7
LogLoss_7
Especificidad7

