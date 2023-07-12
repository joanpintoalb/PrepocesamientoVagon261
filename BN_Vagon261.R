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


### TRATAMIENTO DE DIAS ###

#SELECCIONAMOS LOS DÍAS
Lunes <- sqldf::sqldf("SELECT DISTINCT Fecha,Dia_Nombre Dia_Venta from data_vagon261_final WHERE Dia_Nombre = 'Mon' order by Fecha ")
Martes <- sqldf::sqldf("SELECT DISTINCT Fecha,Dia_Nombre Dia_Venta from data_vagon261_final WHERE Dia_Nombre = 'Tue' order by Fecha ")
Miercoles <- sqldf::sqldf("SELECT DISTINCT Fecha,Dia_Nombre Dia_Venta from data_vagon261_final WHERE Dia_Nombre = 'Wed' order by Fecha ")
Jueves <- sqldf::sqldf("SELECT DISTINCT Fecha,Dia_Nombre Dia_Venta from data_vagon261_final WHERE Dia_Nombre = 'Thu' order by Fecha ")
Viernes <- sqldf::sqldf("SELECT DISTINCT Fecha,Dia_Nombre Dia_Venta from data_vagon261_final WHERE Dia_Nombre = 'Fri' order by Fecha ")
Sabado <- sqldf::sqldf("SELECT DISTINCT Fecha,Dia_Nombre Dia_Venta from data_vagon261_final WHERE Dia_Nombre = 'Sat' order by Fecha ")
Domingo <- sqldf::sqldf("SELECT DISTINCT Fecha,Dia_Nombre Dia_Venta from data_vagon261_final WHERE Dia_Nombre = 'Sun' order by Fecha ")

#ORDENAMOS LOS DÍAS DE MENOR A MAYOR
Lunes <-  Lunes[order(as.Date(Lunes$Fecha, format="%d/%m/%Y")),]
Martes <-  Martes[order(as.Date(Martes$Fecha, format="%d/%m/%Y")),]
Miercoles <-  Miercoles[order(as.Date(Miercoles$Fecha, format="%d/%m/%Y")),]
Jueves <-  Jueves[order(as.Date(Jueves$Fecha, format="%d/%m/%Y")),]
Viernes <- Viernes[order(as.Date(Viernes$Fecha, format="%d/%m/%Y")),]
Sabado <-  Sabado[order(as.Date(Sabado$Fecha, format="%d/%m/%Y")),]
Domingo <- Domingo[order(as.Date(Domingo$Fecha, format="%d/%m/%Y")),]


#OBTENEMOS FECHAS DE VENTA
Lunes_Hei     <- sqldf::sqldf("SELECT DISTINCT A.Fecha from data_vagon261_final A JOIN Lunes     B ON A.Fecha = B.Fecha WHERE Nombre_Producto = 'HEINEKEN 330ML'")
Martes_Hei    <- sqldf::sqldf("SELECT DISTINCT A.Fecha from data_vagon261_final A JOIN Martes    B ON A.Fecha = B.Fecha WHERE Nombre_Producto = 'HEINEKEN 330ML'")
Miercoles_Hei <- sqldf::sqldf("SELECT DISTINCT A.Fecha from data_vagon261_final A JOIN Miercoles B ON A.Fecha = B.Fecha WHERE Nombre_Producto = 'HEINEKEN 330ML'")
Jueves_Hei    <- sqldf::sqldf("SELECT DISTINCT A.Fecha from data_vagon261_final A JOIN Jueves    B ON A.Fecha = B.Fecha WHERE Nombre_Producto = 'HEINEKEN 330ML'")
Viernes_Hei   <- sqldf::sqldf("SELECT DISTINCT A.Fecha from data_vagon261_final A JOIN Viernes   B ON A.Fecha = B.Fecha WHERE Nombre_Producto = 'HEINEKEN 330ML'")
Sabado_Hei    <- sqldf::sqldf("SELECT DISTINCT A.Fecha from data_vagon261_final A JOIN Sabado    B ON A.Fecha = B.Fecha WHERE Nombre_Producto = 'HEINEKEN 330ML'")
Domingo_Hei   <- sqldf::sqldf("SELECT DISTINCT A.Fecha from data_vagon261_final A JOIN Domingo   B ON A.Fecha = B.Fecha WHERE Nombre_Producto = 'HEINEKEN 330ML'")

Lunes_Hei$Venta  <- 'yes'
Martes_Hei$Venta  <- 'yes'   
Miercoles_Hei$Venta  <- 'yes'
Jueves_Hei$Venta  <- 'yes'   
Viernes_Hei$Venta  <- 'yes'  
Sabado_Hei$Venta  <- 'yes'   
Domingo_Hei$Venta  <- 'yes'  

HEINEKEN_330ML_Lunes      <- merge(x = Lunes, y = Lunes_Hei    , by = "Fecha",all.x = TRUE)
HEINEKEN_330ML_Martes     <- merge(x = Martes, y = Martes_Hei   , by = "Fecha",all.x = TRUE)
HEINEKEN_330ML_Miercoles  <- merge(x = Miercoles, y = Miercoles_Hei, by = "Fecha",all.x = TRUE)
HEINEKEN_330ML_Jueves     <- merge(x = Jueves, y = Jueves_Hei   , by = "Fecha",all.x = TRUE)
HEINEKEN_330ML_Viernes    <- merge(x = Viernes, y = Viernes_Hei  , by = "Fecha",all.x = TRUE)
HEINEKEN_330ML_Sabado     <- merge(x = Sabado, y = Sabado_Hei   , by = "Fecha",all.x = TRUE)
HEINEKEN_330ML_Domingo    <- merge(x = Domingo, y = Domingo_Hei  , by = "Fecha",all.x = TRUE)

HEINEKEN_330ML_Lunes[is.na(HEINEKEN_330ML_Lunes)] <- 'no'
HEINEKEN_330ML_Martes[is.na(HEINEKEN_330ML_Martes)] <- 'no'
HEINEKEN_330ML_Miercoles[is.na(HEINEKEN_330ML_Miercoles)] <- 'no'
HEINEKEN_330ML_Jueves[is.na(HEINEKEN_330ML_Jueves)] <- 'no'
HEINEKEN_330ML_Viernes[is.na(HEINEKEN_330ML_Viernes)] <- 'no'
HEINEKEN_330ML_Sabado[is.na(HEINEKEN_330ML_Sabado)] <- 'no'
HEINEKEN_330ML_Domingo[is.na(HEINEKEN_330ML_Domingo)] <- 'no'

HEINEKEN_330ML_Lunes     <- select (HEINEKEN_330ML_Lunes    , -Dia_Venta)
HEINEKEN_330ML_Martes    <- select (HEINEKEN_330ML_Martes   , -Dia_Venta)
HEINEKEN_330ML_Miercoles <- select (HEINEKEN_330ML_Miercoles, -Dia_Venta)
HEINEKEN_330ML_Jueves    <- select (HEINEKEN_330ML_Jueves   , -Dia_Venta)
HEINEKEN_330ML_Viernes   <- select (HEINEKEN_330ML_Viernes  , -Dia_Venta)
HEINEKEN_330ML_Sabado    <- select (HEINEKEN_330ML_Sabado   , -Dia_Venta)
HEINEKEN_330ML_Domingo   <- select (HEINEKEN_330ML_Domingo  , -Dia_Venta)

#TRANSPONEMOS
HEINEKEN_330ML_Lunes     <- data.frame(t(HEINEKEN_330ML_Lunes    [-1]))
HEINEKEN_330ML_Martes    <- data.frame(t(HEINEKEN_330ML_Martes   [-1]))
HEINEKEN_330ML_Miercoles <- data.frame(t(HEINEKEN_330ML_Miercoles[-1]))
HEINEKEN_330ML_Jueves    <- data.frame(t(HEINEKEN_330ML_Jueves   [-1]))
HEINEKEN_330ML_Viernes   <- data.frame(t(HEINEKEN_330ML_Viernes  [-1]))
HEINEKEN_330ML_Sabado    <- data.frame(t(HEINEKEN_330ML_Sabado   [-1]))
HEINEKEN_330ML_Domingo   <- data.frame(t(HEINEKEN_330ML_Domingo  [-1]))



### TRATAMIENTO RANGOS DE HORARIO ###

#SELECCIONAMOS LOS RANGOS DE HORARIO
de12a4  <- sqldf::sqldf("SELECT DISTINCT Fecha from data_vagon261_final order by Fecha ")
de4a6   <- sqldf::sqldf("SELECT DISTINCT Fecha from data_vagon261_final order by Fecha ")
de6a8   <- sqldf::sqldf("SELECT DISTINCT Fecha from data_vagon261_final order by Fecha ")
de8a10  <- sqldf::sqldf("SELECT DISTINCT Fecha from data_vagon261_final order by Fecha ")
de10a12 <- sqldf::sqldf("SELECT DISTINCT Fecha from data_vagon261_final order by Fecha ")
de12a2  <- sqldf::sqldf("SELECT DISTINCT Fecha from data_vagon261_final order by Fecha ")
masde2  <- sqldf::sqldf("SELECT DISTINCT Fecha from data_vagon261_final order by Fecha ")

de12a4$Rango_Horario  <- '1. 12pm a 4pm'
de4a6$Rango_Horario   <- '2. 4pm a 6pm'
de6a8$Rango_Horario   <- '3. 6pm a 8pm'
de8a10$Rango_Horario  <- '4. 8pm a 10pm'
de10a12$Rango_Horario <- '5. 10pm a 12am'
de12a2$Rango_Horario  <- '6. 12am a 2am'
masde2$Rango_Horario  <- '7. 2am a +'


#OBTENEMOS FECHAS DE VENTA Y RANGO DE VENTA
Ventas_Hei  <- sqldf::sqldf("SELECT DISTINCT Fecha,Rango_Horario_Venta Rango_Horario from data_vagon261_final WHERE Nombre_Producto = 'HEINEKEN 330ML'")
Ventas_Hei$Venta <- 'yes'


HEINEKEN_330ML_de12a4  <- merge(x = de12a4, y = Ventas_Hei    , by=c("Fecha","Rango_Horario"),all.x = TRUE)
HEINEKEN_330ML_de4a6   <- merge(x = de4a6, y = Ventas_Hei    , by=c("Fecha","Rango_Horario"),all.x = TRUE)
HEINEKEN_330ML_de6a8   <- merge(x = de6a8, y = Ventas_Hei    , by=c("Fecha","Rango_Horario"),all.x = TRUE)
HEINEKEN_330ML_de8a10  <- merge(x = de8a10, y = Ventas_Hei    , by=c("Fecha","Rango_Horario"),all.x = TRUE)
HEINEKEN_330ML_de10a12 <- merge(x = de10a12, y = Ventas_Hei    , by=c("Fecha","Rango_Horario"),all.x = TRUE)
HEINEKEN_330ML_de12a2  <- merge(x = de12a2, y = Ventas_Hei    , by=c("Fecha","Rango_Horario"),all.x = TRUE)
HEINEKEN_330ML_masde2  <- merge(x = masde2, y = Ventas_Hei    , by=c("Fecha","Rango_Horario"),all.x = TRUE)


HEINEKEN_330ML_de12a4[is.na(HEINEKEN_330ML_de12a4)] <- 'no' 
HEINEKEN_330ML_de4a6[is.na(HEINEKEN_330ML_de4a6)] <- 'no'  
HEINEKEN_330ML_de6a8[is.na(HEINEKEN_330ML_de6a8)] <- 'no'  
HEINEKEN_330ML_de8a10[is.na(HEINEKEN_330ML_de8a10)] <- 'no' 
HEINEKEN_330ML_de10a12[is.na(HEINEKEN_330ML_de10a12)] <- 'no'
HEINEKEN_330ML_de12a2[is.na(HEINEKEN_330ML_de12a2)] <- 'no' 
HEINEKEN_330ML_masde2[is.na(HEINEKEN_330ML_masde2)] <- 'no' 


HEINEKEN_330ML_de12a4 <- select (HEINEKEN_330ML_de12a4 , -Rango_Horario)
HEINEKEN_330ML_de4a6  <- select (HEINEKEN_330ML_de4a6  , -Rango_Horario)
HEINEKEN_330ML_de6a8  <- select (HEINEKEN_330ML_de6a8  , -Rango_Horario)
HEINEKEN_330ML_de8a10 <- select (HEINEKEN_330ML_de8a10 , -Rango_Horario)
HEINEKEN_330ML_de10a12<- select (HEINEKEN_330ML_de10a12, -Rango_Horario)
HEINEKEN_330ML_de12a2 <- select (HEINEKEN_330ML_de12a2 , -Rango_Horario)
HEINEKEN_330ML_masde2 <- select (HEINEKEN_330ML_masde2 , -Rango_Horario)


#TRANSPONEMOS
HEINEKEN_330ML_de12a4  <- data.frame(t(HEINEKEN_330ML_de12a4 [-1]))
HEINEKEN_330ML_de4a6   <- data.frame(t(HEINEKEN_330ML_de4a6  [-1]))
HEINEKEN_330ML_de6a8   <- data.frame(t(HEINEKEN_330ML_de6a8  [-1]))
HEINEKEN_330ML_de8a10  <- data.frame(t(HEINEKEN_330ML_de8a10 [-1]))
HEINEKEN_330ML_de10a12 <- data.frame(t(HEINEKEN_330ML_de10a12[-1]))
HEINEKEN_330ML_de12a2  <- data.frame(t(HEINEKEN_330ML_de12a2 [-1]))
HEINEKEN_330ML_masde2  <- data.frame(t(HEINEKEN_330ML_masde2 [-1]))
















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




############################################################
############################################################




library(neuralnet)
datos <- sqldf::sqldf("SELECT *,
                         CASE 
                         WHEN Dia_Nombre = 'Mon' THEN '1'
                         WHEN Dia_Nombre LIKE 'Tue' THEN '2'
                         WHEN Dia_Nombre LIKE 'Wed' THEN '3'
                         WHEN Dia_Nombre LIKE 'Thu' THEN '4'
                         WHEN Dia_Nombre LIKE 'Fri' THEN '5'
                         WHEN Dia_Nombre LIKE 'Sat' THEN '6'
                         WHEN Dia_Nombre LIKE 'Sun' THEN '7'
                         END AS Dia,
                         CASE
                         WHEN Rango_Horario_Venta = '1. 12pm a 4pm' THEN '1'
                         WHEN Rango_Horario_Venta = '2. 4pm a 6pm' THEN '2'
                         WHEN Rango_Horario_Venta = '3. 6pm a 8pm' THEN '3'
                         WHEN Rango_Horario_Venta = '4. 8pm a 10pm' THEN '4'
                         WHEN Rango_Horario_Venta = '5. 10pm a 12am' THEN '5'
                         WHEN Rango_Horario_Venta = '6. 12am a 2am' THEN '6'
                         WHEN Rango_Horario_Venta = '7. 2am a +' THEN '7'
                         END AS Rango
                         from  data")

datos <- sqldf::sqldf("SELECT Dia, Rango, output from datos")

datos$Dia <- as.numeric(datos$Dia)
datos$Rango <- as.numeric(datos$Rango)
datos$output <- as.numeric(datos$output)
datos$output <- datos$output-1



# Crear la partición muestral
set.seed(123)
particion <- sample.split(datos$output, SplitRatio = 0.9)

# Crear el conjunto de entrenamiento y de prueba
entrenamiento <- subset(datos, particion == TRUE)
prueba <- subset(datos, particion == FALSE)

# Crear la red neuronal
modelo <- neuralnet(output ~ Dia + Rango, data = entrenamiento, hidden = 1,rep=2,algorithm = "rprop-")

predicciones <- compute(modelo, prueba)
# Evaluar el modelo
predicciones_clasif <- ifelse(predicciones$net.result > 0.5, 1, 0)
accuracy1 <- mean(predicciones_clasif == prueba$output)
print(paste0("Precisión del modelo: ", accuracy1))


# Crear la red neuronal
modelo <- neuralnet(output ~ Dia + Rango, data = entrenamiento, hidden = 2,rep=3,algorithm = "rprop+")

predicciones <- compute(modelo, prueba)
# Evaluar el modelo
predicciones_clasif <- ifelse(predicciones$net.result > 0.5, 1, 0)
accuracy2 <- mean(predicciones_clasif == prueba$output)
print(paste0("Precisión del modelo: ", accuracy2))


?neuralnet


# Calcular los valores predichos
PRED9 <- as.factor(predicciones_clasif)

# Calcular la matriz de confusion
tabla9 <- caret::confusionMatrix(PRED9,prueba$output,positive = "1")
tabla9











############################################################
############################################################














###RESULTADOS MODELOS####


GINI <- rbind(GINI_1, GINI_2, GINI_3, GINI_4, GINI_5,GINI_6)
KS <- rbind(ks_1, ks_2, ks_3,ks_4, ks_5,ks_6)
LogLoss <- rbind(LogLoss_1, LogLoss_2, LogLoss_3,LogLoss_4, LogLoss_5, LogLoss_6)

Accuracy=rbind(Accuracy1, Accuracy2, Accuracy3, Accuracy4, Accuracy5, Accuracy6)
Sensibilidad=rbind(Sensitivity1, Sensitivity2, Sensitivity3, Sensitivity4, Sensitivity5, Sensitivity6)
Especificidad=rbind(Especificidad1, Especificidad2, Especificidad3, Especificidad4,Especificidad5, Especificidad6)


resultado <- data.frame(GINI,KS,LogLoss,Accuracy,Sensibilidad,Especificidad)
rownames(resultado) <- c('NaiveBayes e1071','NaiveBayes bnlearn','TAN bnlearn','KNN Caret', 'Hill-climbing bnlearn' ,'TABU search bnlearn')
colnames(resultado) <- c('GINI','KS','LogLoss','Precisión','Sensibilidad','Especificidad')
resultado <- round(resultado,2)
resultado

## Resultado Ordenado #####

# ordenamos por el Indicador que deseamos, quiza Sensibilidad en forma decreciente
Resultado_ordenado <- resultado[order(-Sensibilidad),] 
Resultado_ordenado

#si miramos clases ver:
#cuando hay mas 0 que 1 miramos la sensibilidad
#cuando hay mas 1 que 0 miramos la especificidad

sqldf::sqldf("SELECT output, count(output) FROM data group by output")

##_-----------------------------------------------------------



