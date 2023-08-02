

#Establecemos el directorio
setwd("C:/Users/Kary/Desktop/MBA ORT/2022/Machine learning supervisado/Obligatorio")
getwd()
#Cargamos la base de datos
base1 <-read.csv('C:/Users/Kary/Desktop/MBA ORT/2022/Machine learning supervisado/Obligatorio/datos.csv', stringsAsFactors = TRUE)
View(base1)


dim(base1)
str(base1)
summary(base1)
table(base1$STATUS)

#observamos si hay valores nulos
lapply(base1, function(x) sum(is.na(x)))
lapply(base1, function(x) sum(x=="")) #OCCUPATION_TYPE tiene 6280 valores 

# Se asigna la etiqueta 'SD' a la primera etiqueta sin valor de la variable OCCUPATION_TYPE 
levels(base1$OCCUPATION_TYPE)
levels(base1$OCCUPATION_TYPE)[1] <- 'SD'
levels(base1$OCCUPATION_TYPE)


#Cambiamos Default (1) y No default (0)

base1$STATUS <- ifelse(base1$STATUS == "Default", 1, 0)

#Realizamos análisis exploratotio de las variables
table(base1$CODE_GENDER)
table(base1$FLAG_OWN_CAR)
table(base1$FLAG_OWN_REALTY)
table(base1$NAME_INCOME_TYPE) #student = 9
table(base1$NAME_EDUCATION_TYPE) #Academic degree = 19
table(base1$NAME_FAMILY_STATUS)
table(base1$NAME_HOUSING_TYPE) #co-op apartment =77
table(base1$FLAG_WORK_PHONE)
table(base1$FLAG_PHONE)
table(base1$FLAG_EMAIL)
table(base1$OCCUPATION_TYPE) #SD=6280, HR staff=46, IT staff=41, Realty agents=39, secretaries=78, waiters/barme staff =70
table(base1$MAX_ATR) #120-149 = 41, 90-119 = 59


x = Filter(is.numeric, base1[,-1])
str(x)
summary(x)

# Revision de las variables cualitativas
levels(base1$CODE_GENDER)
levels(base1$FLAG_OWN_CAR)
levels(base1$FLAG_OWN_REALTY)
levels(base1$NAME_INCOME_TYPE)
levels(base1$NAME_EDUCATION_TYPE)
levels(base1$NAME_FAMILY_STATUS)
levels(base1$NAME_HOUSING_TYPE)
levels(base1$FLAG_WORK_PHONE)
levels(base1$FLAG_PHONE)
levels(base1$FLAG_EMAIL)
levels(base1$OCCUPATION_TYPE)
levels(base1$MAX_ATR)

# estadistica de las variables numericas
#boxplots

par(mfrow = c(3, 2))
boxplot(base1$CNT_CHILDREN, col = 'red', main = 'hijos', horizontal = F)
boxplot(base1$AMT_INCOME_TOTAL, col = 'green', main = 'ingresos', horizontal = F)
boxplot(base1$YEAR_BIRTH, col = 'blue', main = 'nacimiento', horizontal = F)
boxplot(base1$YEAR_EMPLOYED, col = 'yellow', main = 'años_trabajo', horizontal = F)
boxplot(base1$CNT_FAM_MEMBERS, col = 'red', main = 'miembros', horizontal = F)
boxplot(base1$ANT_CLI_MONTH, col = 'yellow', main = 'antiguedad', horizontal = F)

# Relacion intuitiva hijos y miembros de la familia
par(mfrow = c(1, 2))
boxplot(base1$CNT_CHILDREN, col = 'red', main = 'hijos', horizontal = F)
boxplot(base1$CNT_FAM_MEMBERS, col = 'red', main = 'miembros familiares', horizontal = F)


# Relacion intuitiva anos de trabajo y antiguedad del cliente
par(mfrow = c(1, 2))
boxplot(base1$YEAR_EMPLOYED, col = 'yellow', main = 'años_trabajo', horizontal = F)
boxplot(base1$ANT_CLI_MONTH, col = 'yellow', main = 'antiguedad del cliente', horizontal = F)


#histogramas
par(mfrow = c(3, 2))
hist(base1$CNT_CHILDREN, col = 'red', main = 'hijos')
hist(base1$AMT_INCOME_TOTAL, col = 'green', main = 'ingresos')
hist(base1$YEAR_BIRTH, col = 'blue', main = 'nacimiento')
hist(base1$YEAR_EMPLOYED, col = 'yellow', main = 'años_trabajo')
hist(base1$CNT_FAM_MEMBERS, col = 'red', main = 'miembros')
hist(base1$ANT_CLI_MONTH, col = 'green', main = 'antiguedad')


# Graficos de variables cualitativas

par(mfrow = c(3, 2))
barplot(table(base1$CODE_GENDER), col = c('lightpink1','lightblue'), main = 'género')
barplot(table(base1$FLAG_OWN_CAR), col = c('red','blue'), main = 'auto')
barplot(table(base1$FLAG_OWN_REALTY), col = c('red','blue'), main = 'bienes raíces')
barplot(table(base1$NAME_INCOME_TYPE), col = 'yellow', main = 'sueldo')
barplot(table(base1$NAME_EDUCATION_TYPE), col = 'magenta', main = 'educación')
barplot(table(base1$NAME_FAMILY_STATUS), col = 'green', main = 'estado marital')

## BOX PLOT CONTRA LA VARIABLE DEPENDIENTE

boxplot(base1$AMT_INCOME_TOTAL ~ base1$STATUS, col = "red",
        main = "INGRESO Y DEFAULT")

## BARPLOT CONTRA LA VARIABLE DEPENDIENTE
library(ggplot2)
plot1 <- ggplot(data = base1, aes(x=factor(base1$NAME_EDUCATION_TYPE), fill =factor(base1$STATUS))) +
  geom_bar() +
  ylab("Cantidad") +
  xlab("(1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown)") 

plot1

## histograma edad

plot3<-ggplot(data = base1, aes(x = base1$YEAR_BIRTH)) + 
  geom_histogram(bins = 50, fill = "purple", col = "blue", alpha = 0.3) + 
  scale_x_continuous(breaks = seq(min(0), max(90), by = 5), na.value = TRUE)

plot3

## histograma max atraso

plot4<-ggplot(data = base1, aes(x = base1$AMT_INCOME_TOTAL)) + 
  geom_histogram(bins = 10, fill = "purple", col = "green", alpha = 0.3) + 
  scale_x_continuous(breaks = seq(min(0), max(90), by = 5), na.value = TRUE)

plot4

#Retiramos las variables "FLAG_WORK_PHONE""FLAG_PHONE""FLAG_EMAIL""OCCUPATION_TYPE"   

base1 <- base1[,-c(13,14,15,16)]


####matriz correlacion
library(dplyr)
corr_mat <- base1 [,-1] %>% select_if(is.numeric)
dim(corr_mat)
library(corrplot)
a <- cor(corr_mat,method = "pearson")
library(tidyverse)
matriz_cor <- a %>% as.data.frame() %>% rownames_to_column() %>% pivot_longer(-rowname)


matriz_cor  %>% ggplot(aes(x= rowname, y= name, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value,2)), color ="white") +
  scale_fill_gradient2(low = "red", high = "darkgreen",mid ="blue", midpoint = 0,
                       limit = c(-1,1),
                       name ="Pearson")


#Separamos los datos en train y test
set.seed(1111)   
train <- sample(nrow(base1), nrow(base1)*0.7) 
test <- (-train)

############################################################################################################
###################################### Regresion logística ##########################################################################################################################

#creamos variables dummy tanto para la base Train como Test
datos_train <- base1[train, ]
datos_test  <- base1[-train, ]

#Base de entrenamiento con dummys
library(fastDummies)
datos_train2 <- datos_train %>% dplyr::select(CODE_GENDER, FLAG_OWN_CAR, FLAG_OWN_REALTY, NAME_INCOME_TYPE, NAME_EDUCATION_TYPE,NAME_FAMILY_STATUS,NAME_HOUSING_TYPE,MAX_ATR) %>%
  dummy_cols(remove_most_frequent_dummy = T, ignore_na = FALSE, remove_selected_columns = FALSE)
datos_train2 <- datos_train2 [,-c(1:8)]
datos_train2 <- datos_train %>% select_if(is.numeric) %>% bind_cols(datos_train2)

#Base de validación con dummys
datos_test2 <- datos_test %>% dplyr::select(CODE_GENDER, FLAG_OWN_CAR, FLAG_OWN_REALTY, NAME_INCOME_TYPE, NAME_EDUCATION_TYPE,NAME_FAMILY_STATUS,NAME_HOUSING_TYPE,MAX_ATR) %>%
  dummy_cols(remove_most_frequent_dummy = T, ignore_na = FALSE, remove_selected_columns = FALSE)
datos_test2 <- datos_test2 [,-c(1:8)]
datos_test2 <- datos_test %>% select_if(is.numeric) %>% bind_cols(datos_test2)

#base completa con dummys
base2 <- bind_rows(datos_train2, datos_test2)

#modelo completo regresión logística
base1_log=glm(STATUS~.,data = datos_train2, family=binomial)
library(car)
vif(base1_log)
# modelo seleccion de parametros
reg_step=step(base1_log)
summary(reg_step)
vif(reg_step)

# predicciones
log_pred<- ifelse(predict(base1_log, base2, type="response")> 0.55, 1, 0)
log.step_pred<- ifelse(predict(reg_step, base2, type="response")> 0.55, 1, 0)


#######
# log_pred

# matriz de confusion train
mc_train1=addmargins(table(log_pred[train], base2$STATUS[train]))
mc_train1

# matriz de confusion test
mc_test1=addmargins(table(log_pred[test], base2$STATUS[test]))
mc_test1

# curva ROC 
library(pROC)

roc_train1 <- roc(base2$STATUS[train], as.numeric(log_pred[train]))
roc_train1$auc

roc_test1 <- roc(base2$STATUS[test], as.numeric(log_pred[test]))
roc_test1$auc

#guardo las metricas en tablas
result_train1= data.frame(
  Modelo='Logit',
  Sensitividad=c((mc_train1[2,2])/mc_train1[3,2]),
  Especificidad=c((mc_train1[1,1])/mc_train1[3,1]),
  Precision=c((mc_train1[1,1]+mc_train1[2,2])/mc_train1[3,3]),
  AUC=c(roc_train1$auc),
  F1=2*c((mc_train1[2,2])/(mc_train1[3,2])*((mc_train1[1,1])/mc_train1[3,1])/(mc_train1[2,2]/mc_train1[3,2]+(mc_train1[1,1])/mc_train1[3,1])),
  FPR= 1-c((mc_train1[1,1])/mc_train1[3,1]),
  TP= mc_train1[2,2],
  TN= mc_train1[1,1],
  FP= mc_train1[2,1],
  FN= mc_train1[1,2]

  )

result_test1= data.frame(
  Modelo='Logit',
  Sensitividad=c((mc_test1[2,2])/mc_test1[3,2]),
  Especificidad=c((mc_test1[1,1])/mc_test1[3,1]),
  Precision=c((mc_test1[1,1]+mc_test1[2,2])/mc_test1[3,3]),
  AUC=c(roc_test1$auc),
  F1=2*c((mc_test1[2,2])/(mc_test1[3,2])*((mc_test1[1,1])/mc_test1[3,1])/(mc_test1[2,2]/mc_test1[3,2]+(mc_test1[1,1])/mc_test1[3,1])),
  FPR=1-c((mc_test1[1,1])/mc_test1[3,1]),
  TP= mc_test1[2,2],
  TN= mc_test1[1,1],
  FP= mc_test1[2,1],
  FN= mc_test1[1,2]
)

# donde voy a guardar los resultados acumulados
Resultados_log.pred1=result_train1
Resultados_log.pred2=result_test1

#######
# log.step_pred

# matriz de confusion train
mc_train2=addmargins(table(log.step_pred[train], base2$STATUS[train]))
mc_train2

# matriz de confusion test
mc_test2=addmargins(table(log.step_pred[test], base2$STATUS[test]))
mc_test2

# curva ROC 

roc_train2 <- roc(base2$STATUS[train], as.numeric(log.step_pred[train]))
roc_train2$auc

roc_test2 <- roc(base2$STATUS[test], as.numeric(log.step_pred[test]))
roc_test2$auc

#guardo las metricas en tablas
result_train2= data.frame(
  Modelo='Logit-Step',
  Sensitividad=c((mc_train2[2,2])/mc_train2[3,2]),
  Especificidad=c((mc_train2[1,1])/mc_train2[3,1]),
  Precision=c((mc_train2[1,1]+mc_train2[2,2])/mc_train2[3,3]),
  AUC=c(roc_train2$auc),
  F1=2*c((mc_train2[2,2])/(mc_train2[3,2])*((mc_train2[1,1])/mc_train2[3,1])/(mc_train2[2,2]/mc_train2[3,2]+(mc_train2[1,1])/mc_train2[3,1])),
  FPR= 1-c((mc_train2[1,1])/mc_train2[3,1]),
  TP= mc_train2[2,2],
  TN= mc_train2[1,1],
  FP= mc_train2[2,1],
  FN= mc_train2[1,2]

  )

result_test2= data.frame(
  Modelo='Logit-Step',
  Sensitividad=c((mc_test2[2,2])/mc_test2[3,2]),
  Especificidad=c((mc_test2[1,1])/mc_test2[3,1]),
  Precision=c((mc_test2[1,1]+mc_test2[2,2])/mc_test2[3,3]),
  AUC=c(roc_test2$auc),
  F1=2*c((mc_test2[2,2])/(mc_test2[3,2])*((mc_test2[1,1])/mc_test2[3,1])/(mc_test2[2,2]/mc_test2[3,2]+(mc_test2[1,1])/mc_test2[3,1])),
  FPR = 1-c((mc_test2[1,1])/mc_test2[3,1]),
  TP= mc_test2[2,2],
  TN= mc_test2[1,1],
  FP= mc_test2[2,1],
  FN= mc_test2[1,2]
)
Resultados_log.step_pred1=rbind(Resultados_log.pred1,result_train2)
Resultados_log.step_pred2=rbind(Resultados_log.pred2,result_test2)

#Comparamos resultados 
Resultados_log.step_pred1
Resultados_log.step_pred2

#############################################################################################################
###################################### arboles de decision #################################################################################

library(rpart)
library(rattle)

tree1= rpart(STATUS~., data=base1,subset=train, method="class")
tree2= rpart(STATUS~., data=base1,subset=train, method="class",parms  = list(split = "information")) #con gini y con information da la misma partición que el árbol original

asRules(tree1)
asRules(tree2)

fancyRpartPlot(tree1,sub="")
fancyRpartPlot(tree2,sub="")

plotcp(tree1)
plotcp(tree2)

pred_tree1<- ifelse(predict(tree1,base1)[,2]>0.55,1,0)
pred_tree2<- ifelse(predict(tree2,base1)[,2]>0.55,1,0)

tree1$variable.importance
tree2$variable.importance
#######
# pred_tree1

# matriz de confusion train
mc_train3=addmargins(table(pred_tree1[train], base1$STATUS[train]))
mc_train3

# matriz de confusion test
mc_test3=addmargins(table(pred_tree1[test], base1$STATUS[test]))
mc_test3

# curva ROC 
library(pROC)
roc_train3 <- roc(base1$STATUS[train], as.numeric(pred_tree1[train]))
roc_train3$auc

roc_test3 <- roc(base1$STATUS[test], as.numeric(pred_tree1[test]))
roc_test3$auc

#guardo las metricas en tablas
result_train3= data.frame(
  Modelo='Tree1',
  Sensitividad=c((mc_train3[2,2])/mc_train3[3,2]),
  Especificidad=c((mc_train3[1,1])/mc_train3[3,1]),
  Precision=c((mc_train3[1,1]+mc_train3[2,2])/mc_train3[3,3]),
  AUC=c(roc_train3$auc),
  F1=2*c((mc_train3[2,2])/(mc_train3[3,2])*((mc_train3[1,1])/mc_train3[3,1])/(mc_train3[2,2]/mc_train3[3,2]+(mc_train3[1,1])/mc_train3[3,1])),
  FPR=1-c((mc_train3[1,1])/mc_train3[3,1]),
  TP= mc_train3[2,2],
  TN= mc_train3[1,1],
  FP= mc_train3[2,1],
  FN= mc_train3[1,2]
)

result_test3= data.frame(
  Modelo='Tree1',
  Sensitividad=c((mc_test3[2,2])/mc_test3[3,2]),
  Especificidad=c((mc_test3[1,1])/mc_test3[3,1]),
  Precision=c((mc_test3[1,1]+mc_test3[2,2])/mc_test3[3,3]),
  AUC=c(roc_test3$auc),
  F1=2*c((mc_test3[2,2])/(mc_test3[3,2])*((mc_test3[1,1])/mc_test3[3,1])/(mc_test3[2,2]/mc_test3[3,2]+(mc_test3[1,1])/mc_test3[3,1])),
  FPR=1-c((mc_test3[1,1])/mc_test3[3,1]),
  TP= mc_test3[2,2],
  TN= mc_test3[1,1],
  FP= mc_test3[2,1],
  FN= mc_test3[1,2]
)
Resultados_pred.tree1_1=rbind(Resultados_log.step_pred1,result_train3)
Resultados_pred.tree1_2=rbind(Resultados_log.step_pred2,result_test3)


#######
# pred_tree2

# matriz de confusion train
mc_train4=addmargins(table(pred_tree2[train], base1$STATUS[train]))
mc_train4

# matriz de confusion test
mc_test4=addmargins(table(pred_tree2[test], base1$STATUS[test]))
mc_test4

# curva ROC 

roc_train4 <- roc(base1$STATUS[train], as.numeric(pred_tree2[train]))
roc_train4$auc

roc_test4 <- roc(base1$STATUS[test], as.numeric(pred_tree2[test]))
roc_test4$auc

#guardo las metricas en tablas
result_train4= data.frame(
  Modelo='Tree2',
  Sensitividad=c((mc_train4[2,2])/mc_train4[3,2]),
  Especificidad=c((mc_train4[1,1])/mc_train4[3,1]),
  Precision=c((mc_train4[1,1]+mc_train4[2,2])/mc_train4[3,3]),
  AUC=c(roc_train4$auc),
  F1=2*c((mc_train4[2,2])/(mc_train4[3,2])*((mc_train4[1,1])/mc_train4[3,1])/(mc_train4[2,2]/mc_train4[3,2]+(mc_train4[1,1])/mc_train4[3,1])),
  FPR=1-c((mc_train4[1,1])/mc_train4[3,1]),
  TP= mc_train4[2,2],
  TN= mc_train4[1,1],
  FP= mc_train4[2,1],
  FN= mc_train4[1,2]
)

result_test4= data.frame(
  Modelo='Tree2',
  Sensitividad=c((mc_test4[2,2])/mc_test4[3,2]),
  Especificidad=c((mc_test4[1,1])/mc_test4[3,1]),
  Precision=c((mc_test4[1,1]+mc_test4[2,2])/mc_test4[3,3]),
  AUC=c(roc_test4$auc),
  F1=2*c((mc_test4[2,2])/(mc_test4[3,2])*((mc_test4[1,1])/mc_test4[3,1])/(mc_test4[2,2]/mc_test4[3,2]+(mc_test4[1,1])/mc_test4[3,1])),
  FPR= 1-c((mc_test4[1,1])/mc_test4[3,1]),
  TP= mc_test4[2,2],
  TN= mc_test4[1,1],
  FP= mc_test4[2,1],
  FN= mc_test4[1,2]
)

Resultados_pred.tree2_1=rbind(Resultados_pred.tree1_1,result_train4)
Resultados_pred.tree2_2=rbind(Resultados_pred.tree1_2,result_test4)


#############################################################################################################
############################################# random forest ##################################################

library(ranger)

RF=ranger(as.factor(STATUS) ~., data=base1[train,], num.trees = 5000,mtry=4, importance= 'permutation', seed=1111)
RF

pred_ranger = predictions(predict(RF,data=base1,type = 'response'))
pred_ranger <- ifelse(pred_ranger== 1, 1, 0)

#Variable más importante
importance(RF)

round(importance(RF)/sum(importance(RF))*100,0)


# matriz de confusion train
mc_train5=addmargins(table(pred_ranger[train], base1$STATUS[train]))
mc_train5

# matriz de confusion test
mc_test5=addmargins(table(pred_ranger[test], base1$STATUS[test]))
mc_test5

# curva ROC 

roc_train5 <- roc(base1$STATUS[train], as.numeric(pred_ranger[train]))
roc_train5$auc

roc_test5 <- roc(base1$STATUS[test], as.numeric(pred_ranger[test]))
roc_test5$auc

#guardo las metricas en tablas
result_train5= data.frame(
  Modelo='RF',
  Sensitividad=c((mc_train5[2,2])/mc_train5[3,2]),
  Especificidad=c((mc_train5[1,1])/mc_train5[3,1]),
  Precision=c((mc_train5[1,1]+mc_train5[2,2])/mc_train5[3,3]),
  AUC=c(roc_train5$auc),
  F1=2*c((mc_train5[2,2])/(mc_train5[3,2])*((mc_train5[1,1])/mc_train5[3,1])/(mc_train5[2,2]/mc_train5[3,2]+(mc_train5[1,1])/mc_train5[3,1])),
  FPR=1-c((mc_train5[1,1])/mc_train5[3,1]),
  TP= mc_train5[2,2],
  TN= mc_train5[1,1],
  FP= mc_train5[2,1],
  FN= mc_train5[1,2]
)

result_test5= data.frame(
  Modelo='RF',
  Sensitividad=c((mc_test5[2,2])/mc_test5[3,2]),
  Especificidad=c((mc_test5[1,1])/mc_test5[3,1]),
  Precision=c((mc_test5[1,1]+mc_test5[2,2])/mc_test5[3,3]),
  AUC=c(roc_test5$auc),
  F1=2*c((mc_test5[2,2])/(mc_test5[3,2])*((mc_test5[1,1])/mc_test5[3,1])/(mc_test5[2,2]/mc_test5[3,2]+(mc_test5[1,1])/mc_test5[3,1])),
  FPR=1-c((mc_test5[1,1])/mc_test5[3,1]),
  TP= mc_test5[2,2],
  TN= mc_test5[1,1],
  FP= mc_test5[2,1],
  FN= mc_test5[1,2]
)
Resultados_forest1=rbind(Resultados_pred.tree2_1,result_train5)
Resultados_forest2=rbind(Resultados_pred.tree2_2,result_test5)

#####################################################################################################
###################################### boosting #####################################################

library(xgboost)

datos_train <- xgb.DMatrix(
  data  = data.matrix(base1[train,-1]),
  label = base1[train,1]
)

datos_test <- xgb.DMatrix(
  data  = data.matrix(base1[test,-1]),
  label = base1[test,1]
)

# para predecir todas las observaciones
datos <- xgb.DMatrix(
  data  = data.matrix(base1[,-1]),
  label = base1[,1]
)

set.seed(1111)
modelo = xgb.train(data = datos_train, params = list(eta = 0.1, objective='binary:hinge'), nrounds = 100) #100 interacciones, profundidad por defecto 6 nodos
modelo
pred_bosting = predict(modelo,newdata = datos)

#Variable más importante
xgb.importance(model = modelo)
sum(xgb.importance(model = modelo)$Gain)

# matriz de confusion train
mc_train6=addmargins(table(pred_bosting[train], base1$STATUS[train]))
mc_train6

# matriz de confusion test
mc_test6=addmargins(table(pred_bosting[test], base1$STATUS[test]))
mc_test6

# curva ROC 

roc_train6 <- roc(base1$STATUS[train], as.numeric(pred_bosting[train]))
roc_train6$auc

roc_test6 <- roc(base1$STATUS[test], as.numeric(pred_bosting[test]))
roc_test6$auc

#guardo las metricas en tablas
result_train6= data.frame(
  Modelo='Boosting',
  Sensitividad=c((mc_train6[2,2])/mc_train6[3,2]),
  Especificidad=c((mc_train6[1,1])/mc_train6[3,1]),
  Precision=c((mc_train6[1,1]+mc_train6[2,2])/mc_train6[3,3]),
  AUC=c(roc_train6$auc),
  F1=2*c((mc_train6[2,2])/(mc_train6[3,2])*((mc_train6[1,1])/mc_train6[3,1])/(mc_train6[2,2]/mc_train6[3,2]+(mc_train6[1,1])/mc_train6[3,1])),
  FPR=1-c((mc_train6[1,1])/mc_train6[3,1]),
  TP= mc_train6[2,2],
  TN= mc_train6[1,1],
  FP= mc_train6[2,1],
  FN= mc_train6[1,2]
)

result_test6= data.frame(
  Modelo='Boosting',
  Sensitividad=c((mc_test6[2,2])/mc_test6[3,2]),
  Especificidad=c((mc_test6[1,1])/mc_test6[3,1]),
  Precision=c((mc_test6[1,1]+mc_test6[2,2])/mc_test6[3,3]),
  AUC=c(roc_test6$auc),
  F1=2*c((mc_test6[2,2])/(mc_test6[3,2])*((mc_test6[1,1])/mc_test6[3,1])/(mc_test6[2,2]/mc_test6[3,2]+(mc_test6[1,1])/mc_test6[3,1])),
  FPR=1-c((mc_test6[1,1])/mc_test6[3,1]),
  TP= mc_test6[2,2],
  TN= mc_test6[1,1],
  FP= mc_test6[2,1],
  FN= mc_test6[1,2]
)
Resultados1=rbind(Resultados_forest1,result_train6)
Resultados2=rbind(Resultados_forest2,result_test6)

#Comparamos todos los resultados en Train y Test 
Resultados1
Resultados2

######################################################################################################
# Performance de los modelos
P=cbind(base1$STATUS,log_pred,log.step_pred,pred_tree1,pred_tree2, pred_ranger, pred_bosting)
colnames(P)=c('Y','RLog completa','RLog seleccion','Arbol','Arbol Podado', 'Random Forest','Boosting')
P=as.data.frame(P)

# error: será la tasa de los falsos positivos 
#debido a que hay una gran cantidad de No Default, 
#entonces nos centraremos en identificar correctamente a los clientes Default 
#(queremos tener mejor clasificado a los clientes malos). 
#Y para eso el error que tenemos que comparar es el de Tipo I. 
#Compararemos cual modelo se minimiza más en: 1-especificidad, o mejora la especificidad.














#############################################################################################################
###################################################################
##                OPTIMIZACION HIPERPARAMETROS                   ##
###################################################################

################################### RANDOM FOREST######################################################
#grilla con posibles valores 

opt = expand.grid(
  'num_trees' = c(50, 100, 500, 1000, 5000),
  'mtry'      = c(4, 6, 8, 10))

opt # 20 modelos a probar

oob_error = rep(NA, nrow(opt))

for(i in 1:nrow(opt)){
  
  modeloop <- ranger(
    formula   = STATUS ~ .,
    data      = base1[train,], 
    num.trees = opt$num_trees[i],
    mtry      = opt$mtry[i],
    seed      = 1111)
  
  oob_error[i] <- (modeloop$prediction.error)
}

# resultado de la optimizacion
oob_error
min(oob_error)                   # minimo
which.min(oob_error)             # cual es la fila del minimo
oob_error[which.min(oob_error)]  # control
opt[which.min(oob_error) ,]      # hiperparametros optimos num_trees=5000 y mtry=4


##########################################Boosting##########################################################

resultados_cv <- xgb.cv(
  data      = datos_train,
  params    = list(eta = 0.3, max_depth = 6, subsample = 1),
  nrounds   = 500,
  nfold     = 5,
  metrics   = "error", 
  verbose   = 0,
  seed      = 1111
)
resultados_cv$evaluation_log

min(resultados_cv$evaluation_log$test_error_mean)                   # minimo
which.min(resultados_cv$evaluation_log$test_error_mean)             # cantidad de arboles

plot.ts(resultados_cv$evaluation_log$test_error_mean)

# Al igual que Random Forest, vamos a crear una grilla de valores para los parametros e hiperparametros.

opt <- expand.grid(
  "tree_depth" = c(2, 4, 6),
  "learn_rate" = c(0.01, 0.1, 0.3),
  "sample_size" = c(0.5, 1)
)

opt

opt$tree = rep(NA, nrow(opt))
opt$cv = rep(NA, nrow(opt))

for(i in 1:nrow(opt)){   # demora mucho
  
  resultados_cv <- xgb.cv(
    data      = datos_train,
    params    = list(eta = opt$learn_rate[i], max_depth = opt$tree_depth[i], subsample = opt$sample_size[i]),
    nrounds   = 1000,
    nfold     = 5,
    metrics   = "error", 
    verbose   = 0,
    seed      = 1111
  )
  
  opt$cv[i] = min(resultados_cv$evaluation_log$test_error_mean) # minimo
  opt$tree[i]   = which.min(resultados_cv$evaluation_log$test_error_mean)# cantidad de arboles
  
}
opt
min(opt$cv)                   # minimo
which.min(opt$cv)             # cual es la fila del minimo
opt$cv[which.min(opt$cv)]     # control
opt[which.min(opt$cv),]       # hiperparametros optimos


