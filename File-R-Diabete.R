###### CREAZIONE DATI#######
options(scipen = 999, digits = 3)

#caricare il dataset

setwd("Y:\\My Drive\\DATA MINING\\DATASET ML")
file1 <- read.csv("diabetes_binary_health_indicators_BRFSS2015.csv", sep=",", dec = ".",  stringsAsFactors=TRUE, na.strings = "NA")


# SISTEMARE NATURA VARIABILI#
file1$Diabetes_binary<-factor(file1$Diabetes_binary)
file1$HighBP <-factor(file1$HighBP)
file1$HighChol<-factor(file1$HighChol)
file1$CholCheck<-factor(file1$CholCheck)
#file1$BMI<-numeric(file1$BMI)
file1$Smoker<-factor(file1$Smoker)
file1$Stroke<-factor(file1$Stroke)
file1$HeartDiseaseorAttack<-factor(file1$HeartDiseaseorAttack)
file1$PhysActivity<-factor(file1$PhysActivity)
file1$Fruits<-factor(file1$Fruits)
file1$Veggies<-factor(file1$Veggies)
file1$HvyAlcoholConsump<-factor(file1$HvyAlcoholConsump)
file1$AnyHealthcare<-factor(file1$AnyHealthcare)
file1$NoDocbcCost<-factor(file1$NoDocbcCost)
file1$GenHlth<-factor(file1$GenHlth)
#file1$MentHlth<-int(file1$MentHlth)
#file1$PhysHlth<-int(file1$PhysHlth)
file1$DiffWalk<-factor(file1$DiffWalk)
file1$Sex<-factor(file1$Sex)
file1$Age<-factor(file1$Age)
file1$Education<-factor(file1$Education)
file1$Income<-factor(file1$Income)
file1$Diabetes_binary<-factor(file1$Diabetes_binary, levels = c(1,0), labels=c('c1','c0'))
#ricorda c0= non diabetico"0"  c1=diabetico"1"
str(file1)


# ANALIZZO SE CLASSI SONO DISTRIBUITE EQUAMENTE NEI DATASET
table(file1$Diabetes_binary)

# column %
prop.table(table(file1$Diabetes_binary))

# controllo Missing #
library(VIM)
library(datasets)
sapply(file1, function(x)(sum(is.na(x))))
str(file1)

# controllo multicollinearita 
library(psych)
library(corrgram)
require(corrgram)
library(caret)

predictors=c('Diabetes_binary','HighBP','HighChol','CholCheck','BMI','Smoker','Stroke','HeartDiseaseorAttack','PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost','GenHlth','MentHlth','PhysHlth','DiffWalk','Sex','Age','Education','Income')
predictors_df <- file1[predictors]  # X variables
numeric <- sapply(predictors_df, function(x) is.numeric(x))
numeric <-predictors_df[, numeric]
more8=predictors_df$Diabetes_binary
numeric <-cbind(numeric, more8)

numeric <- sapply(predictors_df, function(x) is.numeric(x))
numeric <-predictors_df[, numeric]

correlatedPredictors = findCorrelation(cor(numeric), cutoff = 0.90, names = TRUE)
correlatedPredictors

# controllo zero variance o near zero variance
# nzv problema zero varaince o quasi zero variance se trovo TRUE in zero colonna va esclusa, near zero lascia piu margine di manovra  
# soglia conteggio calcolata come : freqRatio =( frequenze categoria piu frequente)/(numero conteggi seconda categoria piu frequente)
library(caret)
nzv = nearZeroVar(file1, saveMetrics = TRUE)
head(nzv[order(nzv$percentUnique, decreasing = FALSE), ], n = 20)
file1$Stroke<-NULL
file1$CholCheck<-NULL
file1$AnyHealthcare<-NULL



# creiamo dataset Validation e Training  e dataset per Step 4 
library(caret)
library(car)


# Dividi il dataset (70% training, 30% test)
set.seed(123)  # RiproducibilitÃ  dividere proporzione nei gruppi

#creazione DATASET step 4

Dati_partizionati <- createDataPartition(file1$Diabetes_binary, p = 0.8, list = FALSE)

# Dati per creare e controllare il modello
Dati <- file1[Dati_partizionati, ]

# Dati per controllo Finale Step_4
dati_step_4 <- file1[-Dati_partizionati, ]


train_indices <- createDataPartition(Dati$Diabetes_binary, p = 0.7, list = FALSE)

# Training set
dati_training <- Dati[train_indices, ]

# Training set mini
set.seed(123)

dati_PROVA_TRAINING <- createDataPartition(dati_training$Diabetes_binary, p = 0.98, list = FALSE)
datamin_training <- dati_training[-dati_PROVA_TRAINING, ]

# Validation set
dati_validation <- Dati[-train_indices, ]

# Validation set mini
set.seed(123)
dati_PROVA_VALIDATION <- createDataPartition(dati_validation$Diabetes_binary, p = 0.98, list = FALSE)
datamin_validation <- dati_validation[-dati_PROVA_VALIDATION, ]


#####  MODELSELECTION BORUTA ####
library(Boruta)
library(caret)

set.seed(123)

# Applicazione di Boruta per la selezione delle variabili
boruta_output <- Boruta(
  Diabetes_binary ~ .,              # Formula: variabile dipendente contro tutte le altre
  data = file1,    # Dataset di input
  doTrace = 2,             # Livello di logging (2 fornisce dettagli)
  maxRuns = 100            # Numero massimo di iterazioni
)

# Risultati del modello Boruta
print(boruta_output)

# Variabili confermate, eliminate e tentativamente rilevanti
final_vars <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(final_vars)

# Visualizzazione grafica
plot(boruta_output, las = 2, main = "Boruta Variable Importance")

# Creazione di un dataset ridotto basato sulle variabili selezionate
MODEL_SELECTION <- file1[, c(final_vars, "Diabetes_binary")]

# Controllo del dataset ridotto
str(MODEL_SELECTION)


#creazione partizion
Dati_partizionati_Model_Selection <- createDataPartition(MODEL_SELECTION$Diabetes_binary, p = 0.8, list = FALSE)
Dati_Model_Selection <- file1[Dati_partizionati_Model_Selection, ]

# Dati per controllo Finale Step_4
dati_step_4_Model_Selection <- file1[-Dati_partizionati_Model_Selection, ]

train_indices_MS <- createDataPartition(Dati_Model_Selection$Diabetes_binary, p = 0.7, list = FALSE)


# Training set
dati_training_MS <- Dati_Model_Selection[train_indices_MS, ]

# Validation set
dati_validation_MS <- Dati_Model_Selection[-train_indices_MS, ]






setwd("Y:\\My Drive\\DATA MINING\\MACHINE LEARNING")
load("ELABORATO_ML_MD&BORUTA.RData")







#### REGRESSIONE LOGISTICA ####
set.seed(1234)
#con cv:
#Control=trainControl(method= "cv",number=5,classProbs = TRUE,summaryFunction=twoClassSummary)
#glmPP=train(Diabetes_binary~.,data=dati_training , method = "glm", preProcess=c( "corr", "nzv"),
#            trControl = Control, tuneLength=5, trace=FALSE,na.action = na.pass,metric='Sens')
#confusionMatrix(glmPP)

#senza cv:
Control=trainControl(method= "none",classProbs = TRUE,summaryFunction=twoClassSummary)
glmPP=train(Diabetes_binary~.,data=dati_training , method = "glm", preProcess=c( "corr", "nzv"),
            trControl = Control, tuneLength=1, trace=FALSE,na.action = na.pass,metric='Sens')

# Ottenere le predizioni sul training set
predictions <- predict(glmPP, newdata = dati_training, type = "raw")  # "raw" per etichette di classe

# Calcolare la matrice di confusione
conf_matrix <- confusionMatrix(
  as.factor(predictions),  # Predizioni come fattore
  as.factor(dati_training$Diabetes_binary)  # Valori reali come fattore
)

# Stampare la matrice di confusione
print(conf_matrix)


#OVERFITTING
library(pROC)
# Previsioni sui dati di training
train_PRED <- predict(glmPP, newdata = dati_training, type = "prob")
train_PRED_prob_c0 <- train_PRED[, "c0"]  
glmPP_ROC_Train <- roc(dati_training$Diabetes_binary, train_PRED_prob_c0)
glmPP_AUC_Train <- auc(glmPP_ROC_Train)
# Previsioni sui dati di validation
validation_PRED <- predict(glmPP, newdata = dati_validation, type = "prob")
validation_PRED_prob_c0 <- validation_PRED[, "c0"]  
glmPP_ROC_Validation <- roc(dati_validation$Diabetes_binary, validation_PRED_prob_c0)
glmPP_AUC_Validation <- auc(glmPP_ROC_Validation)
print(glmPP_ROC_Train)
print(glmPP_ROC_Validation)
print(glmPP_AUC_Train)
print(auc_validation)
# NON OVERFITTTANO 


#### REGRESSIONE LOGISTICA LASSO ####

set.seed(123)

#con cv:
#grid = expand.grid(.alpha=1,.lambda=seq(0, 1, by = 0.01))
#Control=trainControl(method= "cv",number=5, classProbs=TRUE,summaryFunction=twoClassSummary)
#lassoPP=train(Diabetes_binary~.,data=dati_training , method = "glmnet", family ="binomial",
#              trControl = Control, tuneLength=5, tuneGrid=grid,metric='Sens')
#confusionMatrix(lassoPP)

#senza cv:
grid = expand.grid(.alpha=1, .lambda=0.01)
Control=trainControl(method= "none", classProbs=TRUE,summaryFunction=twoClassSummary)
lassoPP=train(Diabetes_binary~.,data=dati_training , method = "glmnet", family ="binomial",
              trControl = Control, tuneGrid=grid,metric='Sens')

predictions <- predict(lassoPP, newdata = dati_training, type = "raw")
conf_matrix <- confusionMatrix(predictions, as.factor(dati_training$Diabetes_binary))
print(conf_matrix)

#OVERFITTING Lasso
library(pROC)
# Previsioni sui dati di training
train_PRED <- predict(lassoPP, newdata = dati_training, type = "prob")
train_PRED_prob_c0 <- train_PRED[, "c0"]  
lassoPP_ROC_Train <- roc(dati_training$Diabetes_binary, train_PRED_prob_c0)
lassoPP_AUC_Train <- auc(lassoPP_ROC_Train)
# Previsioni sui dati di validation
validation_PRED <- predict(lassoPP, newdata = dati_validation, type = "prob")
validation_PRED_prob_c0 <- validation_PRED[, "c0"]  
lassoPP_ROC_Validation <- roc(dati_validation$Diabetes_binary, validation_PRED_prob_c0)
print(lassoPP_ROC_Train)
print(lassoPP_ROC_Validation)

lassoPP_AUC_Validation <- auc(lassoPP_ROC_Validation)
print(lassoPP_AUC_Train)
print(lassoPP_AUC_Validation)
# NON OVERFITTTANO 




#### PLS ####
set.seed(1234)

#con cv:
#preProc <- c("center", "scale")
#Control=trainControl(method= "cv",number=5, classProbs=TRUE,summaryFunction=twoClassSummary)
#plsPP=train(Diabetes_binary~.,data=dati_training_MS , method = "pls",  preProcess = preProc,
#            trControl = Control, tuneLength=5,metric='Sens')

#confusionMatrix(plsPP)


#senza cv:
preProc <- c("center", "scale")
Control=trainControl(method= "none", classProbs=TRUE,summaryFunction=twoClassSummary)
tuneGrid = expand.grid(.ncomp = 3)
plsPP=train(Diabetes_binary~.,data=dati_training_MS , method = "pls",  preProcess = preProc,
            trControl = Control, tuneGrid=tuneGrid,metric='Sens')

predictions <- predict(plsPP, newdata = dati_training_MS, type = "raw")
conf_matrix <- confusionMatrix(predictions, as.factor(dati_training_MS$Diabetes_binary))
print(conf_matrix)

#OVERFITTING PLS
library(pROC)
# Previsioni sui dati di training
train_PRED <- predict(plsPP, newdata = dati_training_MS, type = "prob")
train_PRED_prob_c0 <- train_PRED[, "c0"]  
plsPP_ROC_train <- roc(dati_training_MS$Diabetes_binary, train_PRED_prob_c0)
# Previsioni sui dati di validation
validation_PRED <- predict(plsPP, newdata = dati_validation_MS, type = "prob")
validation_PRED_prob_c0 <- validation_PRED[, "c0"]  
plsPP_ROC_validation <- roc(dati_validation_MS$Diabetes_binary, validation_PRED_prob_c0)
print(plsPP_ROC_train)
print(plsPP_ROC_validation)
plsPP_AUC_train <- auc(plsPP_ROC_train)
plsPP_AUC_validation <- auc(plsPP_ROC_validation)
print(plsPP_AUC_train)
print(plsPP_AUC_validation)
# NON OVERFITTTANO 




#### NAIVE BAYES #### 
#con cv
#set.seed(1234)
#preProc <- c("nzv", "scale","corr")
#Control=trainControl(method= "cv",number=5, classProbs=TRUE,summaryFunction=twoClassSummary)
#Naive_BayesPP=train(Diabetes_binary~.,data=dati_training , method = "nb", family ="binomial",
#                    trControl = Control, tuneLength=5,na.action = na.exclude,metric='Sens')
#
#confusionMatrix(Naive_BayesPP)

#senza cv: 

set.seed(1234)
preProc <- c("nzv", "scale","corr")

tuneGrid = expand.grid(
  fL = 0,       # Laplace smoothing 
  usekernel = TRUE,  # Usa densitÃ  kernel
  adjust = 1         # Regola la larghezza del kernel
)

Control=trainControl(method= "none", classProbs=TRUE,summaryFunction=twoClassSummary)
Naive_BayesPP=train(Diabetes_binary~.,data=dati_training , method = "nb",preProcess=preProc,family ="binomial",
                    trControl = Control, tuneGrid=tuneGrid,na.action = na.exclude,metric='Sens')

predictions <- predict(Naive_BayesPP, newdata = dati_training, type = "raw")
conf_matrix <- confusionMatrix(predictions, as.factor(dati_training$Diabetes_binary))
print(conf_matrix)


#OVERFITTING NAIVE BAYES
library(pROC)
# Previsioni sui dati di training
train_PRED <- predict(Naive_BayesPP, newdata = dati_training, type = "prob")
train_PRED_prob_c0 <- train_PRED[, "c0"]  
Naive_BayesPP_roc_train <- roc(dati_training$Diabetes_binary, train_PRED_prob_c0)
# Previsioni sui dati di validation
validation_PRED <- predict(Naive_BayesPP, newdata = dati_validation, type = "prob")
validation_PRED_prob_c0 <- validation_PRED[, "c0"]  
Naive_BayesPP_roc_validation <- roc(dati_validation$Diabetes_binary, validation_PRED_prob_c0)
print(Naive_BayesPP_roc_train)
print(Naive_BayesPP_roc_validation)
Naive_BayesPP_AUC_train <- auc(Naive_BayesPP_roc_train)
Naive_BayesPP_AUC_validation <- auc(Naive_BayesPP_roc_validation)
print(Naive_BayesPP_AUC_train)
print(Naive_BayesPP_AUC_validation)
# NON OVERFITTA



#### K NEAREST EIGHBORS ####
#con cv:
set.seed(1234)
#grid = expand.grid(.k      =seq(1,20, by=3)               )
#preProc <- c("center", "scale","corr")
#Control=trainControl(method= "cv",number=5, classProbs=TRUE,summaryFunction=twoClassSummary)
#knn_modelPP=train(Diabetes_binary~.,data=dati_training_MS , method = "knn",preProcess = preProc,
#                  trControl = Control, tuneLength=5, tuneGrid=grid, na.action = na.exclude,metric='Sens')

#confusionMatrix(knn_modelPP)


#senza cv

library(Boruta)
library(caret)

set.seed(1234)
grid = expand.grid(.k=5)
preProc <- c("center", "scale","corr")
Control=trainControl(method= "none", classProbs=TRUE,summaryFunction=twoClassSummary)

knn_modelPP=train(Diabetes_binary~.,data=dati_training_MS , method = "knn",preProcess = preProc,
                  trControl = Control, tuneGrid=grid, na.action = na.exclude,metric='Sens')

predictions <- predict(knn_modelPP, newdata = dati_training_MS, type = "raw")
conf_matrix <- confusionMatrix(predictions, as.factor(dati_training_MS$Diabetes_binary))
print(conf_matrix)


#OVERFITTING knn
library(pROC)
# Previsioni sui dati di training
train_PRED <- predict(knn_modelPP, newdata = dati_training_MS, type = "prob")
train_PRED_prob_c0 <- train_PRED[, "c0"]  
knn_modelPP_ROC_train <- roc(dati_training_MS$Diabetes_binary, train_PRED_prob_c0)
# Previsioni sui dati di validation
validation_PRED <- predict(knn_modelPP, newdata = dati_validation_MS, type = "prob")
validation_PRED_prob_c0 <- validation_PRED[, "c0"]  
knn_modelPP_ROC_validation <- roc(dati_validation_MS$Diabetes_binary, validation_PRED_prob_c0)
print(knn_modelPP_ROC_train)
print(knn_modelPP_ROC_validation)
knn_modelPP_AUC_train <- auc(knn_modelPP_ROC_train)
knn_modelPP_AUC_validation <- auc(knn_modelPP_ROC_validation)
print(knn_modelPP_AUC_train)
print(knn_modelPP_AUC_validation)
# c'Ã¨ OVERFITTING


#### ADA BOOST ####
#install.packages("adabag", dependencies = TRUE) GIA INSTALLATO 
library((caret))
install.packages("C:\\Users\\e.nada\\Downloads\\fastAdaboost_1.0.0.tar.gz", repos = NULL, type = "source")
# Definizione del controllo 
control <- trainControl(
  method = "none",            
  summaryFunction = twoClassSummary,  # Per metriche come AUC, Sensitivity, Specificity
  classProbs = TRUE           # Richiesto per classificazione binaria
)

tunegrid <- expand.grid(nIter = 50,method = "adaboost")

set.seed(123)
ada_modelPP <- train(
  Diabetes_binary ~ .,        
  data = dati_training,       # Usa solo il training set
  method = "adaboost",        # Metodo AdaBoost
  metric = "ROC",             # Usa ROC come metrica principale
  trControl = control, 
  tuneLength = 2,              # Ricerca automatica dei migliori iperparametri
  tuneGrid = tunegrid  
)

predictions <- predict(ada_model, newdata = dati_training)
conf_matrix <- table(predictions, dati_training$Diabetes_binary)
print(conf_matrix)

#OVERFITTING ADA Boost
library(pROC)
# Previsioni sui dati di training
train_PRED <- predict(ada_modelPP, newdata = dati_training, type = "prob")
train_PRED_prob_c0 <- train_PRED[, "c0"]  
ada_model_ROC_train <- roc(dati_training$Diabetes_binary, train_PRED_prob_c0)
# Previsioni sui dati di validation
validation_PRED <- predict(ada_model, newdata = dati_validation, type = "prob")
validation_PRED_prob_c0 <- validation_PRED[, "c0"]  
ada_model_ROC_validation <- roc(dati_validation$Diabetes_binary, validation_PRED_prob_c0)
print(ada_model_ROC_train)
print(ada_model_ROC_validation)
ada_model_AUC_train <- auc(ada_model_ROC_train)
ada_model_AUC_validation <- auc(ada_model_ROC_validation)
print(ada_model_AUC_train)
print(ada_model_AUC_validation)

#### ABERO DECISIONALE  ####
library(rpart)
library(rpart.plot)
#set.seed(123)  
#AlberoPP<- rpart(Diabetes_binary ~ ., data = MODEL_SELECTION, method = "class")
#rpart.plot(Albero, type = 4, extra = 101,  split.font = 0.9, ycompress=FALSE, cex=.7)

#Prediction_alberoPP <- predict(AlberoPP, MODEL_SELECTION, type = "class")
#conf_matrix <- confusionMatrix(Prediction_alberoPP, MODEL_SELECTION$Diabetes_binary)
#print(conf_matrix)


#control <- trainControl(method = "cv", number = 5,summaryFunction = twoClassSummary, classProbs = TRUE)
#trControl = control,        # Controllo della cross-validation
# Addestramento del modello ad albero
albero <- train(
  Diabetes_binary ~ .,        # Variabile target
  data = dati_training,       # Dataset di training
  method = "rpart",           # Metodo per alberi decisionali
  tuneLength = 10             # Ricerca automatica dei migliori iperparametri
)

#OVERFITTING Albero
library(pROC)
# Previsioni sui dati di training
train_PRED <- predict(albero, newdata = dati_training, type = "prob")
train_PRED_prob_c0 <- train_PRED[, "c0"]  
albero_ROC_train <- roc(dati_training$Diabetes_binary, train_PRED_prob_c0)
# Previsioni sui dati di validation
validation_PRED <- predict(albero, newdata = dati_validation, type = "prob")
validation_PRED_prob_c0 <- validation_PRED[, "c0"]  
albero_ROC_validation <- roc(dati_validation$Diabetes_binary, validation_PRED_prob_c0)
print(albero_ROC_train)
print(albero_ROC_validation)
albero_AUC_train <- auc(albero_ROC_train)
albero_AUC_validation <- auc(albero_ROC_validation)
print(albero_AUC_train)
print(albero_AUC_validation)
# Possibile Overfitting 




#### RANDOM FOREST ####
#con cv
#library(caret)
#str(dati_training)
#set.seed(123)
#control <- trainControl(method="cv", number=10, search="grid", summaryFunction = twoClassSummary, classProbs = TRUE)
#trControl=control
#tunegrid <- expand.grid(.mtry=c(1:5))
#FORESTPP <- train(Diabetes_binary~., data=dati_training, method="rf", metric="Sens", tuneGrid=tunegrid, ntree=100)

#confusionMatrix(FORESTPP)
#getTrainPerf(FORESTPP)

#sensza cv
library(caret)
set.seed(123)

control <- trainControl(method = "none", classProbs = TRUE, summaryFunction = twoClassSummary)

# Definizione dei parametri della Random Forest
tunegrid <- expand.grid(.mtry = 3)  # Scegli un valore fisso per evitare tuning

# Addestramento del modello Random Forest SENZA CV
FORESTPP <- train(Diabetes_binary ~ ., 
                  data = dati_training, 
                  method = "rf", 
                  metric = "Sens", 
                  tuneGrid = tunegrid, 
                  trControl = control, 
                  ntree = 100)

# Valutazione delle prestazioni
predictions <- predict(FORESTPP, newdata = dati_training)
conf_matrix <- confusionMatrix(predictions, as.factor(dati_training$Diabetes_binary))

print(conf_matrix)
#getTrainPerf(FORESTPP) non so cosa faccia




#OVERFITTING Random Forest
library(pROC)
# Previsioni sui dati di training
train_PRED <- predict(FORESTPP, newdata = dati_training, type = "prob")
train_PRED_prob_c0 <- train_PRED[, "c0"]  
FORESTPP_ROC_train <- roc(dati_training$Diabetes_binary, train_PRED_prob_c0)
# Previsioni sui dati di validation
validation_PRED <- predict(FORESTPP, newdata = dati_validation, type = "prob")
validation_PRED_prob_c0 <- validation_PRED[, "c0"]  
FORESTPP_ROC_validation <- roc(dati_validation$Diabetes_binary, validation_PRED_prob_c0)
print(FORESTPP_ROC_train)
print(FORESTPP_ROC_validation)
FORESTPP_AUC_train <- auc(FORESTPP_ROC_train)
FORESTPP_AUC_validation <- auc(FORESTPP_ROC_validation)
print(FORESTPP_AUC_train)
print(FORESTPP_AUC_validation)
# ?


#### RETI NEURALI ####
#con cv
#Grid0 = expand.grid(.size   =seq(1,7,  by=1), .decay = 0.1 )

#set.seed(123) # mette come parametro di fit casuale 7 nell esmpio dice di provare con diversi "semi" numeri per vedere se modello migliora o peggiora
#control = trainControl(method="cv", number=5, search = "grid",summaryFunction=twoClassSummary)
#nnetPP <- train(dati_training_MS[-1], dati_training_MS$Diabetes_binary,
#                method = "nnet",
#                tuneGrid=Grid0,
#                preProcess=c("scale","corr","nzv"), 
#                metric="Sens", trControl=control,
#                trace = TRUE, # uso per vedere se converge se non converge provare ad aumentare numero max iterazioni
#                maxit = 300) # numero massimo di iterazioni

#plot(nnetPP)
#getTrainPerf(nnetPP) 

#confusionMatrix(nnetPP)

#senza cv
Grid0 = expand.grid(.size = 5, .decay = 0.1)  # Un solo valore

set.seed(123) 
control = trainControl(method = "none", classProbs = TRUE, summaryFunction = twoClassSummary)

nnetPP <- train(dati_training_MS[-1], dati_training_MS$Diabetes_binary,
                method = "nnet",
                tuneGrid = Grid0,
                preProcess = c("scale", "corr", "nzv"), 
                metric = "Sens", 
                trControl = control,
                trace = TRUE, 
                maxit = 300)


getTrainPerf(nnetPP) 

# Genera le predizioni sul training set
predictions <- predict(nnetPP, newdata = dati_training_MS, type = "raw")
conf_matrix <- confusionMatrix(predictions, as.factor(dati_training_MS$Diabetes_binary))
print(conf_matrix)



#OVERFITTING Reti neurali
library(pROC)
# Previsioni sui dati di training
train_PRED <- predict(nnetPP, newdata = dati_training_MS, type = "prob")
train_PRED_prob_c0 <- train_PRED[, "c0"]  
nnetPP_ROC_train <- roc(dati_training_MS$Diabetes_binary, train_PRED_prob_c0)
# Previsioni sui dati di validation
validation_PRED <- predict(nnetPP, newdata = dati_validation_MS, type = "prob")
validation_PRED_prob_c0 <- validation_PRED[, "c0"]  
nnetPP_ROC_validation <- roc(dati_validation_MS$Diabetes_binary, validation_PRED_prob_c0)
print(nnetPP_ROC_train)
print(nnetPP_ROC_validation)
nnetPP_AUC_train <- auc(nnetPP_ROC_train)
nnetPP_AUC_validation <- auc(nnetPP_ROC_validation)
print(nnetPP_AUC_train)
print(nnetPP_AUC_validation)
#Non c'è overfitting 

#### GRADIENT BOOSTING ####
set.seed(123)
control<- trainControl( method="cv", number=5,search="grid",summaryFunction=twoClassSummary,classProbs = TRUE)
gradien_boostingPP <- train(Diabetes_binary~ ., data=dati_training, method = "gbm", trControl = control)
confusionMatrix(gradien_boostingPP)

#OVERFITTING Gradient Boosting
library(pROC)
# Previsioni sui dati di training
train_PRED <- predict(gradien_boostingPP, newdata = dati_training, type = "prob")
train_PRED_prob_c0 <- train_PRED[, "c0"]  
gradien_boostingPP_roc_train <- roc(dati_training$Diabetes_binary, train_PRED_prob_c0)
# Previsioni sui dati di validation
validation_PRED <- predict(gradien_boostingPP, newdata = dati_validation, type = "prob")
validation_PRED_prob_c0 <- validation_PRED[, "c0"]  
gradien_boostingPP_roc_validation <- roc(dati_validation$Diabetes_binary, validation_PRED_prob_c0)
print(gradien_boostingPP_roc_train)
print(gradien_boostingPP_roc_validation)
gradien_boostingPP_auc_train <- auc(gradien_boostingPP_roc_train)
gradien_boostingPP_auc_validation <- auc(gradien_boostingPP_roc_validation)
print(gradien_boostingPP_auc_train)
print(gradien_boostingPP_auc_validation)

###### compara risutati ###### SOLO CON IL CV
results <- resamples(list(glm_PreProc=glmPP,plsPP=plsPP,lasso_PP=lassoPP,   Naive_Bayes_PP=Naive_BayesPP,knn_PP=knn_modelPP,Ada_Boost_PP=ada_model,albero_PP=albero,nnet_pp=nnetPP,RanfomForest_PP=FORESTPP))  
bwplot(results)



############STEP 2#############
plot(glmPP)
plot(plsPP,add=T,col="orange")
plot(lasso_PP,add=T,col="brown")
plot(Naive_Bayes,add=T,col="green")
plot(knn_model,add=T,col="violet")
plot(rf_model,add=T,col="lightgreen")
plot(ada_model,add=T,col="purple")
plot(albero,add=T,col="lightblue")
plot(ada_model,add=T,col="yellow")
plot(FORESTPP,add=T,col="red")
plot(nnetPP,add=T,col="blue")



library(pROC)
plot(roc_glmPP, col="blue", main="Confronto Curve ROC", lwd=2)
plot(roc_plsPP, add=TRUE, col="orange", lwd=2)
plot(roc_lasso, add=TRUE, col="brown", lwd=2)
plot(roc_naive_bayes, add=TRUE, col="green", lwd=2)
plot(roc_knn, add=TRUE, col="violet", lwd=2)
plot(roc_rf, add=TRUE, col="lightgreen", lwd=2)
plot(roc_ada, add=TRUE, col="purple", lwd=2)
plot(roc_albero, add=TRUE, col="lightblue", lwd=2)
plot(roc_forestpp, add=TRUE, col="red", lwd=2)
plot(roc_nnetPP, add=TRUE, col="blue", lwd=2)
#confronto reti con lase:
data(destdf)


gdata(testDF)
glmModel <- glm(y ~ ., data = testDF, family="binomial")
Preds <- predict(glmModel, type = 'response')   
#var dipendente deve essere binaria numerica 0,1.

https://cran.r-project.org/web/packages/ModelMetrics/ModelMetrics.pdf


##### MATRICE DI COSTI ######

library(ROCR)
pred_r0 <- prediction(df_cv$Prob0,y)
# this step do nothing, useful only to have an ROCR object..giving us all metric when varying threshold


# roc curve
roc.perf = performance(pred_r0, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)

# spec metrics (y.values) when varying threshold (x.values) ######
performance(pred_r0,measure="spec")
plot(performance(pred_r0,measure="spec"))


# FA IN AUTOMATICO, VEDREMO CHE LA SOGLIA MIGLIORE SARÃ  SIMILE A QUELLA TROVATA SOPRA. 

pred_r0 <- prediction(df_cv$Prob0,y)
# remember     y=ifelse(y=="r0",1,0)

# which is the best cutoff for a prob P(y=1) = P(lossfreq=r0) which minimize total costs?????
# which is the best cutoff for a prob P(y=1) = P(lossfreq=r0) which minimize total costs?????
library(ROCR)
cost.perf = performance(pred_r0, "cost", cost.fp = 20, cost.fn = 1)
pred_r0@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]


####MATRICE DI PROFITTI FITTIZZI:####

probTr=predict(knn_grid_c,newdata=train, type = "prob")
head(probTr)

# HERE WE  use a different prediction rule: decision maximizing profit##########
# HERE WE  use a different prediction rule: decision maximizing profit


# 1. specify a profit matrix##########
profitMatrix <- ifelse(diag(4) == 1, 0, 1)
profitMatrix[4, 4] <- 150
profitMatrix[3, 3] <- 70
profitMatrix[2, 2] <- 20
profitMatrix[1, 1] <- 1

profitMatrix   
rownames(profitMatrix) <- colnames(profitMatrix) <- levels(train$LOSSFRQ)
profitMatrix # nb is a profit matrix

# new element: individual profit (EP) of each decision##########

# EP = Individual Predictions weighted by element of the profitMatrix  product of two matrices
# EP values i*j are individual profit for subject i of decision in class j: 
# EP(ij)=  prob(y_ij)*(profit matrix column j for row i)

profitMatrix=as.matrix(profitMatrix)
probTr=as.matrix(probTr)    # individual probability to be each class j : p(xj)

EP=data.frame(probTr%*%profitMatrix)
head(EP)
# each row has EP(r0) EIP(r1), EP(r2), EP(r3) profit to classify in one of 4 values this subject##########

# Hence, find a decision  based on max of individual profit (EP) of each decision 
# Example:   y predicted as r1   if EP(r1) > EP(r0), EP(r2), EP(r3) 


# find max of each row (decision based on max profit)##########
zz=data.frame(names(EP)[apply(EP, 1, which.max)] )
colnames(zz)="predict"
# predict is the target class maximizing profit

EP_and_decision=cbind(EP,zz)
head(EP_and_decision,n=10)

##### SPIEGARE LA RELAZIONE TRA COVARIATE E TARGET NELLE BLACK BOX ######
#####COME STIMARE IN CARET UN MODELLO CREATO IN UN ALTRO MODO #####

#lets refit in caret a model already estimated by other packages
seed <- 7
set.seed(seed)
metric <- "Sens"
control <- trainControl(method="none", summaryFunction = twoClassSummary, classProbs = TRUE)
fit_final <- train(u~., data=train.df, method="rf", metric=metric, trControl=control, .mtry=1, ntree=200)

fit_final

Vimportance <- varImp(fit_final)
plot(Vimportance) # HA POCO SENSO, NON SAPPIAMO SE Ã¨ UNA CRESCIAT O DECRESCITA DEL TARGET. 

# performance on test set
pred=predict(fit_final, test.df)
confusionMatrix(as.factor(pred), as.factor(test.df$u))
