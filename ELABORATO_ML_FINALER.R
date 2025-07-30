##### EXTRA DATASET CON PRE-DIABETE ####
file2<-read.csv("diabetes_012_health_indicators_BRFSS2015.csv",sep=";",na.strings=c("NA","NaN", ""))
file1 <- subset(file2, Diabetes_012 != 1)
file1$Diabetes_binary<-factor(file1$Diabetes_binary, levels = c(2,0), labels=c('c1','c0'))
file1$Diabetes_012<-NULL



###### CREAZIONE DATI#######


#caricare il dataset

setwd("PERCORSO DEL FILE")
file1 <- read.csv("diabetes_binary_health_indicators_BRFSS2015_clean.csv", sep=";",na.strings=c("NA","NaN", ""))
options(scipen = 999, digits = 3)

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
set.seed(123)  # Riproducibilità dividere proporzione nei gruppi

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

dati_PROVA_TRAINING <- createDataPartition(dati_training$Diabetes_binary, p = 0.93, list = FALSE)
datamin_training <- dati_training[-dati_PROVA_TRAINING, ]

# Validation set
dati_validation <- Dati[-train_indices, ]

# Validation set mini
set.seed(123)
dati_PROVA_VALIDATION <- createDataPartition(dati_validation$Diabetes_binary, p = 0.93, list = FALSE)
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

# 18 variabili importanti le tiene tutte nessuna diffrenza tra boruta e modelllo normale   "HighBP"  "HighChol" "BMI" "Smoker" "HeartDiseaseorAttack" "PhysActivity"  "Fruits"  "Veggies"  "HvyAlcoholConsump" "NoDocbcCost" "GenHlth"   "MentHlth" "PhysHlth" "DiffWalk" "Sex"  "Age" "Education" "Income" 
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



#### REGRESSIONE LOGISTICA ####
set.seed(1234)
summary(glmPP)
#dati_training_MS$Age <- relevel(dati_training_MS$Age,13) # scelta variabile dummy di confronto 

#con cv:
Control=trainControl(method= "cv",number=5,classProbs = TRUE,summaryFunction=twoClassSummary)
glmPP=train(Diabetes_binary~.,data=dati_training_MS , method = "glm", preProcess=c( "corr", "nzv"),
            trControl = Control, tuneLength=5, trace=FALSE,na.action = na.pass,metric='Sens')
confusionMatrix(glmPP)
#senza cv:
Control=trainControl(method= "none",classProbs = TRUE,summaryFunction=twoClassSummary)
glmPP=train(Diabetes_binary~.,data=dati_training_MS , method = "glm", preProcess=c( "corr", "nzv"),
            trControl = Control, tuneLength=1, trace=FALSE,na.action = na.pass,metric='Sens')

confusionMatrix(glmPP,data = dati_training)
#OVERFITTING
library(pROC)
# Previsioni sui dati di training
train_PRED_logist <- predict(glmPP, newdata = dati_training, type = "prob")
train_PRED_prob_c1_logist <- train_PRED_logist[, "c1"]  # dico che fattore negativo e c0
roc_train_logist <- roc(dati_training$Diabetes_binary, train_PRED_prob_c1_logist)
# Previsioni sui dati di validation
validation_PRED_logist <- predict(glmPP, newdata = dati_validation, type = "prob")
validation_PRED_prob_c1_logist <- validation_PRED_logist[, "c1"]  
roc_validation_logist <- roc(dati_validation$Diabetes_binary, validation_PRED_prob_c1_logist)
auc_train_logist <- auc(roc_train_logist)
auc_validation_logist <- auc(roc_validation_logist)
print(roc_train_logist)
print(roc_validation_logist)
print(auc_train_logist)
print(auc_validation_logist)
overfitting_logist=(auc_train_logist-auc_validation_logist)/auc_train_logist
print(overfitting_logist)
# NON OVERFITTTANO 


#### REGRESSIONE LOGISTICA LASSO ####
set.seed(123)
lassoPP$bestTune
lassoPP$results
#dati_training$Age <- relevel(dati_training$Age,13)
#con cv:
grid = expand.grid(.alpha=1,.lambda=seq(0, 1, by = 0.01))
Control=trainControl(method= "cv",number=5, classProbs=TRUE,summaryFunction=twoClassSummary)
lassoPP=train(Diabetes_binary~.,data=dati_training , method = "glmnet", family ="binomial",
              trControl = Control, tuneLength=5,preProcess=c("scale") ,tuneGrid=grid,metric='Sens')
confusionMatrix(lassoPP)

#senza cv:
grid = expand.grid(.alpha=1, .lambda=0.01)
Control=trainControl(method= "none", classProbs=TRUE,summaryFunction=twoClassSummary)
lassoPP=train(Diabetes_binary~.,data=dati_training , method = "glmnet", family ="binomial",
              trControl = Control, tuneGrid=grid,metric='Sens')

predictions_lasso <- predict(lassoPP, newdata = dati_training, type = "prob")
conf_matrix_lasso <- confusionMatrix(predictions_lasso, as.factor(datamin_training$Diabetes_binary))
print(conf_matrix_lasso)

 
#OVERFITTING Lasso
library(pROC)
# Previsioni sui dati di training
train_PRED_lasso <- predict(lassoPP, newdata = dati_training, type = "prob")
train_PRED_prob_c1_lasso <- train_PRED_lasso[, "c1"]  
roc_train_lasso <- roc(dati_training$Diabetes_binary, train_PRED_prob_c1_lasso)
# Previsioni sui dati di validation
validation_PRED_lasso <- predict(lassoPP, newdata = dati_validation, type = "prob")
validation_PRED_prob_c1_lasso <- validation_PRED_lasso[, "c1"]  
roc_validation_lasso <- roc(dati_validation$Diabetes_binary, validation_PRED_prob_c1_lasso)
print(roc_train_lasso)
print(roc_validation_lasso)
auc_train_lasso <- auc(roc_train_lasso)
auc_validation_lasso <- auc(roc_validation_lasso)
print(auc_train_lasso)
print(auc_validation_lasso)
overfitting_lasso=(auc_train_lasso-auc_validation_lasso)/auc_train_lasso
print(overfitting_lasso)
# NON OVERFITTTANO 



# Caricamento dei pacchetti necessari
library(caret)
library(glmnet)
library(dplyr)  # Per la manipolazione dei dati

# Estrazione dei coefficienti del miglior modello selezionato
best_lambda <- lassoPP$bestTune$lambda  # Miglior valore di lambda scelto dalla CV

lasso_coeffs_best = coef(lassoPP$finalModel, s = best_lambda)
print(lasso_coeffs_best)

lasso_coeffs <- as.matrix(coef(lassoPP$finalModel, s = best_lambda))  # Coefficienti
Coefficient = lasso_coeffs[,1]

# Creazione del dataframe con i nomi delle variabili e i coefficienti
coeff_df <- data.frame(Feature = rownames(lasso_coeffs), Coefficient = lasso_coeffs[,1])
coeff_df
# Ordinare per valore assoluto del coefficiente
coeff_df <- coeff_df %>% arrange(desc(abs(Coefficient)))
print(Coefficient)

# Stampare i coefficienti ordinati
print(coeff_df)

# Grafico a barre senza l'intercetta
barplot(coeff_df$Coefficient[-1], names.arg = coeff_df$Feature[-1], 
        las = 2, col = "royalblue", main = "Importanza delle variabili nel modello Lasso")
abline(h = 0, col = "black", lwd = 1.5)






#Le s:
#Questi valori ti dicono quali variabili sono più forti a diverse penalizzazioni. In particolare:

#Le variabili con coefficienti che rimangono più alti (anche con l'aumento di lambda) sono quelle che il modello considera più importanti.
#Le variabili con coefficienti che si riducono rapidamente a zero sono quelle che il modello ritiene meno rilevanti.
#**Se una variabile ha coefficienti che rimangono esattamente zero in tutte le colonne, significa che la variabile è stata eliminata dal modello.















#### PLS ####
set.seed(1234)

#con cv:
preProc <- c("center", "scale")
Control=trainControl(method= "cv",number=5, classProbs=TRUE,summaryFunction=twoClassSummary)
plsPP=train(Diabetes_binary~.,data=dati_training_MS , method = "pls",  preProcess = preProc,
            trControl = Control, tuneLength=5,metric='Sens')

confusionMatrix(plsPP)


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
train_PRED_PLS <- predict(plsPP, newdata = dati_training, type = "prob")
train_PRED_prob_c1_PLS <- train_PRED_PLS[, "c1"]  
roc_train_PLS <- roc(dati_training$Diabetes_binary, train_PRED_prob_c1_PLS)
# Previsioni sui dati di validation
validation_PRED_PLS <- predict(plsPP, newdata = dati_validation, type = "prob")
validation_PRED_prob_c1_PLS <- validation_PRED_PLS[, "c1"]  
roc_validation_PLS <- roc(dati_validation$Diabetes_binary, validation_PRED_prob_c1_PLS)
print(roc_train_PLS)
print(roc_validation_PLS)
auc_train_PLS <- auc(roc_train_PLS)
auc_validation_PLS <- auc(roc_validation_PLS)
print(auc_train_PLS)
print(auc_validation_PLS)
overfitting_PLS=(auc_train_PLS-auc_validation_PLS)/auc_train_PLS
print(overfitting_PLS)
# NON OVERFITTTANO 




#### NAIVE BAYES #### 
# risultati buoni ci dice che e sostenibile ipotesi di indipendenza incondizionata 
#con cv
set.seed(1234)
preProc <- c("nzv","corr")
Control=trainControl(method= "cv",number=5, classProbs=TRUE,summaryFunction=twoClassSummary)
Naive_BayesPP=train(Diabetes_binary~.,data=dati_training , method = "nb", family ="binomial",
                    trControl = Control, tuneLength=5,na.action = na.exclude,metric='Sens')

confusionMatrix(Naive_BayesPP)

#senza cv: 

set.seed(1234)
preProc <- c("nzv", "scale","corr")

tuneGrid = expand.grid(
  fL = 0,       # Laplace smoothing (esempio: 0)
  usekernel = TRUE,  # Usa densità kernel
  adjust = 1         # Regola la larghezza del kernel
)

Control=trainControl(method= "none", classProbs=TRUE,summaryFunction=twoClassSummary)
Naive_BayesPP=train(Diabetes_binary~.,data=dati_training , method = "nb",preProcess=preProc,family ="binomial",
                    trControl = Control, tuneGrid=tuneGrid,na.action = na.exclude,metric='Sens')

validation_PRED_Naive <- predict(Naive_BayesPP, newdata = dati_validation, type = "raw")
conf_matrix_Naive <- confusionMatrix(validation_PRED_Naive, as.factor(dati_validation$Diabetes_binary))
print(conf_matrix_Naive)


#OVERFITTING NAIVE BAYES
library(pROC)
# Previsioni sui dati di training
train_PRED_Naive <- predict(Naive_BayesPP, newdata = dati_training, type = "prob")
train_PRED_prob_c1_Naive <- train_PRED_Naive[, "c1"]  
roc_train_Naive <- roc(dati_training$Diabetes_binary, train_PRED_prob_c1_Naive)
# Previsioni sui dati di validation
validation_PRED_Naive <- predict(Naive_BayesPP, newdata = dati_validation, type = "prob")
validation_PRED_prob_c1_Naive <- validation_PRED_Naive[, "c1"]  
roc_validation_Naive <- roc(dati_validation$Diabetes_binary, validation_PRED_prob_c1_Naive)
print(roc_train_Naive)
print(roc_validation_Naive)
auc_train_Naive <- auc(roc_train_Naive)
auc_validation_Naive <- auc(roc_validation_Naive)
print(auc_train_Naive)
print(auc_validation_Naive)
overfitting_Naive=(auc_train_Naive-auc_validation_Naive)/auc_train_Naive
print(overfitting_Naive)
# NON OVERFITTA



#### K NEAREST EIGHBORS ####
#con cv:
set.seed(1234)
grid = expand.grid(.k=seq(5,20, by=3))
preProc <- c( "scale","corr","nzv")
Control=trainControl(method= "cv",number=5,search="grid", classProbs=TRUE,summaryFunction=twoClassSummary)
knn_modelPP=train(Diabetes_binary~.,data=dati_training_MS , method = "knn",preProcess = preProc,
                  trControl = Control, tuneLength=5, tuneGrid=grid, na.action = na.exclude,metric='Sens')

confusionMatrix(knn_modelPP)


#senza cv



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
train_PRED_KNN <- predict(knn_modelPP, newdata = dati_training, type = "prob")
train_PRED_prob_c1_KNN <- train_PRED_KNN[, "c1"]  
roc_train_KNN <- roc(dati_training$Diabetes_binary, train_PRED_prob_c1_KNN)
# Previsioni sui dati di validation
validation_PRED_KNN <- predict(knn_modelPP, newdata = dati_validation, type = "prob")
validation_PRED_prob_c1_KNN <- validation_PRED_KNN[, "c1"]  
roc_validation_KNN <- roc(dati_validation$Diabetes_binary, validation_PRED_prob_c1_KNN)
print(roc_train_KNN)
print(roc_validation_KNN)
auc_train_KNN <- auc(roc_train_KNN)
auc_validation_KNN <- auc(roc_validation_KNN)
print(auc_train_KNN)
print(auc_validation_KNN)
overfitting_Knn=(auc_train_KNN-auc_validation_KNN)/auc_train_KNN
print(overfitting_Knn)
#Niente  OVERFITTING


validation_PRED_Naive <- predict(Naive_BayesPP, newdata = dati_validation, type = "raw")
conf_matrix_Naive <- confusionMatrix(validation_PRED_Naive, dati_validation$Diabetes_binary)
print(conf_matrix_Naive)

#### ADA BOOST ####


predictions <- predict(ada_modelPP, newdata = dati_training)
conf_matrix <- table(predictions, dati_training$Diabetes_binary)
print(conf_matrix)

#OVERFITTING ADA Boost
library(pROC)
# Previsioni sui dati di training
train_PRED_ada <- predict(ada_modelPP, newdata = dati_training_MS, type = "prob")
train_PRED_prob_c1_ada <- train_PRED_ada[, "c1"]  
roc_train_ada <- roc(dati_training_MS$Diabetes_binary, train_PRED_prob_c1_ada)
# Previsioni sui dati di validation
validation_PRED_ada <- predict(ada_modelPP, newdata = dati_validation_MS, type = "prob")
validation_PRED_prob_c1_ada <- validation_PRED_ada[, "c1"]  
roc_validation_ada <- roc(dati_validation_MS$Diabetes_binary, validation_PRED_prob_c1_ada)
print(roc_train_ada)
print(roc_validation_ada)
auc_train_ada <- auc(roc_train_ada)
auc_validation_ada <- auc(roc_validation_ada)
print(auc_train_ada)
print(auc_validation_ada)
overfitting_ada=(auc_train_ada-auc_validation_ada)/auc_train_ada
print(overfitting_ada)

#### GRADIENT BOOSTING ####
set.seed(123)
control<- trainControl( method="cv", number=5,search="grid",summaryFunction=twoClassSummary,classProbs = TRUE)
gradien_boostingPP <- train(Diabetes_binary~ ., data=dati_training, method = "gbm", trControl = control) 
confusionMatrix(gradien_boostingPP)

#OVERFITTING Gradient Boosting
library(pROC)
# Previsioni sui dati di training
train_PRED_GradBoost <- predict(gradien_boostingPP, newdata = dati_training, type = "prob")
train_PRED_prob_c1_GradBoost <- train_PRED_GradBoost[, "c1"]  
roc_train_GradBoost <- roc(dati_training$Diabetes_binary, train_PRED_prob_c1_GradBoost)
# Previsioni sui dati di validation
validation_PRED_GradBoost <- predict(gradien_boostingPP, newdata = dati_validation, type = "prob")
validation_PRED_prob_c1_GradBoost <- validation_PRED_GradBoost[, "c1"]  
roc_validation_GradBoost <- roc(dati_validation$Diabetes_binary, validation_PRED_prob_c1_GradBoost)
print(roc_train_GradBoost)
print(roc_validation_GradBoost)
auc_train_GradBoost <- auc(roc_train_GradBoost)
auc_validation_GradBoost <- auc(roc_validation_GradBoost)
print(auc_train_GradBoost)
print(auc_validation_GradBoost)
overfitting_gradientBoost=(auc_train_GradBoost-auc_validation_GradBoost)/auc_train_GradBoost
print(overfitting_gradientBoost)
# non overfitta 


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
control <- trainControl(method="cv", number=5, search="grid",summaryFunction = twoClassSummary,  classProbs = TRUE)
alberoPP <- train(
  Diabetes_binary ~ .,        # Variabile target
  data = dati_training,       # Dataset di training
  method = "rpart",           # Metodo per alberi decisionali
  trControl=control,
  metric='Sens',
  tuneLength = 10             # Ricerca automatica dei migliori iperparametri
)
Prediction_alberoPP <- predict(alberoPP, dati_training)
conf_matrix_alberoPP <- confusionMatrix(Prediction_alberoPP, dati_training$Diabetes_binary)
print(conf_matrix_alberoPP)
confusionMatrix(alberoPP)

#OVERFITTING Albero
library(pROC)
# Previsioni sui dati di training
train_PRED_Tree <- predict(alberoPP, newdata = dati_training, type = "prob")
train_PRED_prob_c1_Tree <- train_PRED_Tree[, "c1"]  
roc_train_Tree <- roc(dati_training$Diabetes_binary, train_PRED_prob_c1_Tree)
# Previsioni sui dati di validation
validation_PRED_Tree <- predict(alberoPP, newdata = dati_validation, type = "prob")
validation_PRED_prob_c1_Tree <- validation_PRED_Tree[, "c1"]  
roc_validation_Tree <- roc(dati_validation$Diabetes_binary, validation_PRED_prob_c1_Tree)
print(roc_train_Tree)
print(roc_validation_Tree)
auc_train_Tree <- auc(roc_train_Tree)
auc_validation_Tree <- auc(roc_validation_Tree)
print(auc_train_Tree)
print(auc_validation_Tree)
overfitting_albero=(auc_train_Tree-auc_validation_Tree)/auc_train_Tree
print(overfitting_albero)
# Non overfitta
Vimportance <- varImp(alberoPP)
plot(Vimportance) # plot importanza variabili 

validation_PRED_Tree <- predict(alberoPP, newdata = dati_validation, type = "raw")
conf_matrix_albero <- confusionMatrix(validation_PRED_Tree, dati_validation$Diabetes_binary)
print(conf_matrix_albero)



#### RANDOM FOREST ####

library(caret)
str(dati_training)
set.seed(123)
metric <- "Sens"
control <- trainControl(method="cv", number=5, search="grid",summaryFunction = twoClassSummary,  classProbs = TRUE)
tunegrid <- expand.grid(.mtry=c(4:10)) # numero covariate random per ogni albero centrata valore suggerito da braiman radice di n =6,5
FORESTPP2 <- train(Diabetes_binary~., data=dati_training, method="rf", metric=metric, tuneGrid=tunegrid,trControl=control, ntree=250)
plot(FORESTPP2)
FORESTPP2
confusionMatrix(FORESTPP2)
getTrainPerf(FORESTPP2)



# VARIABILI IMPORTANTI fare model selection con random forest
Vimportance <- varImp(FORESTPP2)
plot(Vimportance) # plot importanza variabili 

VImP=as.data.frame(Vimportance$importance)
V=subset(VImP, Overall>20)
V2=t(V)
 # variabili che tiene "HighBP1" "HighChol1" "BMI" "MentHlth" "PhysHlth" "DiffWalk1" °GenHth4°
rpart.plot(alberoPP, type = 4)



#OVERFITTING Random Forest
library(pROC)
# Previsioni sui dati di training
train_PRED_RandomF <- predict(FORESTPP2, newdata = dati_training, type = "prob")
train_PRED_prob_c1_RandomF <- train_PRED_RandomF[, "c1"]  
roc_train_RandomF <- roc(dati_training$Diabetes_binary, train_PRED_prob_c1_RandomF)
# Previsioni sui dati di validation
validation_PRED_RandomF <- predict(FORESTPP2, newdata = dati_validation, type = "prob")
validation_PRED_prob_c1_RandomF <- validation_PRED_RandomF[, "c1"]  
roc_validation_RandomF <- roc(dati_validation$Diabetes_binary, validation_PRED_prob_c1_RandomF)
print(roc_train_RandomF)
print(roc_validation_RandomF)
auc_train_RandomF <- auc(roc_train_RandomF)
auc_validation_RandomF <- auc(roc_validation_RandomF)
print(auc_train_RandomF)
print(auc_validation_RandomF)
overfitting_RandomForest=(auc_train_RandomF-auc_validation_RandomF)/auc_train_RandomF
print(overfitting_RandomForest)
#overfitting 


#### RETI NEURALI ####
#metodo migliore non sbaglia nessuno diabetico
Grid0 = expand.grid(.size   =seq(1,7,  by=1), .decay = 0.1 )

set.seed(123) # mette come parametro di fit casuale 7 nell esmpio dice di provare con diversi "semi" numeri per vedere se modello migliora o peggiora
control = trainControl(method="cv", number=5, search = "grid",summaryFunction = twoClassSummary,classProbs = TRUE)
nnetPP2 <- train(dati_training_MS[-1], dati_training_MS$Diabetes_binary,
                method = "nnet",
                tuneGrid=Grid0,
                preProcess=c("scale","corr","nzv"), 
                metric="Sens", trControl=control,
                trace = TRUE, # uso per vedere se converge se non converge provare ad aumentare numero max iterazioni
                maxit = 300) # numero massimo di iterazioni

plot(nnetPP2) #vedo andamento neuroni nascosti e metrica dopo il 6 inizia a decrescere
getTrainPerf(nnetPP2) 
print(nnetPP2) # size=dimensione neuroni nascosti 
getTrainPerf(nnetPP2) #migliore rete per sensitivity ha 6 neuroni nascosti 
confusionMatrix(nnetPP2)

#OVERFITTING Reti neurali 
library(pROC)
# Previsioni sui dati di training
train_PRED_nnet <- predict(nnetPP2, newdata = dati_training, type = "prob")
train_PRED_prob_c1_nnet <- train_PRED_nnet[, "c1"]  
roc_train_nnet <- roc(dati_training$Diabetes_binary, train_PRED_prob_c1_nnet)
# Previsioni sui dati di validation
validation_PRED_nnet <- predict(nnetPP2, newdata = dati_validation, type = "prob")
validation_PRED_prob_c1_nnet <- validation_PRED_nnet[, "c1"]  
roc_validation_nnet <- roc(dati_validation$Diabetes_binary, validation_PRED_prob_c1_nnet)
print(roc_train_nnet)
print(roc_validation_nnet)
auc_train_nnet <- auc(roc_train_nnet)
auc_validation_nnet <- auc(roc_validation_nnet)
print(auc_train_nnet)
print(auc_validation_nnet)
overfitting_neuralnet=(auc_train_nnet-auc_validation_nnet)/auc_train_nnet
print(overfitting_neuralnet)
#Non c'è overfitting 

validation_PRED_nnet <- predict(nnetPP2, newdata = dati_validation, type = "raw")
conf_matrix_lasso <- confusionMatrix(validation_PRED_nnet, dati_validation$Diabetes_binary)
print(conf_matrix_lasso)



#### ENSAMBLE e STAKING  ####

library(caretEnsemble) 
library(caret) 
library(pROC ) 

#Usiamo i 2 modelli più performanti 
modelli <- list("glmnet"=lassoPP,"nnet"=nnetPP2) 
class(modelli) <- "caretList" 
control = trainControl(method="cv", number=5, search = "grid",summaryFunction = twoClassSummary,classProbs = TRUE)

#Ensamble model 
ENSAMBLE <- caretEnsemble(modelli, metric="Sens", trControl=control, classProbs=TRUE)

#Staking glm con 2 modelli più performanti 
stakglm<-caretStack(modelli,method='glm',metric="Sens", trControl=control, classProbs=TRUE)

confusionMatrix(nnetPP2)


#PREDICTIONS OF ENSAMBLE E STAKING 
enspred <- predict(ENSAMBLE, newdata=vali, type="prob") 
stackpreds <- predict(stakglm, newdata=vali, type="prob") 





##### CONFRONTO CURVE BOXPLOT ####
library(caret)
library(pROC)
results <- resamples(list(albero=alberoPP,glm_PreProc=glmPP,plsPP=plsPP,lasso_PP=lassoPP,Naive_Bayes_PP=Naive_BayesPP,knn_PP=knn_modelPP,nnet_pp=nnetPP2,RanfomForest_PP=FORESTPP2,Gradient_Boosting_PP=gradien_boostingPP))  
bwplot(results)
bwplot(results,layout=c(1,3))


##### CONFRONTO CURVE ROC ####
library(funModeling)
library(dplyr)

# Plot della prima curva ROC
plot(roc_validation_logist, col="pink", lwd=2, main="Confronto Curve ROC", print.auc=TRUE, legacy.axes=TRUE)
lines(roc_validation_lasso, col="brown", lwd=2)
lines(roc_validation_PLS, col="orange", lwd=2)
lines(roc_validation_Naive, col="green", lwd=2)
lines(roc_validation_KNN, col="yellow", lwd=2)
lines(roc_validation_Tree, col="purple", lwd=2)
lines(roc_validation_RandomF, col="red", lwd=2)
lines(roc_validation_nnet, col="blue", lwd=2)
lines(roc_validation_GradBoost, col="lightgreen", lwd=2)


##### CURVE LIFT ####
# score= probabilita prevista 
#gain_lift(data = copy, score = 'predP', target = 'c1')


train=dati_training
test=dati_validation
library(klaR)
library(caret)
set.seed(1234)
colnames(test)
#glmPP
p_glm <- predict(glmPP, newdata = dati_validation, type = "prob")
p_glm_c1 <- p_glm[, "c1"]  

test$posterior_glm=p_glm_c1
test=na.omit(test)
head(test)
library(funModeling)
gain_lift(data = test, score = 'posterior_glm', target = 'Diabetes_binary')

#LassoPP
p_Lasso <- predict(lassoPP, newdata = dati_validation, type = "prob")
posterior_Lasso_c1 <- p_Lasso[, "c1"]  

test$posterior_Lasso=posterior_Lasso_c1
test=na.omit(test)
head(test)
library(funModeling)
gain_lift(data = test, score = 'posterior_Lasso', target = 'Diabetes_binary')

#plsPP
p_pls <- predict(plsPP, newdata = dati_validation_MS, type = "prob")
posterior_pls_c1 <- p_pls[, "c1"]  

test$posterior_pls=posterior_pls_c1
test=na.omit(test)
head(test)
library(funModeling)
gain_lift(data = test, score = 'posterior_pls', target = 'Diabetes_binary')


#FORESTPP2
p_FOREST <- predict(FORESTPP2, newdata = dati_validation, type = "prob")
posterior_FOREST_c1 <- p_FOREST[, "c1"]  

test$posterior_FOREST=posterior_FOREST_c1
test=na.omit(test)
head(test)
library(funModeling)
gain_lift(data = test, score = 'posterior_FOREST', target = 'Diabetes_binary')

#nnetPP2
p_nnet <- predict(nnetPP2, newdata = dati_validation, type = "prob")
posterior_nnet_c1 <- p_nnet[, "c1"]  

test$posterior_nnet=posterior_nnet_c1
test=na.omit(test)
head(test)
library(funModeling)
gain_lift(data = test, score = 'posterior_nnet', target = 'Diabetes_binary')


#gradien_boostingPP
p_gradien_boosting <- predict(gradien_boostingPP, newdata = dati_validation, type = "prob")
posterior_gradien_boosting_c1 <- p_gradien_boosting[, "c1"]  

test$posterior_gradien_boosting=posterior_gradien_boosting_c1
test=na.omit(test)
head(test)
library(funModeling)
gain_lift(data = test, score = 'posterior_gradien_boosting', target = 'Diabetes_binary')

#ADA
p_ADA <- predict(ada_modelPP, newdata = dati_validation, type = "prob")
posterior_ADA_c1 <- p_gradien_boosting[, "c1"]  

test$posterior_ADA=posterior_ADA_c1
test=na.omit(test)
head(test)
library(funModeling)
gain_lift(data = test, score = 'posterior_ADA', target = 'Diabetes_binary')

#albero

p_albero<- predict(alberoPP, newdata = dati_validation, type = "prob")
posterior_albero_c1 <- p_albero[, "c1"]  

test$posterior_albero=posterior_albero_c1
test=na.omit(test)
head(test)
library(funModeling)
gain_lift(data = test, score = 'posterior_albero', target = 'Diabetes_binary')


# Nayve=Bayes
p_Nayve<- predict(Naive_BayesPP, newdata = dati_validation, type = "prob")
posterior_Naive_c1 <- p_Nayve[, "c1"]  

test$posterior_Naive=posterior_Naive_c1
test=na.omit(test)
head(test)
library(funModeling)
gain_lift(data = test, score = 'posterior_Naive', target = 'Diabetes_binary')


# KNN

p_Nayve<- predict(knn_modelPP, newdata = dati_validation, type = "prob")
posterior_Naive_c1 <- p_Nayve[, "c1"]  

test$posterior_Naive=posterior_Naive_c1
test=na.omit(test)
head(test)
library(funModeling)
gain_lift(data = test, score = 'posterior_Naive', target = 'Diabetes_binary')


##### MATRICE DI COSTI pt1 SOGLIA  ######
# ricavo postirior su dataset validation
validation_PRED_lasso <- predict(lassoPP, newdata = dati_validation, type = "prob")
head(validation_PRED_lasso)
df=data.frame(cbind(dati_validation$Diabetes_binary , validation_PRED_lasso))
head(df)
colnames(df)=c("Class","Probc1","Probc0") # Class+ se diabetico o no in validation, porbabilita di essre c0 o c1 utilizzando le predict
head(df)# ottengo classe 

# seleziono prime due colonne target osservato e probabilita evento di interesse
df=df[,1:2]
head(df) 

library(dplyr)

thresholds <- seq(from = 0, to = 1, by = 0.01)
prop_table <- data.frame(threshold = thresholds, prop_true_c1 = NA,  prop_true_c0 = NA, true_c1 = NA,  true_c0 = NA ,fn_c1=NA)

for (threshold in thresholds) {
  pred <- ifelse(df$Probc1 > threshold, "c1", "c0")  # be careful here!!!
  pred_t <- ifelse(pred == df$Class, TRUE, FALSE)
  
  group <- data.frame(df, "pred" = pred_t) %>%
    group_by(Class, pred) %>%
    dplyr::summarise(n = n())
  
  group_c1 <- filter(group, Class == "c1")
  
  true_c1=sum(filter(group_c1, pred == TRUE)$n)
  prop_c1 <- sum(filter(group_c1, pred == TRUE)$n) / sum(group_c1$n)
  
  prop_table[prop_table$threshold == threshold, "prop_true_c1"] <- prop_c1
  prop_table[prop_table$threshold == threshold, "true_c1"] <- true_c1
  
  fn_c1=sum(filter(group_c1, pred == FALSE)$n)
  # true M predicted as R
  prop_table[prop_table$threshold == threshold, "fn_c1"] <- fn_c1
  
  
  group_c0 <- filter(group, Class == "c0")
  
  true_c0=sum(filter(group_c0, pred == TRUE)$n)
  prop_c0 <- sum(filter(group_c0, pred == TRUE)$n) / sum(group_c0$n)
  
  prop_table[prop_table$threshold == threshold, "prop_true_c0"] <- prop_c0
  prop_table[prop_table$threshold == threshold, "true_c0"] <- true_c0
  
}

head(prop_table, n=10)


prop_table$n=nrow(dati_validation)
prop_table$fp_c1=nrow(dati_validation)-prop_table$true_c0-prop_table$true_c1-prop_table$fn_c1 # falsi positivi
prop_table$acc=(prop_table$true_c0+prop_table$true_c1)/nrow(dati_validation) # accuracy
# precision= veri c1 sul totale previsti come c1
prop_table$prec_c1=prop_table$true_c1/(prop_table$true_c1+prop_table$fp_c1)
prop_table$F1=2*(prop_table$prop_true_c1*prop_table$prec_c1)/(prop_table$prop_true_c1+prop_table$prec_c1)



tail(prop_table)
# inputazione dati per non avere NaN zero denominatore manda valori a infinito
library(Hmisc)
prop_table$prec_c1=impute(prop_table$prec_c1, 1)
prop_table$F1=impute(prop_table$F1, 0)
tail(prop_table)
 # elimino colonne conteggi che non mi servono 
prop_table2 = prop_table[,-c(4:8)] 

library(dplyr)
library(tidyr)
head(prop_table2)
# impilo metriche 
gathered=prop_table2 %>%
  gather(x, y, prop_true_c1:F1)

head(gathered)
#faccio plot metriche 
library(ggplot2)
gathered %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(y = "measures",
       color = "M: event\nR: nonevent")
# plot mi da informazioni su soglia da scegliere per validation 
# precision metrica da prediligire ovvero percentUale di c1 veri sul totale di previsti c1 
# vorro quindi soglia precision alta 0,95
# zoom
gathered %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(y = "measures",
       color = "M: event\n R: nonevent") +
  coord_cartesian(xlim = c(0.8, 0.99))

# applico soglia sul dataset validation 
df$decision=ifelse(df$Probc1>0.15,"c1","c0")

# matrice confusione
table(df$Class,df$decision)
 

confusionMatrix(as.factor(df$decision),df$Class, positive = "c1")
#Pos Pred Value (precision) : 1.00000000 =a/a+c  a  c
#                                                b  d

validation_PRED_lasso <- predict(lassoPP, newdata = dati_validation, type = "raw")
conf_matrix_lasso <- confusionMatrix(validation_PRED_lasso, dati_validation$Diabetes_binary)
print(conf_matrix_lasso)
table(dati_validation$Diabetes_binary)
head(df)






##### MATRICE COSTI pt2 ####
library(caret)
library(ada)
# Matrice di costi: FN penalizzati più di FP
cost_matrix <- matrix(
  c(0, 1,  # Costi per predizione negativa: TN = 0, FN = 1 (penalità alta)
    1, 0), # Costi per predizione positiva: FP = 1, TP = 0
  nrow = 2,
  byrow = TRUE,
  dimnames = list(Predicted = c("c1", "c0"), Actual = c("c1", "c0"))
)

print("Matrice dei costi:")
print(cost_matrix)
# Funzione personalizzata per ottimizzare la sensitivity (usando costi)
custom_metric <- function(data, lev = NULL, model = NULL) {
  confusion <- table(data$obs, data$pred)
  
  # Calcola il costo totale basato sulla matrice
  total_cost <- sum(
    confusion[1, 2] * cost_matrix["c1", "c0"], # FN
    confusion[2, 1] * cost_matrix["c0", "c1"]  # FP
  )
  
  # Sensitivity
  sensitivity <- confusion[2, ] / sum(confusion[2, 2])
  
  return(c(Cost = -total_cost, Sensitivity = sensitivity))
}

# Controllo per il training
control <- trainControl(
  method = "cv",                      # Cross-validation
  number = 5,                         # 5-fold CV
  summaryFunction = custom_metric,    # Usa la metrica personalizzata
  classProbs = TRUE                   # Probabilità di classe
)

# Modello AdaBoost con matrice di costi
set.seed(123)
model_cost <- train(
  Diabetes_binary ~ .,                # Formula
  data = dati_training,               # Dataset di training
  method = "glmnet",                     # Modello AdaBoost
  metric = "Cost",                    # Ottimizza il costo
  trControl = control,                # Controllo
  preProcess=c("scale")
  #tuneGrid = expand.grid(iter = 50, maxdepth = 3, nu = 0.1)  # Parametri AdaBoost
)

# Predizioni sul dataset di validazione
predictions_cost <- predict(model_cost, newdata = dati_validation, type = "raw")
conf_matrix_cost <- confusionMatrix(predictions_cost,as.factor( dati_validation$Diabetes_binary))
print(conf_matrix_cost)
confusionMatrix(model_cost)
confusionMatrix(lassoPP)


##### STEP 4 CONFRONTO MODELLO DATI STEP 4 ####

STEP4_PRED_lasso <- predict(lassoPP, newdata = dati_step_4, type = "raw")
conf_matrix_STEP4 <- confusionMatrix(STEP4_PRED_lasso, dati_step_4$Diabetes_binary)
print(conf_matrix_STEP4)



STEP4_PRED_lasso <- predict(ada_model_cost, newdata = dati_step_4, type = "raw")
conf_matrix_STEP4 <- confusionMatrix(STEP4_PRED_lasso, dati_step_4$Diabetes_binary)
print(conf_matrix_STEP4)


# far diventare lista una vettore e poi unirlo al dataset iniziale 
dati= list(valori=c(predictions_ADA))
tabella= data.frame(dati)
print(tabella)
dati_validation$new=tabella$valori


##### STEP 4 FINALE KNN ####

STEP4_PRED_Knn <- predict(knn_modelPP, newdata = dati_step_4, type = "prob")
head(STEP4_PRED_Knn)
df=data.frame(cbind(dati_step_4$Diabetes_binary , STEP4_PRED_Knn))
head(df)
colnames(df)=c("Class","Probc1","Probc0") # Class+ se diabetico o no in validation, porbabilita di essre c0 o c1 utilizzando le predict
head(df)# ottengo classe 

# seleziono prime due colonne target osservato e probabilita evento di interesse
df=df[,1:2]
head(df) 

library(dplyr)

thresholds <- seq(from = 0, to = 1, by = 0.01)
prop_table <- data.frame(threshold = thresholds, prop_true_c1 = NA,  prop_true_c0 = NA, true_c1 = NA,  true_c0 = NA ,fn_c1=NA)

for (threshold in thresholds) {
  pred <- ifelse(df$Probc1 > threshold, "c1", "c0")  # be careful here!!!
  pred_t <- ifelse(pred == df$Class, TRUE, FALSE)
  
  group <- data.frame(df, "pred" = pred_t) %>%
    group_by(Class, pred) %>%
    dplyr::summarise(n = n())
  
  group_c1 <- filter(group, Class == "c1")
  
  true_c1=sum(filter(group_c1, pred == TRUE)$n)
  prop_c1 <- sum(filter(group_c1, pred == TRUE)$n) / sum(group_c1$n)
  
  prop_table[prop_table$threshold == threshold, "prop_true_c1"] <- prop_c1
  prop_table[prop_table$threshold == threshold, "true_c1"] <- true_c1
  
  fn_c1=sum(filter(group_c1, pred == FALSE)$n)
  # true M predicted as R
  prop_table[prop_table$threshold == threshold, "fn_c1"] <- fn_c1
  
  
  group_c0 <- filter(group, Class == "c0")
  
  true_c0=sum(filter(group_c0, pred == TRUE)$n)
  prop_c0 <- sum(filter(group_c0, pred == TRUE)$n) / sum(group_c0$n)
  
  prop_table[prop_table$threshold == threshold, "prop_true_c0"] <- prop_c0
  prop_table[prop_table$threshold == threshold, "true_c0"] <- true_c0
  
}

head(prop_table, n=10)


prop_table$n=nrow(dati_step_4)
prop_table$fp_c1=nrow(dati_step_4)-prop_table$true_c0-prop_table$true_c1-prop_table$fn_c1 # falsi positivi
prop_table$acc=(prop_table$true_c0+prop_table$true_c1)/nrow(dati_step_4) # accuracy
# precision= veri c1 sul totale previsti come c1
prop_table$prec_c1=prop_table$true_c1/(prop_table$true_c1+prop_table$fp_c1)
prop_table$F1=2*(prop_table$prop_true_c1*prop_table$prec_c1)/(prop_table$prop_true_c1+prop_table$prec_c1)



tail(prop_table)
# inputazione dati per non avere NaN zero denominatore manda valori a infinito
library(Hmisc)
prop_table$prec_c1=impute(prop_table$prec_c1, 1)
prop_table$F1=impute(prop_table$F1, 0)
tail(prop_table)
# elimino colonne conteggi che non mi servono 
prop_table2 = prop_table[,-c(4:8)] 

library(dplyr)
library(tidyr)
head(prop_table2)
# impilo metriche 
gathered=prop_table2 %>%
  gather(x, y, prop_true_c1:F1)

head(gathered)
#faccio plot metriche 
library(ggplot2)
gathered %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(y = "measures",
       color = "M: event\nR: nonevent")
# plot mi da informazioni su soglia da scegliere per validation 
# precision metrica da prediligire ovvero percentUale di c1 veri sul totale di previsti c1 
# vorro quindi soglia precision alta 0,95
# zoom
gathered %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(y = "measures",
       color = "M: event\n R: nonevent") +
  coord_cartesian(xlim = c(0.35, 0.50))

# applico soglia sul dataset validation 
df$decision=ifelse(df$Probc1>0.30,"c1","c0")

# matrice confusione
table(df$Class,df$decision)


confusionMatrix(as.factor(df$decision),df$Class, positive = "c1")
#Pos Pred Value (precision) : 1.00000000 =a/a+c  a  c
#                                                b  d
table(dati_step_4$Diabetes_binary)

head(df,n=20)


##### STEP 4 FINALE Lasso ####

STEP4_PRED_Lasso <- predict(lassoPP, newdata = dati_step_4, type = "prob")
head(STEP4_PRED_Lasso)
df=data.frame(cbind(dati_step_4$Diabetes_binary , STEP4_PRED_Lasso))
head(df)
colnames(df)=c("Class","Probc1","Probc0") # Class+ se diabetico o no in validation, porbabilita di essre c0 o c1 utilizzando le predict
head(df)# ottengo classe 

# seleziono prime due colonne target osservato e probabilita evento di interesse
df=df[,1:2]
head(df) 

library(dplyr)

thresholds <- seq(from = 0, to = 1, by = 0.01)
prop_table <- data.frame(threshold = thresholds, prop_true_c1 = NA,  prop_true_c0 = NA, true_c1 = NA,  true_c0 = NA ,fn_c1=NA)

for (threshold in thresholds) {
  pred <- ifelse(df$Probc1 > threshold, "c1", "c0")  # be careful here!!!
  pred_t <- ifelse(pred == df$Class, TRUE, FALSE)
  
  group <- data.frame(df, "pred" = pred_t) %>%
    group_by(Class, pred) %>%
    dplyr::summarise(n = n())
  
  group_c1 <- filter(group, Class == "c1")
  
  true_c1=sum(filter(group_c1, pred == TRUE)$n)
  prop_c1 <- sum(filter(group_c1, pred == TRUE)$n) / sum(group_c1$n)
  
  prop_table[prop_table$threshold == threshold, "prop_true_c1"] <- prop_c1
  prop_table[prop_table$threshold == threshold, "true_c1"] <- true_c1
  
  fn_c1=sum(filter(group_c1, pred == FALSE)$n)
  # true M predicted as R
  prop_table[prop_table$threshold == threshold, "fn_c1"] <- fn_c1
  
  
  group_c0 <- filter(group, Class == "c0")
  
  true_c0=sum(filter(group_c0, pred == TRUE)$n)
  prop_c0 <- sum(filter(group_c0, pred == TRUE)$n) / sum(group_c0$n)
  
  prop_table[prop_table$threshold == threshold, "prop_true_c0"] <- prop_c0
  prop_table[prop_table$threshold == threshold, "true_c0"] <- true_c0
  
}

head(prop_table, n=10)


prop_table$n=nrow(dati_step_4)
prop_table$fp_c1=nrow(dati_step_4)-prop_table$true_c0-prop_table$true_c1-prop_table$fn_c1 # falsi positivi
prop_table$acc=(prop_table$true_c0+prop_table$true_c1)/nrow(dati_step_4) # accuracy
# precision= veri c1 sul totale previsti come c1
prop_table$prec_c1=prop_table$true_c1/(prop_table$true_c1+prop_table$fp_c1)
prop_table$F1=2*(prop_table$prop_true_c1*prop_table$prec_c1)/(prop_table$prop_true_c1+prop_table$prec_c1)



tail(prop_table)
# inputazione dati per non avere NaN zero denominatore manda valori a infinito
library(Hmisc)
prop_table$prec_c1=impute(prop_table$prec_c1, 1)
prop_table$F1=impute(prop_table$F1, 0)
tail(prop_table)
# elimino colonne conteggi che non mi servono 
prop_table2 = prop_table[,-c(4:8)] 

library(dplyr)
library(tidyr)
head(prop_table2)
# impilo metriche 
gathered=prop_table2 %>%
  gather(x, y, prop_true_c1:F1)

head(gathered)
#faccio plot metriche 
library(ggplot2)
gathered %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(y = "measures",
       color = "M: event\nR: nonevent")
# plot mi da informazioni su soglia da scegliere per validation 
# precision metrica da prediligire ovvero percentUale di c1 veri sul totale di previsti c1 
# vorro quindi soglia precision alta 0,95
# zoom
gathered %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(y = "measures",
       color = "M: event\n R: nonevent") +
  coord_cartesian(xlim = c(0.10, 0.25))

# applico soglia sul dataset validation 
df$decision=ifelse(df$Probc1>0.15,"c1","c0")

# matrice confusione
table(df$Class,df$decision)


confusionMatrix(as.factor(df$decision),df$Class, positive = "c1")
#Pos Pred Value (precision) : 1.00000000 =a/a+c  a  c
#                                                b  d
table(dati_step_4$Diabetes_binary)

head(df,n=20)

dati_plus_prevLasso<-dati_step_4
predizione<- predict(lassoPP, newdata = dati_plus_prevLasso, type = "prob")
dati_plus_prevLasso$diabete=ifelse(predizione$c1>0.15,"c1","c0")
dati_plus_prevLasso$diabete<-factor(dati_plus_prevLasso$diabete, levels = c('c1','c0'), labels=c('c1','c0'))
str(dati_plus_prevLasso)
conf_matrix_STEP4 <- confusionMatrix(dati_plus_prevLasso$diabete, dati_plus_prevLasso$Diabetes_binary)
print(conf_matrix_STEP4)

##### STEP 4 FINALE Neural NET ####

STEP4_PRED_nnet <- predict(nnetPP2, newdata = dati_step_4, type = "prob")
head(STEP4_PRED_nnet)
df=data.frame(cbind(dati_step_4$Diabetes_binary , STEP4_PRED_nnet))
head(df)
colnames(df)=c("Class","Probc1","Probc0") # Class+ se diabetico o no in validation, porbabilita di essre c0 o c1 utilizzando le predict
head(df)# ottengo classe 

# seleziono prime due colonne target osservato e probabilita evento di interesse
df=df[,1:2]
head(df) 

library(dplyr)

thresholds <- seq(from = 0, to = 1, by = 0.01)
prop_table <- data.frame(threshold = thresholds, prop_true_c1 = NA,  prop_true_c0 = NA, true_c1 = NA,  true_c0 = NA ,fn_c1=NA)

for (threshold in thresholds) {
  pred <- ifelse(df$Probc1 > threshold, "c1", "c0")  # be careful here!!!
  pred_t <- ifelse(pred == df$Class, TRUE, FALSE)
  
  group <- data.frame(df, "pred" = pred_t) %>%
    group_by(Class, pred) %>%
    dplyr::summarise(n = n())
  
  group_c1 <- filter(group, Class == "c1")
  
  true_c1=sum(filter(group_c1, pred == TRUE)$n)
  prop_c1 <- sum(filter(group_c1, pred == TRUE)$n) / sum(group_c1$n)
  
  prop_table[prop_table$threshold == threshold, "prop_true_c1"] <- prop_c1
  prop_table[prop_table$threshold == threshold, "true_c1"] <- true_c1
  
  fn_c1=sum(filter(group_c1, pred == FALSE)$n)
  # true M predicted as R
  prop_table[prop_table$threshold == threshold, "fn_c1"] <- fn_c1
  
  
  group_c0 <- filter(group, Class == "c0")
  
  true_c0=sum(filter(group_c0, pred == TRUE)$n)
  prop_c0 <- sum(filter(group_c0, pred == TRUE)$n) / sum(group_c0$n)
  
  prop_table[prop_table$threshold == threshold, "prop_true_c0"] <- prop_c0
  prop_table[prop_table$threshold == threshold, "true_c0"] <- true_c0
  
}

head(prop_table, n=10)


prop_table$n=nrow(dati_step_4)
prop_table$fp_c1=nrow(dati_step_4)-prop_table$true_c0-prop_table$true_c1-prop_table$fn_c1 # falsi positivi
prop_table$acc=(prop_table$true_c0+prop_table$true_c1)/nrow(dati_step_4) # accuracy
# precision= veri c1 sul totale previsti come c1
prop_table$prec_c1=prop_table$true_c1/(prop_table$true_c1+prop_table$fp_c1)
prop_table$F1=2*(prop_table$prop_true_c1*prop_table$prec_c1)/(prop_table$prop_true_c1+prop_table$prec_c1)



tail(prop_table)
# inputazione dati per non avere NaN zero denominatore manda valori a infinito
library(Hmisc)
prop_table$prec_c1=impute(prop_table$prec_c1, 1)
prop_table$F1=impute(prop_table$F1, 0)
tail(prop_table)
# elimino colonne conteggi che non mi servono 
prop_table2 = prop_table[,-c(4:8)] 

library(dplyr)
library(tidyr)
head(prop_table2)
# impilo metriche 
gathered=prop_table2 %>%
  gather(x, y, prop_true_c1:F1)

head(gathered)
#faccio plot metriche 
library(ggplot2)
gathered %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(y = "measures",
       color = "M: event\nR: nonevent")
# plot mi da informazioni su soglia da scegliere per validation 
# precision metrica da prediligire ovvero percentUale di c1 veri sul totale di previsti c1 
# vorro quindi soglia precision alta 0,95
# zoom
gathered %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(y = "measures",
       color = "M: event\n R: nonevent") +
  coord_cartesian(xlim = c(0.10, 0.25))

# applico soglia sul dataset validation 
df$decision=ifelse(df$Probc1>0.15,"c1","c0")

# matrice confusione
table(df$Class,df$decision)


confusionMatrix(as.factor(df$decision),df$Class, positive = "c1")
#Pos Pred Value (precision) : 1.00000000 =a/a+c  a  c
#                                                b  d
table(dati_step_4$Diabetes_binary)

head(df,n=20)



dati_plus_prevLasso<-dati_step_4
predizione<- predict(lassoPP, newdata = dati_plus_prevLasso, type = "prob")
dati_plus_prevLasso$diabete=ifelse(predizione$c1>0.15,"c1","c0")
dati_plus_prevLasso$diabete<-factor(dati_plus_prevLasso$diabete, levels = c('c1','c0'), labels=c('c1','c0'))
str(dati_plus_prevLasso)
conf_matrix_STEP4 <- confusionMatrix(dati_plus_prevLasso$diabete, dati_plus_prevLasso$Diabetes_binary)
print(conf_matrix_STEP4)
















# Caricare i pacchetti necessari
library(rpart)
library(rpart.plot)
library(caret)
# Visualizzare l'albero
print(alberoPP)

# Calcolare l'importanza delle variabili
var_importance <- varImp(alberoPP)

# Stampare i valori di importanza
print(var_importance)

# Plot dell'importanza delle variabili
plot(var_importance, main = "Importanza delle Variabili")

# Estrarre il modello rpart da train()
albero_rpart <- alberoPP$finalModel

# Stampare la struttura dell'albero
print(albero_rpart)

# Visualizzare l'albero con rpart.plot
rpart.plot(albero_rpart, type = 4, extra = 101,split.font = 0.9, ycompress=FALSE, cex=.45, main = "Albero Decisionale")



##### STEP 4 FINALE Naive Bayes####
STEP4_PRED_Naive <- predict(Naive_BayesPP, newdata = dati_validation, type = "prob")
head(STEP4_PRED_Naive)
df=data.frame(cbind(dati_validation$Diabetes_binary , STEP4_PRED_Naive))
head(df)
colnames(df)=c("Class","Probc1","Probc0") # Class+ se diabetico o no in validation, porbabilita di essre c0 o c1 utilizzando le predict
head(df)# ottengo classe 

# seleziono prime due colonne target osservato e probabilita evento di interesse
df=df[,1:2]
head(df) 

library(dplyr)

thresholds <- seq(from = 0, to = 1, by = 0.01)
prop_table <- data.frame(threshold = thresholds, prop_true_c1 = NA,  prop_true_c0 = NA, true_c1 = NA,  true_c0 = NA ,fn_c1=NA)

for (threshold in thresholds) {
  pred <- ifelse(df$Probc1 > threshold, "c1", "c0")  # be careful here!!!
  pred_t <- ifelse(pred == df$Class, TRUE, FALSE)
  
  group <- data.frame(df, "pred" = pred_t) %>%
    group_by(Class, pred) %>%
    dplyr::summarise(n = n())
  
  group_c1 <- filter(group, Class == "c1")
  
  true_c1=sum(filter(group_c1, pred == TRUE)$n)
  prop_c1 <- sum(filter(group_c1, pred == TRUE)$n) / sum(group_c1$n)
  
  prop_table[prop_table$threshold == threshold, "prop_true_c1"] <- prop_c1
  prop_table[prop_table$threshold == threshold, "true_c1"] <- true_c1
  
  fn_c1=sum(filter(group_c1, pred == FALSE)$n)
  # true M predicted as R
  prop_table[prop_table$threshold == threshold, "fn_c1"] <- fn_c1
  
  
  group_c0 <- filter(group, Class == "c0")
  
  true_c0=sum(filter(group_c0, pred == TRUE)$n)
  prop_c0 <- sum(filter(group_c0, pred == TRUE)$n) / sum(group_c0$n)
  
  prop_table[prop_table$threshold == threshold, "prop_true_c0"] <- prop_c0
  prop_table[prop_table$threshold == threshold, "true_c0"] <- true_c0
  
}

head(prop_table, n=10)


prop_table$n=nrow(dati_validation)
prop_table$fp_c1=nrow(dati_validation)-prop_table$true_c0-prop_table$true_c1-prop_table$fn_c1 # falsi positivi
prop_table$acc=(prop_table$true_c0+prop_table$true_c1)/nrow(dati_validation) # accuracy
# precision= veri c1 sul totale previsti come c1
prop_table$prec_c1=prop_table$true_c1/(prop_table$true_c1+prop_table$fp_c1)
prop_table$F1=2*(prop_table$prop_true_c1*prop_table$prec_c1)/(prop_table$prop_true_c1+prop_table$prec_c1)



tail(prop_table)
# inputazione dati per non avere NaN zero denominatore manda valori a infinito
library(Hmisc)
prop_table$prec_c1=impute(prop_table$prec_c1, 1)
prop_table$F1=impute(prop_table$F1, 0)
tail(prop_table)
# elimino colonne conteggi che non mi servono 
prop_table2 = prop_table[,-c(4:8)] 

library(dplyr)
library(tidyr)
head(prop_table2)
# impilo metriche 
gathered=prop_table2 %>%
  gather(x, y, prop_true_c1:F1)

head(gathered)
#faccio plot metriche 
library(ggplot2)
gathered %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(y = "measures",
       color = "M: event\nR: nonevent")
# plot mi da informazioni su soglia da scegliere per validation 
# precision metrica da prediligire ovvero percentUale di c1 veri sul totale di previsti c1 
# vorro quindi soglia precision alta 0,95
# zoom
gathered %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(y = "measures",
       color = "M: event\n R: nonevent") +
  coord_cartesian(xlim = c(0.10, 0.25))

# applico soglia sul dataset validation 
df$decision=ifelse(df$Probc1>0.22,"c1","c0")

# matrice confusione
table(df$Class,df$decision)


confusionMatrix(as.factor(df$decision),df$Class, positive = "c1")
#Pos Pred Value (precision) : 1.00000000 =a/a+c  a  c
#                                                b  d
table(dati_step_4$Diabetes_binary)

head(df,n=20)
