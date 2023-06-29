install.packages("tibble")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("RSNNS")
install.packages("class")
install.packages("tree")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tibble)
library(readr)
library(RSNNS)
library(class)
library(tree)

## 1 - Devido ao tamanho da base de dados,
## para reduzir a chance do modelo ser enviesado
#e à divisão de 60/40 nos dados da base, será utilizado o método K-Fold Cross Validation.

file <- "C:\\Users\\hiago\\OneDrive\\Área de Trabalho\\Trabalho 3 IA\\baseCompleta.csv"
dados <- read.csv(file)

# Defina o número de partições
k <- 5

# Embaralhe as linhas do dataframe
set.seed(123)
df_embaralhado <- dados[sample(nrow(dados)), ]

# Calcule o tamanho de cada partição
tamanho_particao <- ceiling(nrow(df_embaralhado) / k)

# Crie as partições
particoes <- split(df_embaralhado, rep(1:k, each = tamanho_particao, length.out = nrow(df_embaralhado)))

# Acesse as partições individualmente
particao1 <- particoes[[1]]
particao2 <- particoes[[2]]
particao3 <- particoes[[3]]
particao4 <- particoes[[4]]
particao5 <- particoes[[5]]

#Aqui, realizarei as combinações de treinamento e teste. Fundir as partições 2, 3, 4 e 5 em uma única partição
particaoTreinamento1 <- rbind(particoes[[2]], particoes[[3]], particoes[[4]], particoes[[5]])
particaoTreinamento2 <- rbind(particoes[[1]], particoes[[3]], particoes[[4]], particoes[[5]])
particaoTreinamento3 <- rbind(particoes[[1]], particoes[[2]], particoes[[4]], particoes[[5]])
particaoTreinamento4 <- rbind(particoes[[1]], particoes[[2]], particoes[[3]], particoes[[5]])
particaoTreinamento5 <- rbind(particoes[[1]], particoes[[2]], particoes[[3]], particoes[[4]])

targetTrain1 <- particaoTreinamento1$Diagnostic
particaoTreinamento1$Diagnostic <- NULL
targetTest1 <- particao1$Diagnostic
particao1$Diagnostic <- NULL

targetTrain2 <- particaoTreinamento2$Diagnostic
particaoTreinamento2$Diagnostic <- NULL
targetTest2 <- particao2$Diagnostic
particao2$Diagnostic <- NULL

targetTrain3 <- particaoTreinamento3$Diagnostic
particaoTreinamento3$Diagnostic <- NULL
targetTest3 <- particao3$Diagnostic
particao3$Diagnostic <- NULL

targetTrain4 <- particaoTreinamento4$Diagnostic
particaoTreinamento4$Diagnostic <- NULL
targetTest4 <- particao4$Diagnostic
particao4$Diagnostic <- NULL

targetTrain5 <- particaoTreinamento5$Diagnostic
particaoTreinamento5$Diagnostic <- NULL
targetTest5 <- particao5$Diagnostic
particao5$Diagnostic <- NULL

## 2 - Com relação às métricas, todas seriam importantes. Acurácia, pela base poder ser considerada como "Balanceada", 
## Precisão por ser relacionado à diagnósticos de câncer. 
## Recall, devido ao foco na identificação da classe minoritária (1, ou Malignos) 

## 3 - O baseline a ser utilizado será o Algoritmo de base minoritária, devido ao foco na classe minoritária, que seriam os malignos.

## 4 -

############################
## KNN
############################

limiar <- 5
KNNTFPMedio = 0
KNNPrecisaoMedio = 0
KNNAcuraciaMedia = 0
KNNRecallMedio = 0

for (i in 1:5) {
  # Criar os nomes das variáveis dinamicamente
  targetTest <- paste0("targetTest", i)
  particao <- paste0("particao", i)
  targetTrain <- paste0("targetTrain", i)
  particaoTreinamento <- paste0("particaoTreinamento", i)
  
  # Criar o data frame com as variáveis adequadas
  x <- data.frame(get(particao), y = as.factor(get(targetTrain)))
  
  # Executar o algoritmo KNN
  model <- class::knn(train = get(particaoTreinamento), test = get(particao), cl = get(targetTrain), k = 3)
  predsVal <- as.numeric(as.character(model))
  predVal <- ifelse(predsVal > limiar, 1, 0)
  
  # Calcular a matriz de confusão
  tp <- sum((get(targetTest) == 1) & (predVal == 1))
  fp <- sum((get(targetTest) == 0) & (predVal == 1))
  tn <- sum((get(targetTest) == 0) & (predVal == 0))
  fn <- sum((get(targetTest) == 1) & (predVal == 0))
  confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))
  
  # Imprimir a matriz de confusão
  print(confusionMat)
  print(fp) #Falsos Positivos
  print(tp) #Verdadeiros Positivos
  print(fn) #Falsos Negativos
  print(tn) #Verdadeiros Negativos
  TFP = (fp/(tn+fp))
  print(TFP) #TFP
  
  Precisao = (tp/(tp+fn)) #Precisão
  print(Precisao)
  
  Acuracia = (tp+tn/(tp+tn+fp+fn)) #Acurácia
  print(Acuracia)
  
  Recall = (tp/(tp+fn)) #Recall
  print(Recall)
  
  KNNTFPMedio = KNNTFPMedio + TFP ##TFP Medio
  KNNPrecisaoMedio = KNNPrecisaoMedio + Precisao #Precisao Media
  KNNAcuraciaMedia = KNNAcuraciaMedia + Acuracia #Acuracia Media
  KNNRecallMedio = KNNRecallMedio + Recall #Recall Medio
}
print((KNNTFPMedio)/5) #TFP Medio
print((KNNPrecisaoMedio)/5) #Precisao Media
print((KNNAcuraciaMedia)/5) #Acuracia Media
print((KNNRecallMedio)/5) #Recall Medio

## 5 - 

############################
## DECISION TREE
############################
DTTFPMedio = 0
DTPrecisaoMedio = 0
DTAcuraciaMedia = 0
DTRecallMedio = 0
## PRIMEIRA COMBINAÇÃO/LOOP
model <- tree(targetTrain1 ~ ., particaoTreinamento1)
predsVal <- predict(model, particao1)
predVal <- ifelse (predsVal > 0.5, 1, 0)

# Calcular a matriz de confusão
tp <- sum((targetTest1 == 1) & (predVal == 1))
fp <- sum((targetTest1 == 0) & (predVal == 1))
tn <- sum((targetTest1 == 0) & (predVal == 0))
fn <- sum((targetTest1 == 1) & (predVal == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

# Imprimir a matriz de confusão
print(confusionMat)
print(fp) #Falsos Positivos
print(tp) #Verdadeiros Positivos
print(fn) #Falsos Negativos
print(tn) #Verdadeiros Negativos
TFP = (fp/(tn+fp))
print(TFP) #TFP

Precisao = (tp/(tp+fn)) #Precisão
print(Precisao)

Acuracia = (tp+tn/(tp+tn+fp+fn)) #Acurácia
print(Acuracia)

Recall = (tp/(tp+fn)) #Recall
print(Recall)

DTTFPMedio = DTTFPMedio + TFP ##TFP Medio
DTPrecisaoMedio = DTPrecisaoMedio + Precisao ##Precisao Media
DTAcuraciaMedia = DTAcuraciaMedia + Acuracia ##Acuracia Media
DTRecallMedio = DTRecallMedio + Recall ##Recall Medio

##SEGUNDA COMBINAÇÃO/LOOP
model <- tree(targetTrain2 ~ ., particaoTreinamento2)
predsVal <- predict(model, particao2)
predVal <- ifelse (predsVal > 0.5, 1, 0)

# Calcular a matriz de confusão
tp <- sum((targetTest2 == 1) & (predVal == 1))
fp <- sum((targetTest2 == 0) & (predVal == 1))
tn <- sum((targetTest2 == 0) & (predVal == 0))
fn <- sum((targetTest2 == 1) & (predVal == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

# Imprimir a matriz de confusão
print(confusionMat)
print(fp) #Falsos Positivos
print(tp) #Verdadeiros Positivos
print(fn) #Falsos Negativos
print(tn) #Verdadeiros Negativos
TFP = (fp/(tn+fp))
print(TFP) #TFP

Precisao = (tp/(tp+fn)) #Precisão
print(Precisao)

Acuracia = (tp+tn/(tp+tn+fp+fn)) #Acurácia
print(Acuracia)

Recall = (tp/(tp+fn)) #Recall
print(Recall)

DTTFPMedio = DTTFPMedio + TFP ##TFP Medio
DTPrecisaoMedio = DTPrecisaoMedio + Precisao ##Precisao Media
DTAcuraciaMedia = DTAcuraciaMedia + Acuracia ##Acuracia Media
DTRecallMedio = DTRecallMedio + Recall ##Recall Medio

## TERCEIRA COMBINAÇÃO/LOOP
model <- tree(targetTrain3 ~ ., particaoTreinamento3)
predsVal <- predict(model, particao3)
predVal <- ifelse (predsVal > 0.5, 1, 0)

# Calcular a matriz de confusão
tp <- sum((targetTest3 == 1) & (predVal == 1))
fp <- sum((targetTest3 == 0) & (predVal == 1))
tn <- sum((targetTest3 == 0) & (predVal == 0))
fn <- sum((targetTest3 == 1) & (predVal == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

# Imprimir a matriz de confusão
print(confusionMat)
print(fp) #Falsos Positivos
print(tp) #Verdadeiros Positivos
print(fn) #Falsos Negativos
print(tn) #Verdadeiros Negativos
TFP = (fp/(tn+fp))
print(TFP) #TFP

Precisao = (tp/(tp+fn)) #Precisão
print(Precisao)

Acuracia = (tp+tn/(tp+tn+fp+fn)) #Acurácia
print(Acuracia)

Recall = (tp/(tp+fn)) #Recall
print(Recall)

DTTFPMedio = DTTFPMedio + TFP ##TFP Medio
DTPrecisaoMedio = DTPrecisaoMedio + Precisao ##Precisao Media
DTAcuraciaMedia = DTAcuraciaMedia + Acuracia ##Acuracia Media
DTRecallMedio = DTRecallMedio + Recall ##Recall Medio

##QUARTA COMBINAÇÃO/LOOP
model <- tree(targetTrain4 ~ ., particaoTreinamento4)
predsVal <- predict(model, particao4)
predVal <- ifelse (predsVal > 0.5, 1, 0)

# Calcular a matriz de confusão
tp <- sum((targetTest4 == 1) & (predVal == 1))
fp <- sum((targetTest4 == 0) & (predVal == 1))
tn <- sum((targetTest4 == 0) & (predVal == 0))
fn <- sum((targetTest4 == 1) & (predVal == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

# Imprimir a matriz de confusão
print(confusionMat)
print(fp) #Falsos Positivos
print(tp) #Verdadeiros Positivos
print(fn) #Falsos Negativos
print(tn) #Verdadeiros Negativos
TFP = (fp/(tn+fp))
print(TFP) #TFP

Precisao = (tp/(tp+fn)) #Precisão
print(Precisao)

Acuracia = (tp+tn/(tp+tn+fp+fn)) #Acurácia
print(Acuracia)

Recall = (tp/(tp+fn)) #Recall
print(Recall)

DTTFPMedio = DTTFPMedio + TFP ##TFP Medio
DTPrecisaoMedio = DTPrecisaoMedio + Precisao ##Precisao Media
DTAcuraciaMedia = DTAcuraciaMedia + Acuracia ##Acuracia Media
DTRecallMedio = DTRecallMedio + Recall ##Recall Medio

##QUINTA COMBINAÇÃO/LOOP
model <- tree(targetTrain5 ~ ., particaoTreinamento5)
predsVal <- predict(model, particao5)
predVal <- ifelse (predsVal > 0.5, 1, 0)

# Calcular a matriz de confusão
tp <- sum((targetTest5 == 1) & (predVal == 1))
fp <- sum((targetTest5 == 0) & (predVal == 1))
tn <- sum((targetTest5 == 0) & (predVal == 0))
fn <- sum((targetTest5 == 1) & (predVal == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

# Imprimir a matriz de confusão
print(confusionMat)
print(fp) #Falsos Positivos
print(tp) #Verdadeiros Positivos
print(fn) #Falsos Negativos
print(tn) #Verdadeiros Negativos
TFP = (fp/(tn+fp))
print(TFP) #TFP

Precisao = (tp/(tp+fn)) #Precisão
print(Precisao)

Acuracia = (tp+tn/(tp+tn+fp+fn)) #Acurácia
print(Acuracia)

Recall = (tp/(tp+fn)) #Recall
print(Recall)

DTTFPMedio = DTTFPMedio + TFP 
print((DTTFPMedio)/5)##TFP Medio
DTPrecisaoMedio = DTPrecisaoMedio + Precisao 
print((DTPrecisaoMedio)/5)##Precisao Media
DTAcuraciaMedia = DTAcuraciaMedia + Acuracia 
print((DTAcuraciaMedia)/5)##Acuracia Media
DTRecallMedio = DTRecallMedio + Recall 
print((DTRecallMedio)/5)##Recall Medio

## 6 - 
############################
## MLP
############################

MLPTFPMedio = 0
MLPPrecisaoMedio = 0
MLPAcuraciaMedia = 0
MLPRecallMedio = 0

##PRIMEIRA COMBINAÇÃO/LOOP
model <- mlp(	x = particaoTreinamento1, 
              y = targetTrain1, 
              size = 5, 
              learnFuncParams = c(0.1), 
              maxit = 100, 
              inputsTest = particao1, 
              targetsTest = targetTest1)
predsVal <- predict(model,particao1)
predVal <- ifelse (predsVal > 0.5, 1, 0)

# Calcular a matriz de confusão
tp <- sum((targetTest1 == 1) & (predVal == 1))
fp <- sum((targetTest1 == 0) & (predVal == 1))
tn <- sum((targetTest1 == 0) & (predVal == 0))
fn <- sum((targetTest1 == 1) & (predVal == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

# Imprimir a matriz de confusão
print(confusionMat)
print(fp) #Falsos Positivos
print(tp) #Verdadeiros Positivos
print(fn) #Falsos Negativos
print(tn) #Verdadeiros Negativos
TFP = (fp/(tn+fp))
print(TFP) #TFP

Precisao = (tp/(tp+fn)) #Precisão
print(Precisao)

Acuracia = (tp+tn/(tp+tn+fp+fn)) #Acurácia
print(Acuracia)

Recall = (tp/(tp+fn)) #Recall
print(Recall)

MLPTFPMedio = MLPTFPMedio + TFP ##TFP Medio
MLPPrecisaoMedio = MLPPrecisaoMedio + Precisao ##Precisao Media
MLPAcuraciaMedia = MLPAcuraciaMedia + Acuracia ##Acuracia Media
MLPRecallMedio = MLPRecallMedio + Recall ##Recall Medio

##SEGUNDA COMBINAÇÃO/LOOP
model <- mlp(	x = particaoTreinamento2, 
              y = targetTrain2, 
              size = 5, 
              learnFuncParams = c(0.1), 
              maxit = 100, 
              inputsTest = particao2, 
              targetsTest = targetTest2)
predsVal <- predict(model,particao2)
predVal <- ifelse (predsVal > 0.5, 1, 0)

# Calcular a matriz de confusão
tp <- sum((targetTest2 == 1) & (predVal == 1))
fp <- sum((targetTest2 == 0) & (predVal == 1))
tn <- sum((targetTest2 == 0) & (predVal == 0))
fn <- sum((targetTest2 == 1) & (predVal == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

# Imprimir a matriz de confusão
print(confusionMat)
print(fp) #Falsos Positivos
print(tp) #Verdadeiros Positivos
print(fn) #Falsos Negativos
print(tn) #Verdadeiros Negativos
TFP = (fp/(tn+fp))
print(TFP) #TFP

Precisao = (tp/(tp+fn)) #Precisão
print(Precisao)

Acuracia = (tp+tn/(tp+tn+fp+fn)) #Acurácia
print(Acuracia)

Recall = (tp/(tp+fn)) #Recall
print(Recall)

MLPTFPMedio = MLPTFPMedio + TFP ##TFP Medio
MLPPrecisaoMedio = MLPPrecisaoMedio + Precisao ##Precisao Media
MLPAcuraciaMedia = MLPAcuraciaMedia + Acuracia ##Acuracia Media
MLPRecallMedio = MLPRecallMedio + Recall ##Recall Medio

##TERCEIRA COMBINAÇÃO/LOOP
model <- mlp(	x = particaoTreinamento3, 
              y = targetTrain3, 
              size = 5, 
              learnFuncParams = c(0.1), 
              maxit = 100, 
              inputsTest = particao3, 
              targetsTest = targetTest3)
predsVal <- predict(model,particao3)
predVal <- ifelse (predsVal > 0.5, 1, 0)

# Calcular a matriz de confusão
tp <- sum((targetTest3 == 1) & (predVal == 1))
fp <- sum((targetTest3 == 0) & (predVal == 1))
tn <- sum((targetTest3 == 0) & (predVal == 0))
fn <- sum((targetTest3 == 1) & (predVal == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

# Imprimir a matriz de confusão
print(confusionMat)
print(fp) #Falsos Positivos
print(tp) #Verdadeiros Positivos
print(fn) #Falsos Negativos
print(tn) #Verdadeiros Negativos
TFP = (fp/(tn+fp))
print(TFP) #TFP

Precisao = (tp/(tp+fn)) #Precisão
print(Precisao)

Acuracia = (tp+tn/(tp+tn+fp+fn)) #Acurácia
print(Acuracia)

Recall = (tp/(tp+fn)) #Recall
print(Recall)

MLPTFPMedio = MLPTFPMedio + TFP ##TFP Medio
MLPPrecisaoMedio = MLPPrecisaoMedio + Precisao ##Precisao Media
MLPAcuraciaMedia = MLPAcuraciaMedia + Acuracia ##Acuracia Media
MLPRecallMedio = MLPRecallMedio + Recall ##Recall Medio

##QUARTA COMBINAÇÃO/LOOP
model <- mlp(	x = particaoTreinamento4, 
              y = targetTrain4, 
              size = 5, 
              learnFuncParams = c(0.1), 
              maxit = 100, 
              inputsTest = particao4, 
              targetsTest = targetTest4)
predsVal <- predict(model,particao4)
predVal <- ifelse (predsVal > 0.5, 1, 0)

# Calcular a matriz de confusão
tp <- sum((targetTest4 == 1) & (predVal == 1))
fp <- sum((targetTest4 == 0) & (predVal == 1))
tn <- sum((targetTest4 == 0) & (predVal == 0))
fn <- sum((targetTest4 == 1) & (predVal == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

# Imprimir a matriz de confusão
print(confusionMat)
print(fp) #Falsos Positivos
print(tp) #Verdadeiros Positivos
print(fn) #Falsos Negativos
print(tn) #Verdadeiros Negativos
TFP = (fp/(tn+fp))
print(TFP) #TFP

Precisao = (tp/(tp+fn)) #Precisão
print(Precisao)

Acuracia = (tp+tn/(tp+tn+fp+fn)) #Acurácia
print(Acuracia)

Recall = (tp/(tp+fn)) #Recall
print(Recall)

MLPTFPMedio = MLPTFPMedio + TFP ##TFP Medio
MLPPrecisaoMedio = MLPPrecisaoMedio + Precisao ##Precisao Media
MLPAcuraciaMedia = MLPAcuraciaMedia + Acuracia ##Acuracia Media
MLPRecallMedio = MLPRecallMedio + Recall ##Recall Medio

##QUINTA COMBINAÇÃO/LOOP
model <- mlp(	x = particaoTreinamento5, 
              y = targetTrain5, 
              size = 5, 
              learnFuncParams = c(0.1), 
              maxit = 100, 
              inputsTest = particao5, 
              targetsTest = targetTest5)
predsVal <- predict(model,particao5)
predVal <- ifelse (predsVal > 0.5, 1, 0)

# Calcular a matriz de confusão
tp <- sum((targetTest5 == 1) & (predVal == 1))
fp <- sum((targetTest5 == 0) & (predVal == 1))
tn <- sum((targetTest5 == 0) & (predVal == 0))
fn <- sum((targetTest5 == 1) & (predVal == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

# Imprimir a matriz de confusão
print(confusionMat)
print(fp) #Falsos Positivos
print(tp) #Verdadeiros Positivos
print(fn) #Falsos Negativos
print(tn) #Verdadeiros Negativos
TFP = (fp/(tn+fp))
print(TFP) #TFP

Precisao = (tp/(tp+fn)) #Precisão
print(Precisao)

Acuracia = (tp+tn/(tp+tn+fp+fn)) #Acurácia
print(Acuracia)

Recall = (tp/(tp+fn)) #Recall
print(Recall)

MLPTFPMedio = MLPTFPMedio + TFP ##TFP Medio
MLPPrecisaoMedio = MLPPrecisaoMedio + Precisao ##Precisao Media
MLPAcuraciaMedia = MLPAcuraciaMedia + Acuracia ##Acuracia Media
MLPRecallMedio = MLPRecallMedio + Recall ##Recall Medio

print((MLPTFPMedio)/5) ##TFP Medio
print((MLPPrecisaoMedio)/5) ##Precisao Media
print((MLPAcuraciaMedia)/5) ##Acuracia Media
print((MLPRecallMedio)/5) ##Recall Medio

## 7 - Aplicando o baseline de classe minoritária:

BTFPMedio = 0
BPrecisaoMedio = 0
BAcuraciaMedia = 0
BRecallMedio = 0

##Primeira combinação/loop
baselineMinoritaria <- rep(1, nrow(dados))

tp <- sum((targetTest1 == 1) & (baselineMinoritaria == 1))
fp <- sum((targetTest1 == 0) & (baselineMinoritaria == 1))
tn <- sum((targetTest1 == 0) & (baselineMinoritaria == 0))
fn <- sum((targetTest1 == 1) & (baselineMinoritaria == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

print(confusionMat)
print(fp) #Falsos Positivos
print(tp) #Verdadeiros Positivos
print(fn) #Falsos Negativos
print(tn) #Verdadeiros Negativos
TFPBaseline = (fp/(tn+fp))
print(TFPBaseline) #TFP

PrecisaoBaseline = (tp/(tp+fn)) #Precisão
print(PrecisaoBaseline)

AcuraciaBaseline = ((tp+tn)/(tp+tn+fp+fn)) #Acurácia
print(AcuraciaBaseline)

RecallBaseline = (tp/(tp+fn)) #Recall
print(RecallBaseline)

BTFPMedio = BTFPMedio + TFP ##TFP Medio
BPrecisaoMedio = BPrecisaoMedio + Precisao ##Precisao Media
BAcuraciaMedia = BAcuraciaMedia + Acuracia ##Acuracia Media
BRecallMedio = BRecallMedio + Recall ##Recall Medio

##Segunda combinação/loop
baselineMinoritaria <- rep(1, nrow(dados))

tp <- sum((targetTest2 == 1) & (baselineMinoritaria == 1))
fp <- sum((targetTest2 == 0) & (baselineMinoritaria == 1))
tn <- sum((targetTest2 == 0) & (baselineMinoritaria == 0))
fn <- sum((targetTest2 == 1) & (baselineMinoritaria == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

print(confusionMat)
print(fp) #Falsos Positivos
print(tp) #Verdadeiros Positivos
print(fn) #Falsos Negativos
print(tn) #Verdadeiros Negativos
TFPBaseline = (fp/(tn+fp))
print(TFPBaseline) #TFP

PrecisaoBaseline = (tp/(tp+fn)) #Precisão
print(PrecisaoBaseline)

AcuraciaBaseline = ((tp+tn)/(tp+tn+fp+fn)) #Acurácia
print(AcuraciaBaseline)

RecallBaseline = (tp/(tp+fn)) #Recall
print(RecallBaseline)

BTFPMedio = BTFPMedio + TFP ##TFP Medio
BPrecisaoMedio = BPrecisaoMedio + Precisao ##Precisao Media
BAcuraciaMedia = BAcuraciaMedia + Acuracia ##Acuracia Media
BRecallMedio = BRecallMedio + Recall ##Recall Medio

##Terceira combinação/loop
baselineMinoritaria <- rep(1, nrow(dados))

tp <- sum((targetTest3 == 1) & (baselineMinoritaria == 1))
fp <- sum((targetTest3 == 0) & (baselineMinoritaria == 1))
tn <- sum((targetTest3 == 0) & (baselineMinoritaria == 0))
fn <- sum((targetTest3 == 1) & (baselineMinoritaria == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

print(confusionMat)
print(fp) #Falsos Positivos
print(tp) #Verdadeiros Positivos
print(fn) #Falsos Negativos
print(tn) #Verdadeiros Negativos
TFPBaseline = (fp/(tn+fp))
print(TFPBaseline) #TFP

PrecisaoBaseline = (tp/(tp+fn)) #Precisão
print(PrecisaoBaseline)

AcuraciaBaseline = ((tp+tn)/(tp+tn+fp+fn)) #Acurácia
print(AcuraciaBaseline)

RecallBaseline = (tp/(tp+fn)) #Recall
print(RecallBaseline)

BTFPMedio = BTFPMedio + TFP ##TFP Medio
BPrecisaoMedio = BPrecisaoMedio + Precisao ##Precisao Media
BAcuraciaMedia = BAcuraciaMedia + Acuracia ##Acuracia Media
BRecallMedio = BRecallMedio + Recall ##Recall Medio

##Quarta combinação/loop
baselineMinoritaria <- rep(1, nrow(dados))

tp <- sum((targetTest4 == 1) & (baselineMinoritaria == 1))
fp <- sum((targetTest4 == 0) & (baselineMinoritaria == 1))
tn <- sum((targetTest4 == 0) & (baselineMinoritaria == 0))
fn <- sum((targetTest4 == 1) & (baselineMinoritaria == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

print(confusionMat)
print(fp) #Falsos Positivos
print(tp) #Verdadeiros Positivos
print(fn) #Falsos Negativos
print(tn) #Verdadeiros Negativos
TFPBaseline = (fp/(tn+fp))
print(TFPBaseline) #TFP

PrecisaoBaseline = (tp/(tp+fn)) #Precisão
print(PrecisaoBaseline)

AcuraciaBaseline = ((tp+tn)/(tp+tn+fp+fn)) #Acurácia
print(AcuraciaBaseline)

RecallBaseline = (tp/(tp+fn)) #Recall
print(RecallBaseline)

BTFPMedio = BTFPMedio + TFP ##TFP Medio
BPrecisaoMedio = BPrecisaoMedio + Precisao ##Precisao Media
BAcuraciaMedia = BAcuraciaMedia + Acuracia ##Acuracia Media
BRecallMedio = BRecallMedio + Recall ##Recall Medio

##Quinta combinação/loop
baselineMinoritaria <- rep(1, nrow(dados))

tp <- sum((targetTest5 == 1) & (baselineMinoritaria == 1))
fp <- sum((targetTest5 == 0) & (baselineMinoritaria == 1))
tn <- sum((targetTest5 == 0) & (baselineMinoritaria == 0))
fn <- sum((targetTest5 == 1) & (baselineMinoritaria == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

print(confusionMat)
print(fp) #Falsos Positivos
print(tp) #Verdadeiros Positivos
print(fn) #Falsos Negativos
print(tn) #Verdadeiros Negativos
TFPBaseline = (fp/(tn+fp))
print(TFPBaseline) #TFP

PrecisaoBaseline = (tp/(tp+fn)) #Precisão
print(PrecisaoBaseline)

AcuraciaBaseline = ((tp+tn)/(tp+tn+fp+fn)) #Acurácia
print(AcuraciaBaseline)

RecallBaseline = (tp/(tp+fn)) #Recall
print(RecallBaseline)

BTFPMedio = BTFPMedio + TFP ##TFP Medio
BPrecisaoMedio = BPrecisaoMedio + Precisao ##Precisao Media
BAcuraciaMedia = BAcuraciaMedia + Acuracia ##Acuracia Media
BRecallMedio = BRecallMedio + Recall ##Recall Medio

print((BTFPMedio)/5) #TFP Médio (Baseline)
print((BPrecisaoMedio)/5) #Precisão Média (Baseline)
print((BAcuraciaMedia)/5) #Acurácia Média (Baseline)
print((BRecallMedio)/5) #Recall Médio (Baseline)

## 8 - Comparando os resultados obtidos:
##---
##KNN
##---
print((KNNTFPMedio/5)) #TFP Médio (KNN)
print((KNNPrecisaoMedio/5)) #Precisão Média (KNN)
print((KNNAcuraciaMedia/5)) #Acuracia Média (KNN)
print((KNNRecallMedio)/5) #Recall Médio (KNN)

##---
##Decision Tree
##---
print((DTTFPMedio)/5) #TFP Médio (Decision Tree)
print((DTPrecisaoMedio)/5) #Precisão Média (Decision Tree)
print((DTAcuraciaMedia)/5) #Acuracia Média (Decision Tree)
print((DTRecallMedio)/5) #Recall Médio (Decision Tree)

##---
##MLP
##---
print((MLPTFPMedio)/5) #TFP Médio (MLP)
print((MLPPrecisaoMedio)/5) #Precisão Média (MLP)
print((MLPAcuraciaMedia)/5) #Acuracia Média (MLP)
print((MLPRecallMedio)/5) #Recall Médio (MLP)

##---
##Algoritmo Baseline
##---
print((BTFPMedio)/5) #TFP Médio (Baseline)
print((BPrecisaoMedio)/5) #Precisão Média (Baseline)
print((BAcuraciaMedia)/5) #Acurácia Média (Baseline)
print((BRecallMedio)/5) #Recall Médio (Baseline)

#O maior (e mais plausível) TFP médio foi o da Decision Tree, enquanto os menores foram zero.
#A maior (e mais plausível) Precisão média foi a Decision Tree, enquanto os menores foram zero.
#As maiores (e mais plausíveis) Acurácias médias foram as da MLP e KNN, enquanto os menores foram zero.
#O maior (e mais plausível) Recall médio foi o da Decision Tree, enquanto os menores foram zero.

#A árvore de decisão foi a que obteve o melhor resultado com relação à recall, precisão e TFP, portanto seria o melhor algoritmo para ser utilizado nessa base, apesar da acurácia.
#O KNN e MLP apresentaram zeros em todos exceto a acurácia Média.

