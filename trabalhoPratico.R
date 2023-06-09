## Questão 1 - Atributo Alvo: Diagnóstico M(Malignant) ou B(Benign)

## Questão 2 - Qualitativo: ID
## Quantitativo: Radius, Texture, Perimeter, Area, Smoothness, Compactness, Concavity, Concave Points, Symmetry, Fractal Dimension
install.packages("tibble")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tibble)
library(readr)
file <- "C:\\Users\\hiago\\OneDrive\\Área de Trabalho\\Base de Dados\\WDBC.dat"

dados <- read.csv(file, header = FALSE, stringsAsFactors = TRUE)
database = rename(dados, ID = V1, Diagnostic = V2, radius1 = V3, texture1 = V4, perimeter1 = V5, area1 = V6, smoothness1 = V7, compactness1 = V8, concavity1 = V9, concave_points1 = V10, symmetry1 = V11, fractal_dimension1 = V12, radius2 = V13, texture2 = V14, perimeter2 = V15, area2 = V16, smoothness2 = V17, compactness2 = V18, concavity2 = V19, concave_points2 = V20, symmetry2 = V21, fractal_dimension2 = V22, radius3 = V23, texture3 = V24, perimeter3 = V25, area3 = V26, smoothness3 = V27, compactness3 = V28, concavity3 = V29, concave_points3 = V30, symmetry3 = V31, fractal_dimension3 = V32)
database
View(database)
names(database)

# Questão 3:
#radius -> Racional
#texture -> Intervalar
#perimeter -> Racional
#area -> Racional
#smoothness -> Intervalar
#compactness -> Racional
#concavity -> Racional
#concave points -> Racional
#symmetry -> Intervalar
#fractal dimension -> Racional

#Questão 4,5 e 6:

#Gráfico das variáveis (Verificação de Outliers):

boxplot(database$radius1, main = "Boxplot: radius1")
boxplot(database$texture1, main = "Boxplot: texture1")
boxplot(database$perimeter1, main = "Boxplot: perimeter1")
boxplot(database$area1, main = "Boxplot: area1")
boxplot(database$smoothness1, main = "Boxplot: smoothness1")
boxplot(database$compactness1, main = "Boxplot: compactness1")
boxplot(database$concavity1, main = "Boxplot: concavity1")
boxplot(database$concave_points1, main = "Boxplot: concave_points1")
boxplot(database$symmetry1, main = "Boxplot: symmetry1")
boxplot(database$fractal_dimension1, main = "Boxplot: fractal_dimension1")
boxplot(database$radius1, main = "Boxplot: radius2")
boxplot(database$texture1, main = "Boxplot: texture2")
boxplot(database$perimeter1, main = "Boxplot: perimeter2")
boxplot(database$area2, main = "Boxplot: area2")
boxplot(database$smoothness2, main = "Boxplot: smoothness2")
boxplot(database$compactness2, main = "Boxplot: compactness2")
boxplot(database$concavity2, main = "Boxplot: concavity2")
boxplot(database$concave_points2, main = "Boxplot: concave_points2")
boxplot(database$symmetry2, main = "Boxplot: symmetry2")
boxplot(database$fractal_dimension2, main = "Boxplot: fractal_dimension2")
boxplot(database$radius3, main = "Boxplot: radius3")
boxplot(database$texture3, main = "Boxplot: texture3")
boxplot(database$perimeter3, main = "Boxplot: perimeter3")
boxplot(database$area3, main = "Boxplot: area3")
boxplot(database$smoothness3, main = "Boxplot: smoothness3")
boxplot(database$compactness3, main = "Boxplot: compactness3")
boxplot(database$concavity3, main = "Boxplot: concavity3")
boxplot(database$concave_points3, main = "Boxplot: concave_points3")
boxplot(database$symmetry3, main = "Boxplot: symmetry3")
boxplot(database$fractal_dimension3, main = "Boxplot: fractal_dimension3")


#Conferindo os boxplots, todas as variáveis quantitativas EXCETO concave_points3 possuem outliers notáveis, portanto é melhor usar mediana

databaseMeans <- colMeans(database[,3:32]) #Média dos valores da base de dados (Valor inflado por Outliers, melhor usar mediana)
format(databaseMeans,scientific = FALSE)

mean(database$concave_points3) #Média de concave_points3

apply(database,2,median) #Mediana dos valores da base de dados
summary(database) #Medidas de localidade da base de dados
summary(database$Diagnostic) #Moda do Atributo Alvo

databaseInterval <- matrix(NA, ncol = ncol(database) - 2, nrow = 1) #Intervalo dos valores da base de dados
for (i in 3:ncol(database)) {
  databaseInterval[1, i-2] <- diff(range(database[, i]))
}
databaseInterval #Intervalo dos valores da base de dados

#Histogramas da base de dados

hist(database$radius1, main = "Intervalo", xlab = "radius1", ylab = "Frequencia")
hist(database$texture1, main = "Intervalo", xlab = "texture1", ylab = "Frequencia")
hist(database$perimeter1, main = "Intervalo", xlab = "perimeter1", ylab = "Frequencia")
hist(database$area1, main = "Intervalo", xlab = "area1", ylab = "Frequencia")
hist(database$smoothness1, main = "Intervalo", xlab = "smoothness1", ylab = "Frequencia")
hist(database$compactness1, main = "Intervalo", xlab = "compactness1", ylab = "Frequencia")
hist(database$concavity1, main = "Intervalo", xlab = "concavity1", ylab = "Frequencia")
hist(database$concave_points1, main = "Intervalo", xlab = "concave_points1", ylab = "Frequencia")
hist(database$symmetry1, main = "Intervalo", xlab = "symmetry1", ylab = "Frequencia")
hist(database$fractal_dimension1, main = "Intervalo", xlab = "fractal_dimension1", ylab = "Frequencia")
hist(database$radius2, main = "Intervalo", xlab = "radius2", ylab = "Frequencia")
hist(database$texture2, main = "Intervalo", xlab = "texture2", ylab = "Frequencia")
hist(database$perimeter2, main = "Intervalo", xlab = "perimeter2", ylab = "Frequencia")
hist(database$area2, main = "Intervalo", xlab = "area2", ylab = "Frequencia")
hist(database$smoothness2, main = "Intervalo", xlab = "smoothness2", ylab = "Frequencia")
hist(database$compactness2, main = "Intervalo", xlab = "compactness2", ylab = "Frequencia")
hist(database$concavity2, main = "Intervalo", xlab = "concavity2", ylab = "Frequencia")
hist(database$concave_points2, main = "Intervalo", xlab = "concave_points2", ylab = "Frequencia")
hist(database$symmetry2, main = "Intervalo", xlab = "symmetry2", ylab = "Frequencia")
hist(database$fractal_dimension2, main = "Intervalo", xlab = "fractal_dimension2", ylab = "Frequencia")
hist(database$radius3, main = "Intervalo", xlab = "radius3", ylab = "Frequencia")
hist(database$texture3, main = "Intervalo", xlab = "texture3", ylab = "Frequencia")
hist(database$perimeter3, main = "Intervalo", xlab = "perimeter3", ylab = "Frequencia")
hist(database$area3, main = "Intervalo", xlab = "area3", ylab = "Frequencia")
hist(database$smoothness3, main = "Intervalo", xlab = "smoothness3", ylab = "Frequencia")
hist(database$compactness3, main = "Intervalo", xlab = "compactness3", ylab = "Frequencia")
hist(database$concavity3, main = "Intervalo", xlab = "concavity3", ylab = "Frequencia")
hist(database$concave_points3, main = "Intervalo", xlab = "concave_points3", ylab = "Frequencia")
hist(database$symmetry3, main = "Intervalo", xlab = "symmetry3", ylab = "Frequencia")
hist(database$fractal_dimension3, main = "Intervalo", xlab = "fractal_dimension3", ylab = "Frequencia")


databaseVariance <- apply(database,2,var) #Cálculo da Variância da base de dados (ID desconsiderado)
databaseVariance <- round(databaseVariance, 5)
format(databaseVariance, 2, scientific = FALSE)

sd(database$radius1)
databaseSd <- apply(database,2,sd)
format(databaseSd, scientific = FALSE) #Desvio Padrão dos valores da base de dados (ID desconsiderado)

#Questão 7

malignantPercentage <- round((212 / 569) * 100,3)
format(malignantPercentage)
benignantPercentage <- round((357/569) * 100,3)
format(benignantPercentage)
#Malignant é a saída de 37.26% da base, enquanto benignant é a saída de 62.74% da base

dados_amostra <- database %>% sample_frac(0.2) #Pela base poder ser considerada como balanceada, será distribuído 20% para teste, e 80% para treinamento
dados_treinamento <- anti_join(database, dados_amostra)
View(dados_amostra)
summary(dados_amostra$Diagnostic)
View(dados_treinamento)
summary(dados_treinamento$Diagnostic)

#Questão 8
#O Atributo ID é desnecessário.
amostraSemID <- dados_amostra[, -which(names(dados_amostra) == "ID")]
treinoSemID <- dados_treinamento[, -which(names(dados_treinamento) == "ID")]

#Questão 9
#Nenhum dos exemplos são desnecessários.

#Questão 10
#Foi realizada a amostragem simplificada, separando 80% para treinamento, e 20% para teste.


#Questão 11
#Não são necessárias aplicações de técnicas do tipo, pois não há problemas de desbalanceamento na base de dados

#Questão 12

amostraDadosNumericos = unique(amostraDadosNumericos) #Remoção de possíveis dados duplicados:

#Há ruídos/outliers, portanto foi utilizado o método do Desvio Padrão:

# Selecionar apenas as colunas numéricas
amostraDadosNumericos <- amostraSemID[, sapply(amostraSemID, is.numeric)]

# Calcular a média e o desvio padrão para cada coluna numérica
amostraMedia <- colMeans(amostraDadosNumericos, na.rm = TRUE)
desvio_padrao <- apply(amostraDadosNumericos, 2, sd, na.rm = TRUE)

# Definir limite para remoção de outliers
limiteDesvioPadrao <- 3

# Identificar linhas com outliers nas colunas numéricas
amostraLinhasComOutliers <- apply(amostraDadosNumericos, 1, function(x) any(abs((x - amostraMedia) / desvio_padrao) > limiteDesvioPadrao))

# Filtrar a base de dados mantendo apenas as linhas sem outliers
amostraSemOutliers <- amostraSemID[!linhasComOutliers, ]


treinoDadosNumericos <- treinoSemID[, sapply(treinoSemID, is.numeric)]

treinoMedia <- colMeans(treinoDadosNumericos, na.rm = TRUE)
treinoDesvioPadrao <- apply(treinoDadosNumericos, 2, sd, na.rm = TRUE)

treinoLinhasComOutliers <- apply(treinoDadosNumericos, 1, function(x) any(abs((x - media) / treinoDesvioPadrao) > limiteDesvioPadrao))

treinoSemOutliers <- treinoSemID[!treinoLinhasComOutliers, ]

#Questão 13
amostraSemOutliers$Diagnostic <- ifelse(amostraSemOutliers$Diagnostic == "M", 1, 0) #Os valores de saída podem ser alterados de simbólico para numérico.
treinoSemOutliers$Diagnostic <- ifelse(treinoSemOutliers$Diagnostic == "M", 1, 0)

#Os valores de ID precisariam ser normalizados, porém já foram removidos por serem um atributo não necessário, na questão 8.

#Questão 14
#Para a redução da dimensionalidade, será realizado o método PCA(Principal Component Analysis):
#O método PCA condensa a informação em várias variáveis, com o objetivo de minimizar a perda de informação.

# Aplicar a PCA na base de dados padronizada
amostraPcaAplicado <- prcomp(amostraSemOutliers, scale = TRUE)

# Verificar a proporção da variância explicada por cada componente
amostraProporcaoVariancia <- amostraPcaAplicado$sdev^2 / sum(amostraPcaAplicado$sdev^2)

# Plotar o gráfico de variância explicada acumulada
plot(cumsum(amostraProporcaoVariancia), xlab = "Número de Componentes (Amostra)", ylab = "Variância Explicada Acumulada (Amostra)", type = "b")

# Determinar o número de componentes a serem retidos com base na variância explicada acumulada
amostraNumComponentes <- which(cumsum(amostraProporcaoVariancia) >= 0.95)[1]

# Extrair os componentes principais
amostraCompPrincipais <- amostraPcaAplicado$x[, 1:amostraNumComponentes]
summary(amostraCompPrincipais)


treinoPcaAplicado <- prcomp(treinoSemOutliers, scale = TRUE)

treinoProporcaoVariancia <- treinoPcaAplicado$sdev^2 / sum(treinoPcaAplicado$sdev^2)

plot(cumsum(treinoProporcaoVariancia), xlab = "Número de Componentes (Treino)", ylab = "Variância Explicada Acumulada (Treino)", type = "b")

treinoNumComponentes <- which(cumsum(treinoProporcaoVariancia) >= 0.95)[1]

treinoCompPrincipais <- treinoPcaAplicado$x[, 1:treinoNumComponentes]
summary(treinoCompPrincipais)

