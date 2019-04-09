install.packages('DMwR')
library(DMwR)
library(dplyr)
data(iris)

#####################################################################
## Validação Cruzada
#####################################################################
# Particionando o iris dataset em x partições
numero_particoes = 10
particoes = cut(seq(1, nrow(iris)), breaks = numero_particoes, labels = FALSE)
print(particoes)

acuracia_acumulada = 0.0

for (i in 1:numero_particoes){
  #Selecionando os elementos da partição i
  indices = which(particoes == i)
  conjuntoTreino = iris[-indices,]
  conjuntoTeste = iris[indices,]
  #k = 5
  nn5 = kNN(Species ~., conjuntoTreino, conjuntoTeste, norm=FALSE, k = 5)
  
  #resultados na matriz de confusão
  print(cat("Partição = ", i,""))
  matriz = table(conjuntoTeste[,'Species'], nn5)
  print(matriz)
  
  #Acurácia = Acertos (representados pela diagonal) / total de classificações
  acuracia = sum(diag(matriz))/sum(matriz) 
  print(cat("Acurácia ", acuracia))
  acuracia_acumulada = acuracia + acuracia_acumulada
}
#Acurácia media = acuracia acumulada / particoes
print(cat("Acurácia média = ", acuracia_acumulada / numero_particoes))


#####################################################################
## Validação Cruzada Estratificada
#####################################################################
data = iris
library(caret)
numero_particoes = 10
# Função createFolds fará o particionamento estratificado, ou seja, mantendo a proporção das classes em cada partição
particoes = createFolds(factor(data$Species), k = numero_particoes, list = FALSE)
data
#juntando a particao ao conjunto de dados
data$particao = particoes
#Verificando a partição 3...
data[data$particao == 3,]

acuracia_acumulada = 0.0
for(i in 1:numero_particoes){
  #Selecionando os elementos da partição i
  indices = which(data$particao == i)
  conjuntoTreino = data[-indices,]
  conjuntoTeste = data[indices,]
  #k = 5
  nn5 = kNN(Species ~., conjuntoTreino, conjuntoTeste, norm=FALSE, k = 5)
  
  #resultados na matriz de confusão
  print(cat("Partição = ", i,""))
  matriz = table(conjuntoTeste[,'Species'], nn5)
  print(matriz)
  
  #Acurácia = Acertos (representados pela diagonal) / total de classificações
  acuracia = sum(diag(matriz))/sum(matriz) 
  print(cat("Acurácia ", acuracia))
  acuracia_acumulada = acuracia + acuracia_acumulada
}

#Acurácia media = acuracia acumulada / particoes
print(cat("Acurácia média = ", acuracia_acumulada / numero_particoes))

#####################################################################
## Tentando melhorar a acurácia selecionando elementos aleatoreamente
#####################################################################

#A Função sample selecionará aleatoreamente os valores do conjunto
indices_aleatorios <- sample(1:nrow(iris))
iris_aleatorio = iris[indices_aleatorios,]

numero_particoes = 4
particoes = cut(seq(1, nrow(iris_aleatorio)), breaks = numero_particoes, labels = FALSE)
print(particoes)

acuracia_acumulada = 0.0

for (i in 1:numero_particoes){
  #Selecionando os elementos da partição i
  indices = which(particoes == i)
  conjuntoTreino = iris_aleatorio[-indices,]
  conjuntoTeste = iris_aleatorio[indices,]
  #k = 5
  nn5 = kNN(Species ~., conjuntoTreino, conjuntoTeste, norm=FALSE, k = 5)
  
  #resultados na matriz de confusão
  print(cat("Partição = ", i,""))
  matriz = table(conjuntoTeste[,'Species'], nn5)
  print(matriz)
  
  #Acurácia = Acertos (representados pela diagonal) / total de classificações
  acuracia = sum(diag(matriz))/sum(matriz) 
  print(cat("Acurácia ", acuracia))
  acuracia_acumulada = acuracia + acuracia_acumulada
}
#Acurácia media = acuracia acumulada / particoes
print(cat("Acurácia média = ", acuracia_acumulada / numero_particoes))



