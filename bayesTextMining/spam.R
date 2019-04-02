
# Adaptado de 
#title: "SMS Spam Classification"
#author: "Deepal DSilva"
#date: "July 10, 2018"
# URL https://github.com/dsilvadeepal/Data-Analysis-Projects-In-R/blob/master/Spam%20Text%20Classification/SMS%20Spam%20Classification_Naive%20Bayes.Rmd

# Bibliotecas nencessárias
#install.packages(c("tm","SnowballC", "wordcloud", "RColorBrewer","e1071","caret"))

library(tm) 					# Para o processamento de texto
library(SnowballC)
library(wordcloud)
library(RColorBrewer) 
library(e1071)         #Para Naive Bayes
library(caret)         #Para a "confusion matrix"



# Carregando os dados
sms_raw <- read.csv("spam.csv", encodig="latin1")

#1. Explorando e visualizando os dados

## Há 5 colunas, mas apenas 2 importam. A primeira indicar se o email é um SPAM ou um HAM, ou seja, um conteúdo útil
head(sms_raw)

## Manteremos apenas as 2 primeiras colunas e a renomearemos

sms_raw <- sms_raw[, 1:2]
colnames(sms_raw) <- c("Tag", "Msg")
str(sms_raw)
table(sms_raw$Tag)
prop.table(table(sms_raw$Tag))

## 87% das mensagens são válidas, 13% são spam

#1.2 Visualizando usando word clouds
spam <- subset(sms_raw, Tag == "spam")
wordcloud(spam$Msg, max.words = 60, colors = brewer.pal(7, "Paired"), random.order = FALSE)
ham <- subset(sms_raw, Tag == "ham")
wordcloud(ham$Msg, max.words = 60, colors = brewer.pal(7, "Paired"), random.order = FALSE)


#2. Preprocessamento dos dados

##Limpeza dos dados, onde serão removidos números, letras maiúsculas, pontuação, "stopwords" 
##e aplicação de "stemming"
## Será criada uma matriz de termos dos documentos ( Document Term Matrix)

## VectorSource() cria um documento para cada mensagem de sms e Vcorpus cria um corpus a partir de VectorSource
sms_corpus <- VCorpus(VectorSource(sms_raw$Msg))
sms_dtm <- DocumentTermMatrix(sms_corpus, control = 
                                 list(tolower = TRUE,
                                      removeNumbers = TRUE,
                                      stopwords = TRUE,
                                      removePunctuation = TRUE,
                                      stemming = TRUE))
dim(sms_dtm)
##Cria separa os dados entre dados de treino e dados de teste, onde cada um terá 80% e 20% dos registros, respectivamente. Os dados já estão ordenados aleatoriamente.
##Conjunto de treino
sms_dtm_train <- sms_dtm[1:4457, ]
##Conjunto de teste
sms_dtm_test <- sms_dtm[4458:5572, ]
##Rótulos de treino
sms_train_labels <- sms_raw[1:4457, ]$Tag
#Rótulos de teste
sms_test_labels <- sms_raw[4458:5572, ]$Tag
##Verificando as proporções dos dados de treino
prop.table(table(sms_train_labels))
##Verificando as proporções dos dados de teste
prop.table(table(sms_test_labels))


## Reduzindo a dimensionalidade da Matriz
## Como a matriz de termos possue uma coluna para cada termo encontrado, os dados são excessivamente esparsos 
## Selecionaremos um vetor com as palavras mais frequentes, que será usado para filtrar a matriz
## facilitando o aprendizado pelo algoritmo Naive Bayes

## Definindo uma frequência mínima
threshold <- 0.1
min_freq = round(sms_dtm$nrow*(threshold/100),0)
min_freq
## Criando o vetor com as palavras mais frequentes
freq_words <- findFreqTerms(x = sms_dtm, lowfreq = min_freq)
str(freq_words)

## Chegamos a um vetor de 1260 palavras mais frequentes

## Filtramos a matriz ...
sms_dtm_freq_train <- sms_dtm_train[ , freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , freq_words]
dim(sms_dtm_freq_train)

## Como o Naive Bayes é treinado com dados categóricos, convertemos os dados numéricos para "Yes" ou "No" caso estejam presentes ou não no documento da matriz

## Função de conversão
convert_values <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}
## Criamos novos conjuntos de dados de treino e teste
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,
                   convert_values)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,
                  convert_values)
str(sms_train)


#3. Treinamento do Classificador Naive Bayes

## Utilizamos a função naiveBayes do pacote e1071
## e criamos um modelo para classificação
sms_classifier <- naiveBayes(sms_train, sms_train_labels)
## Realizamos as previsões a partir do modelo
sms_test_pred <- predict(sms_classifier, sms_test)


#4. Avaliando o modelo

## Avaliação realizada através da "matriz de confusão"

confusionMatrix(data = sms_test_pred, reference = sms_test_labels, positive = "spam", dnn = c("Prediction", "Actual"))

## Foi alcançada a Acurácia de 98.12%. 5 mensagens que eram spam foram classificadas como "ham"



