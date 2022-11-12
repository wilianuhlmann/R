Aula de R
21/10/2022
install.packages(c('caret','rpart','rpart','sqldf'))
install.packages(c('forecast'))
install.packages(c('rattle'))
library(caret)
library(rpart)
library(rattle)
library(sqldf)
library(forecast)
# y target
# x variaveis preditoras
treinamento <- createDataPartition(credito_scoring$ID, p=.7, list=F, times=1)
amostra_treinamento <- credito_scoring[treinamento,]
dim(amostra_treinamento)
amostra_teste <-credito_scoring[treinamento,]
amostra_teste
dim(amostra_teste)
################################ criar modelo
###############################
# til buscando relação entre variaveis
table(credito_scoring$Cargo)
trn <- rpart(
  Rank_credito ~ Cargo+Pagamento+Faixa_Etaria+Cartao_Credito,
  data=amostra_treinamento,
  method = 'class',
  parms = list(split = 'gini'),
  minsplit = 2,
  minbucket = 1
)
trn
fancyRpartPlot(trn) # rattle
table(amostra_treinamento$Rank_credito)
path.rpart(trn,node=2)
path.rpart(trn,node=3)
path.rpart(trn,node=4)
path.rpart(trn,node=5)
path.rpart(trn,node=6)
path.rpart(trn,node=7)
path.rpart(trn,node=8)
path.rpart(trn,node=10)
path.rpart(trn,node=11)
amostra_treinamento$prob <- predict(trn, newdata = amostra_treinamento, type = 'prob')
amostra_treinamento$class <- predict(trn, newdata = amostra_treinamento, type = 'class')
View(amostra_treinamento)
tabela <- table(amostra_treinamento$Rank_credito,amostra_treinamento$class)
tabela
acuracia <- (tabela[1]+tabela[4])/sum(tabela)
acuracia
####### prevendo o modelo ##############
amostra_teste$prob <- predict(trn, newdata = amostra_teste, type = 'prob')
amostra_teste$class <- predict(trn, newdata = amostra_teste, type = 'class')
tabela1 <- table(amostra_teste$Rank_credito,amostra_teste$class)
View(amostra_teste)
tabela1
acuracia1 <- (tabela1[1]+tabela1[4])/sum(tabela1)
acuracia1
install.packages('e1071')
library(e1071)
confusionMatrix(factor(amostra_treinamento$Rank_credito),factor(amostra_treinamento$cla
                                                                ss))
############ series temporais ###################
data() # procurar datasets disponiveis no R
AirPassengers
plot(AirPassengers)
library(forecast)
modelo <- auto.arima(AirPassengers)
previsao <-forecast(modelo, h=12)
previsao
View(AirPassengers)
plot(previsao)
############ sqldf ###############
library(sqldf)
masculino <-sqldf("select * from Banco where sexo = 'Masculino'")
masculino
table(Banco$sexo)
table(masculino$sexo)
media1 <- sqldf('select AVG(salário) from Banco')
media1
mean(Banco$salário)