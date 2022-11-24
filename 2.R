library(readxl)
a <- read_excel("Documentos/MBA/Working R/Entregar/1/Working with R/Cadastral.xlsx")
View(a)

b <- read_excel("Documentos/MBA/Working R/Entregar/1/Working with R/Transacional.xlsx")
View(b)

consolidado <- merge(a,b)
dim(consolidado)
View(consolidado)



# Modelo 1



set.seed(42)
treinamento <- createDataPartition(consolidado$ID, p=.8, list = F)
#View(treinamento)
amostra_treinamento <- consolidado[treinamento,]
#View(amostra_treinamento)
dim(amostra_treinamento)
amostra_teste <- consolidado[treinamento,]  
dim(amostra_teste)

table(consolidado$NumerodeFilhos)
table(consolidado$EstadoCivil)
# Existe um desbalanceamento de classes
table(consolidado$default)
# Listar nome de colunas
colnames(consolidado)
trn <- rpart(
  default1 ~ EstadoCivil+NumerodeFilhos+salario+Atraso+TempodeResidencia,
  data = amostra_treinamento,
  method = 'class',
  parms = list(split = 'gini'),
  minsplit = 2,
  minbucket = 1
)
trn
fancyRpartPlot(trn)
table(amostra_treinamento$default1)
path.rpart(trn,node=2)



#amostra_treinamento$prob <- predict(trn, newdata = amostra_treinamento, type = 'prob')
amostra_treinamento$class <- predict(trn, newdata = amostra_treinamento, type = 'class')
#View(amostra_treinamento)
tabela <- table(amostra_treinamento$default1,amostra_treinamento$class)
tabela
acuracia_m1 <- (tabela[1]+tabela[4])/sum(tabela)
acuracia_m1



# Modelo 2

set.seed(42)
treinamento <- createDataPartition(consolidado$ID, p=.8, list = F)
#View(treinamento)
amostra_treinamento <- consolidado[treinamento,]
#View(amostra_treinamento)
dim(amostra_treinamento)
amostra_teste <- consolidado[treinamento,]  
dim(amostra_teste)

table(consolidado$NumerodeFilhos)
table(consolidado$EstadoCivil)
# Existe um desbalanceamento de classes
table(consolidado$default)
# Listar nome de colunas
colnames(consolidado)
trn <- rpart(
  default1 ~ EstadoCivil+NumerodeFilhos+salario+Atraso,
  data = amostra_treinamento,
  method = 'class',
  parms = list(split = 'gini'),
  minsplit = 2,
  minbucket = 1
)
trn
fancyRpartPlot(trn)
table(amostra_treinamento$default1)
path.rpart(trn,node=2)



#amostra_treinamento$prob <- predict(trn, newdata = amostra_treinamento, type = 'prob')
amostra_treinamento$class <- predict(trn, newdata = amostra_treinamento, type = 'class')
#View(amostra_treinamento)
tabela <- table(amostra_treinamento$default1,amostra_treinamento$class)
tabela
acuracia_m2 <- (tabela[1]+tabela[4])/sum(tabela)
acuracia_m2


# Modelo 3




# Modelo 4




# Modelo 5






