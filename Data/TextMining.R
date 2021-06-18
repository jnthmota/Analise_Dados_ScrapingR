#Preparação de Dados (Dados Ausentes, Outliers, Discretização, Normalização e Seleção de Atributos)
library(dplyr)
library(skimr)
library(funModeling)
library(mice)
library(VIM)
library(arules)
library(MXM)
library(caret)
library(mRMRe)
tmp <- "https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/"
dat1 <- "auto-mpg.data-original"
dat1 <- read.table(paste(tmp, dat1, sep = ""))


#Entendendo a base de dados
dim(dat1)
str(dat1)
glimpse(dat1) #similar ao str, pacote dplyr
summary(dat1)


skim(dat1) #pode ser um complemento da Summary, pacote skimr
df_status(dat1) #pode ser um complemento da Summary, pacote funModeling

describe(dat1)
describe(dat1$V1)

freq(dat1)   # gerando gráficos para variáveis categóricas, pacote funModeling
plot_num(dat1) # gerando gráficos para variáveis numéricas, pacote funModeling


#Imputando dados ausentes

#Imputação simples
dados_imputados = dat1 #DF temporário
colSums(is.na(dados_imputados))
which(is.na(dat1[,1]))
dat1[which(is.na(dat1[,1])),]
mean(dados_imputados$V1,  na.rm = TRUE)
dados_imputados$V1 = impute(dados_imputados$V1, mean) #Imputação pacote hmisc
mean(dados_imputados$V1)
colSums(is.na(dados_imputados))

#imputação múltipla
dados_imputados = dat1 #DF temporário
impute <- mice(dados_imputados, m=3, seed = 123) #Imputação pacote mice
print(impute)
impute$imp$V4
dados_imputados <- complete(impute, 1)
colSums(is.na(dados_imputados))

#Descartando dados ausentes
dados_completos = dat1[complete.cases(dat1), ]
colSums(is.na(dados_completos))
dim(dat1)
dim(dados_completos)

#Identificando outliers
boxplot(dat1[,-c(9)]) #Boxplot com atributos numericos
boxplot(dat1[,-c(9)], plot=FALSE)$out
outliers <- boxplot(dat1$V4, plot=FALSE)$out

IQR(dat1$V1, na.rm = TRUE) #identifica interquartil

temp = summary(dat1$V1)
li = temp[2] - 1.5*IQR(dat1$V1, na.rm = TRUE) #Calcula limite inferior
ls = temp[5] + 1.5*IQR(dat1$V1, na.rm = TRUE)  #Calcula limite superior
li
ls
boxplot(dat1[,1])
dat1[which(dat1$V1 > ls),]

#Tratando outliers

#Remover outiler

dat1 = dat1[-which(dat1$V1 > ls),]
boxplot(dat1[,1])

#Atualizar com o limite de Tukey
summary(dat1$V1)
dat1[which(dat1$V1 > ls),]$V1 = ls
boxplot(dat1[,1])
summary(dat1$V1)

#Discretização
#Não supervisionada
hist(dat1$V1, breaks = 20, main = "Data")
table(discretize(dat1$V1, k = 3)) #Discretização. Pacote arules
hist(dat1$V1, breaks = 20, main = "Frequência igual")
abline(v = discretize(dat1$V1, breaks = 3, 
                      onlycuts = TRUE), col = "red")

#Intervalo igual
table(discretize(dat1$V1, method = "interval", breaks = 3))
hist(dat1$V1, breaks = 20, main = "Intervalo igual")
abline(v = discretize(dat1$V1, method = "interval", breaks = 3, 
                      onlycuts = TRUE), col = "red")

#Supervisionada
table(discretize(dat1$V1, method = "fixed", breaks = c(9, 18, 23, 35, 47), 
                 labels = c("Bebe Muito","Alto Consumo","Normal","Econômico")))
hist(dat1$V1, breaks = 20, main = "Fixed")
abline(v = discretize(dat1$V1, method = "fixed", breaks = c(9, 18, 23, 35, 47),
                      onlycuts = TRUE), col = "red")

#Normalização de dados.
summary(dat1$V1)
datNorm <- dat1
datNorm$V1 = scale(datNorm$V1, center = TRUE, scale = TRUE) #normalização z-score
summary(datNorm$V1)

#Feature selection
#Correlação
cor(dados_completos[,1:8])


#RFE PAcote Caret
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(dados_completos[,2:8], dados_completos[,1], sizes=c(1:8), rfeControl=control)
predictors(results)


#mRMR
dd <- mRMR.data(data = dados_completos[,-c(9)])
fs <- mRMR.classic(data = dd, target_indices = c(1), feature_count = 5)
solutions(fs)

#MMPC Pacote MXM
target = 1
PC=MMPC(dados_completos[,target], dados_completos[,-c(target,9)], max_k = 3, 
        threshold = 0.05, test = "auto", ini = NULL, 
        wei = NULL, user_test = NULL, hash = FALSE, 
        hashObject = NULL, 
        ncores = 3, backward = TRUE)
print(PC@selectedVarsOrder)



