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


#Analisando dados Ausentes


