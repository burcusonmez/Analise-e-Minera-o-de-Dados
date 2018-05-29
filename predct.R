library(datasets)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(class)
library(gmodels)


glibc_balanced <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/random_undersampling/glibc_data_balanced.csv", stringsAsFactors = FALSE)

httpd_balanced <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/random_undersampling/httpd_data_balanced.csv", stringsAsFactors = FALSE)

kernel_balanced <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/random_undersampling/kernel_data_balanced.csv",stringsAsFactors = FALSE)

mozillal_balanced <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/random_undersampling/mozilla_data_balanced.csv", stringsAsFactors = FALSE)

xen_balanced <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/random_undersampling/xen_data_balanced.csv",stringsAsFactors = FALSE)


glibc <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/unbalanced/glibc_data.csv",stringsAsFactors = FALSE)

httpd <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/unbalanced/httpd_data.csv", stringsAsFactors = FALSE)

kernel <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/unbalanced/kernel_data.csv",stringsAsFactors = FALSE)

mozillal <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/unbalanced/mozilla_data.csv", stringsAsFactors = FALSE)

xen <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/unbalanced/xen_data.csv",stringsAsFactors = FALSE)

#---------------------KNN-----------------------------------------------------------------------------
# função normalizar
normalizar <- function(x) { num <- x - min(x)
denom <- max(x) - min(x)
return (num/denom)
}
# aplicando a função normalizar para as variáveis numéricas
glibc_balanced_norm <- as.data.frame(lapply(glibc_balanced[-28], normalizar))
tail(glibc_balanced_norm)
summary(glibc_balanced_norm)

# Construindo um indicador para amostra aleatória:
set.seed(1234)

ind <- sample(2, nrow(glibc_balanced), replace=TRUE, prob=c(0.67, 0.33))

# Construindo o conjunto de treinamento:
glibc_balanced.treinamento <- glibc_balanced[ind==1, -28]

# Construindo o conjunto de test:
glibc_balanced.test <- glibc_balanced[ind==2, -28]

# Inspecionando os conjuntos:
head(glibc_balanced.treinamento)
head(glibc_balanced.test)

# Criando o vetor com os nomes(labels) para o conjunto treinamento
glibc_balanced.trainLabels <- glibc_balanced[ind == 1,28]

# Criando o vetor com os nomes(labels) para o conjunto teste
glibc_balanced.testLabels <- glibc_balanced[ind == 2, 28]

# Inspecionando o resultado
print(glibc_balanced.trainLabels)
print(glibc_balanced.testLabels)


# Construindo o modelo
glibc_balanced_pred <- knn(train = glibc_balanced.treinamento, test = glibc_balanced.test,
                           cl = glibc_balanced.trainLabels, k=3)

# Inspecionando `iris_pred`
glibc_balanced_pred

# Construindo a tabela:
CrossTable(x = glibc_balanced.testLabels, y = glibc_balanced_pred, prop.chisq=FALSE)



#--------Naive Bayes--------------------------------

library(e1071)

modelo_NB <- naiveBayes(Affected ~., data = mozilla)
modelo_NB

exemplar_teste <- data.frame(Class="1st",  NEUTRAL="n",  VULNERABLE="v")

predict(modelo_NB, exemplar_teste, type = "class")
predict(modelo_NB, exemplar_teste, type = "raw")


#------------------------------tree_and_rules--------------------------------------


library(party)
# aplicando a função normalizar para as variáveis numéricas
glibc_balanced_norm <- as.data.frame(lapply(glibc_balanced[-28], normalizar))
tail(glibc_balanced_norm)
summary(glibc_balanced_norm)

# Construindo um indicador para amostra aleatória:
set.seed(1234)

ind <- sample(2, nrow(glibc_balanced), replace=TRUE, prob=c(0.7, 0.3))

# Construindo o conjunto de treinamento:
glibc_balanced.treinamento <- glibc_balanced[ind==1, -28]

# Construindo o conjunto de test:
glibc_balanced.test <- glibc_balanced[ind==2, -28]


feature_glibc <- c("AltCountLineCode","CountInput", "CountLineBlank","CountLineCodeDec", "CountLineComment","CountLinePreprocessor",
                   "CountPath","CountStmt","CountStmtEmpty","Cyclomatic","CyclomaticStrict","Knots", "MinEssentialKnots","RatioCommentToCode",
                   "AltCountLineComment","CountLine","CountLineCode", "CountLineCodeExe","CountLineInactive","CountOutput","CountSemicolon",
                   "CountStmtDecl","CountStmtExe","CyclomaticModified","Essential","MaxEssentialKnots","MaxNesting")

myFormula <- Affected ~ feature_glibc

tree <- ctree(tree, data = glibc_balanced.treinamento)


myFormula <- Affected ~ AltCountLineCode + CountInput + CountLineBlank + CountLineCodeDecl + CountLineComment + CountLinePreprocessor + CountPath + CountStmt
                          

