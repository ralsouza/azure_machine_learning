# Lista de Exercícios 1 - Capítulo 13

# Configuração do diretório de trabalho
# OS X
setwd("/Users/ls_rafael/Documents/GitHub/azure_machine_learning")
getwd()

# Para este script, vamos usar o mlbench (Machine Learning Benchmark Problems)
# https://cran.r-project.org/web/packages/mlbench/mlbench.pdf
# Este pacote contém diversos datasets e usaremos um com os dados 
# de votação do congresso americano 

# Seu trabalho é prever os votos em republicanos e democratas (variável Class)

# Import
install.packages("mlbench")
library(mlbench)
?mlbench

# Carregando o dataset
?HouseVotes84
data("HouseVotes84")
View(HouseVotes84)
head(HouseVotes84)
summary(HouseVotes84)
str(HouseVotes84)

# Analise exploratória de dados
plot(as.factor(HouseVotes84[ ,2]))
title(main = "Votes cast for issue", xlab = "vote", ylab = "# reps")

plot(as.factor(HouseVotes84[HouseVotes84$Class == 'republican', 2]))
title(main = "Republican votes cast for issue 1", xlab = "vote", ylab = "# reps")

plot(as.factor(HouseVotes84[HouseVotes84$Class == 'democrat',2]))
title(main = "Democrat votes cast for issue 1", xlab = "vote", ylab = "# reps")

# Funções usadas para imputation
# Função que retorna o numeros de NA's por voto e classe (democrat or republican)
na_by_col_class <- function (col,cls){return(sum(is.na(HouseVotes84[ ,col]) & HouseVotes84$Class==cls))}

p_y_col_class <- function(col,cls){
  sum_y <- sum(HouseVotes84[,col] == 'y' & HouseVotes84$Class == cls, na.rm = TRUE)
  sum_n <- sum(HouseVotes84[,col] == 'n' & HouseVotes84$Class == cls, na.rm = TRUE)
  return(sum_y/(sum_y+sum_n))}

# Testando a função
p_y_col_class(2,'democrat')
p_y_col_class(2,'republican')
na_by_col_class(2,'democrat')
na_by_col_class(2,'republican')

# Impute missing values
for (i in 2:ncol(HouseVotes84)) {
  if(sum(is.na(HouseVotes84[,i])>0)) {
    c1 <- which(is.na(HouseVotes84[,i]) & HouseVotes84$Class == 'democrat',arr.ind = TRUE)
    c2 <- which(is.na(HouseVotes84[,i]) & HouseVotes84$Class == 'republican',arr.ind = TRUE)
    HouseVotes84[c1,i] <- ifelse(runif(na_by_col_class(i,'democrat'))<p_y_col_class(i,'democrat'),'y','n')
    HouseVotes84[c2,i] <- ifelse(runif(na_by_col_class(i,'republican'))<p_y_col_class(i,'republican'),'y','n')}
}

# Gerando dados de treino e dados de teste
HouseVotes84[,"train"] <- ifelse(runif(nrow(HouseVotes84)) < 0.80,1,0)
trainColNum <- grep("train",names(HouseVotes84))

# Gerando os dados de treino e de teste a partir da coluna de treino
trainHouseVotes84 <- HouseVotes84[HouseVotes84$train == 1, -trainColNum]
testHouseVotes84 <- HouseVotes84[HouseVotes84$train == 0, -trainColNum]

# Invocando o método NaiveBayes
install.packages("e1071")
library(e1071)

# Exercício 1 - Crie o modelo Naive Bayes e faça as previsões
# Treine o modelo
?naiveBayes

model_nb <- naiveBayes(Class ~., data = trainHouseVotes84)
# democrat   republican 
# 0.6197183  0.3802817 

# Visualização do Resultado
model_nb
str(model_nb)
summary(model_nb)

# Predição
pred_hv84 <- predict(model_nb, testHouseVotes84[ ,-1])

summary(pred_hv84)
# democrat republican 
#       44         36

# Confusion Matrix (Tabela de Comparação)
table(Preditos = pred_hv84, Observados = testHouseVotes84$Class)

##################################
#              Observados
# Preditos     democrat republican
#   democrat         42          2
#   republican        5         31
##################################

# Avaliação do Desempenho do Naive Bayes
mean(pred_hv84 == testHouseVotes84$Class) # 91,25% de precisão

# Função para executar e registrar todos os resultados do modelo
hb_multiple_runs <- function(train_fraction, n){
  
  fraction_correct <- rep(NA, n)
  
  for(i in 1:n){
    
    HouseVotes84[ ,'train'] <- ifelse(runif(nrow(HouseVotes84)) < train_fraction, 1, 0)
    
    trainColNum <- grep('train', names(HouseVotes84))
    
    trainHouseVotes84 <- HouseVotes84[HouseVotes84$train == 1, -trainColNum]
    testHouseVotes84  <- HouseVotes84[HouseVotes84$train == 0, -trainColNum]
    
    model_nb <- naiveBayes(Class ~., data = trainHouseVotes84)
    
    pred_hv84 <- predict(model_nb, testHouseVotes84[ ,-1])
    
    fraction_correct[i] <- mean(pred_hv84 == testHouseVotes84$Class)
  }
  return(fraction_correct)
}

# Executar o Modelo 20 vezes e 80% para massa de treino
fraction_correct_predictions <- hb_multiple_runs(0.8, 20)
fraction_correct_predictions

# Resumo dos Resultados 
summary(fraction_correct_predictions)

# Desvio Padrão
sd(fraction_correct_predictions)

# Conclusão
# Os resultados estão das execuções estão bem próximos, entre 88% e 95%
# e desvio padrão de 2%
# Logo o Naive Bayes fez um bom trabalho de predição.














