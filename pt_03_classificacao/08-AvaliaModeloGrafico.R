# Analisando o resultado atraves de gráficos (bônus extra)

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
# setwd("C:/FCD/BigDataRAzure/Cap15/Projeto")
# getwd()

Azure <- FALSE

# Alterando atribuição da variável compFrame
if(Azure){
  source("src/ClassTools.R")
  compFrame <- maml.mapInputPort(1)
} else {
  compFrame <- result_previsto_v2 #outFrame
}

# Usando o dplyr para filter linhas com classificação incorreta
require(dplyr)
creditTest <- cbind(dados_teste, scored = compFrame[ ,2] )
creditTest <- creditTest %>% filter(CreditStatus != scored)

# Plot dos residuos para os niveis de cada fator
require(ggplot2)
colNames <- c("CheckingAcctStat", "Duration_f", "Purpose",
              "CreditHistory", "SavingsBonds", "Employment",
              "CreditAmount_f", "Employment")

lapply(colNames, function(x){
  if(is.factor(creditTest[,x])) {
    ggplot(creditTest, aes_string(x)) +
      geom_bar() + 
      facet_grid(. ~ CreditStatus) + 
      ggtitle(paste("Numero de creditos ruim/bom por",x))}})


# Plot dos residuos condicionados nas variváveis CreditStatus vs CheckingAcctStat
lapply(colNames, function(x){
  if(is.factor(creditTest[,x]) & x != "CheckingAcctStat") {
    ggplot(creditTest, aes(CheckingAcctStat)) +
      geom_bar() + 
      facet_grid(paste(x, " ~ CreditStatus"))+ 
      ggtitle(paste("Numero de creditos bom/ruim por CheckingAcctStat e ",x))
  }})


