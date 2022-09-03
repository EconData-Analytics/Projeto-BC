######## Pacotes e Data ########

library(tidyverse)
library(rbcb)

start_date = "2009-12-01"

# Funcao que remove coluna end_date caso ela exista

remove_end_date <- function(df){
  if("end_date" %in% colnames(df)){
    df <- df %>%
      select(-end_date)
    return(df)
  }else{
    return(df)
  }
}
######## Inflacao ########

inflacao <- rbcb::get_series(c("IPCA" = 433,
                           "IPCA Alimentacao e Bebidas" = 1635,
                           "IPCA Habitacao" = 1636,
                           "IPCA Artigos de Resid?ncia" = 1637,
                           "IPCA Vestuario" = 1638,
                           "IPCA Transportes" = 1639,
                           "IPCA Comunicacao" = 1640,
                           "IPCA Saude e Cuidados Pessoais" = 1641,
                           "IPCA Despesas Pessoais" = 1642,
                           "IPCA Educacao" = 1643,
                           "IPCA Comercializaveis" = 4447,
                           "IPCA Nao Comercializaveis" = 4448,
                           "IPCA Monitorados" = 4449,
                           "IPCA Nucleo Medias Aparadas(Suavizado)" = 4466,
                           "IPCA Nucleo Medias Aparadas(Sem Suavizado)" = 11426,
                           "IPCA Nao Duraveis" = 10841,
                           "IPCA Semi Duraveis" = 10842,
                           "IPCA Duraveis" = 10843,
                           "IPCA Servicos" = 10844,
                           "IPCA Livres" = 11428,
                           "IPCA Industriais" = 27863,
                           "IPCA Alimentacao no DOmicilio" = 27864,
                           "Indice de Difusao" = 21379), start_date) %>%
  reshape::merge_recurse() %>%
  remove_end_date()
  
#e normal dar um erro, isso acontece caso a importacao tenha sido efetuada corretamente e a coluna end_date nao tenha sido criada.

#Colocando o s?mbolo _ (underline) como separador dos ?ndices IPCA, pois - (h?fen) estava dando problema com o ggplot

ICBR <- get_series(c("ICBR" = 27574,
                     "ICBR_Agro" = 27575,
                     "ICBR_Metal" = 27576,
                     "ICBR_Energia" = 27577), start_date) %>%
  reshape::merge_recurse() %>%
  remove_end_date

ICBR_temp <- ICBR[,-1] #Criando um ICBR tempor?rio sem a coluna date, para facilitar a constru??o do loop

n <- nrow(ICBR_temp) - 1 #Em raz?o do modo como ? calculado a varia??o(utilizando i + 1)

m <- ncol(ICBR_temp)

variacao = data.frame()

for(i in 1:n){
  for(j in 1:m){
    taxa_variacao_ICBR <- (ICBR_temp[i+1,j]/ICBR_temp[i,j])-1
    variacao[i + 1, j] <- round(100 *taxa_variacao_ICBR, digits = 2)
  }
}
#Calcula a varia??o para todas as linhas e colunas do ICBR_temp

names(variacao) <- c("Tx.Var_ICBR", "Tx.Var_ICBR_Agro", "Tx.Var_ICBR_Metal", "Tx.Var_ICBR_Energia")

ICBR <- cbind(ICBR, variacao)

inflacao <- dplyr::right_join(inflacao, ICBR, by = "date")




######## Setor Publico ########

setor_publico <- rbcb::get_series(c("Gov. Federal e BC" = 2053,
                                    "Gov. Federal" = 2054,
                                    "BC" = 2055,
                                    "Estaduais e Municipais" = 2056,
                                    "Estaduais" = 2057,
                                    "Municipais" = 2058,
                                    "Estatais" = 2059,
                                    "Estatais Federais" = 2060,
                                    "Estatais Estaduais" = 2061,
                                    "Estatais Municipais" = 2062), start_date) %>%
  reshape::merge_recurse() %>%
  remove_end_date()


setor_publico_temp <- setor_publico[,-1] 

n <- nrow(setor_publico_temp) - 1

m <- ncol(setor_publico_temp)

variacao = data.frame()

for(i in 1:n){
  for(j in 1:m){
    taxa_variacao_setor_publico <- (setor_publico_temp[i+1,j]/setor_publico_temp[i,j])-1
    variacao[i + 1, j] <- round(100 *taxa_variacao_setor_publico, digits = 2)
  }
}

names(variacao) <- c("Tx.Var_Gov. Federal e BC", "Tx.Var_Gov. Federal", "Tx.Var_BC", "Tx.Var_Estaduais e Municipais","Estaduais","Municipais","Estatais","Estatais Federais","Estatais Estaduais","Estatais Municipais")

setor_publico <- cbind(setor_publico, variacao)


######## Setor Externo ########

setor_externo <- get_series(c("Reservas Internacionais" = 3546,
                              "Transacoes Correntes_Saldo" = 22701,
                              "Transacoes Correntes_Receita" = 22702,
                              "Transacoes Correntes_Despesa" = 22703,
                              "Conta Capital_Saldo" = 22851,
                              "Conta Capital_Receita" = 22852,
                              "Conta Capital_Despesa" = 22853,
                              "Conta Financeira" = 22863,
                              "Investimento Direto" = 23080,
                              "Dolar" = 3696), start_date) %>%
  reshape::merge_recurse()%>%
  remove_end_date()


setor_externo_temp <- setor_externo[,-1] 

n <- nrow(setor_externo_temp) - 1

m <- ncol(setor_externo_temp)

variacao = data.frame()

for(i in 1:n){
  for(j in 1:m){
    taxa_variacao_setor_externo <- (setor_externo_temp[i+1,j]/setor_externo_temp[i,j])-1
    variacao[i + 1, j] <- round(100 *taxa_variacao_setor_externo, digits = 2)
  }
}

names(variacao) <- c("Tx.Var Reservas Internacionais", 
                     "Tx.Var Transacoes Correntes_Saldo", 
                     "Tx.Var Transacoes Correntes_Receita",
                     "Tx.Var Transacoes Correntes_Despesa",
                     "Tx.Var Conta Capital_Saldo",
                     "Tx.Var Conta Capital_Receita",
                     "Tx.Var Conta Capital_Despesa",
                     "Tx.Var Conta Financeira",
                     "Tx.Var Investimento Direto",
                     "Tx.Var Dolar")

setor_externo <- cbind(setor_externo, variacao)

######## Atividade ########

atividade <- rbcb::get_series(c("Producao Industrial" = 21859,
                                "Producao de Oleo Bruto" = 1389,
                                "Producao de LGN" = 1390,
                                "Producao Total de Derivados de Petroleo" = 1391,
                                "Producao de Gas Natural" = 1392,
                                "Consumo de Energia Eletrica - Comercial" = 1402,
                                "Consumo de Energia Eletrica - Residencial" = 1403,
                                "Consumo de Energia Eletrica - Industrial" = 1404,
                                "Consumo de Energia Eletrica - Outros" = 1405,
                                "Consumo de Energia Eletrica - Total" = 1406,
                                "Consumo de Gasolina" = 1393,
                                "Consumo de GLP" = 1394,
                                "Consumo de Oleo Combustivel" = 1395,
                                "Consumo de Oleo Diesel" = 1396,
                                "Consumo de Outros Derivados de Petroleo" = 1397,
                                "Consumo de Derivados de Petroleo - Total" = 1398,
                                "Consumo de Alcool Hidratado" = 1399,
                                "Consumo de Alcool Anidro" = 1400,
                                "Consumo de Alcool Carburante" = 1401,
                                "Volume de Vendas no Varejo" = 1455,
                                "Taxa de Desocupação" = 24369,
                                "IVGR" = 21340,
                                "IBCBR" = 24363), start_date) %>%
  reshape::merge_recurse() %>%
  remove_end_date()


atividade_temp <- atividade[,-1] 

n <- nrow(atividade_temp) - 1

m <- ncol(atividade_temp)

variacao = data.frame()

for(i in 1:n){
  for(j in 1:m){
    taxa_variacao_atividade <- (atividade_temp[i+1,j]/atividade_temp[i,j])-1
    variacao[i + 1, j] <- round(100 *taxa_variacao_atividade, digits = 2)
  }
}

names(variacao) <- c("Tx.Var Producao Industrial", 
                     "Tx.Var Producao de Oleo Bruto", 
                     "Tx.Var Producao de LGN",
                     "Tx.Var Producao Total de Derivados de Petroleo",
                     "Tx.Var Producao de Gas Natural",
                     "Tx.Var Consumo de Energia Eletrica - Comercial",
                     "Tx.Var Consumo de Energia Eletrica - Residencial",
                     "Tx.Var Consumo de Energia Eletrica - Industrial",
                     "Tx.Var Consumo de Energia Eletrica - Outros",
                     "Tx.Var Consumo de Energia Eletrica - Total",
                     "Tx.Var Consumo de Gasolina",
                     "Tx.Var Consumo de GLP",
                     "Tx.Var Consumo de Oleo Combustivel",
                     "Tx.Var Consumo de Oleo Diesel",
                     "Tx.Var Consumo de Outros Derivados de Petroleo",
                     "Tx.Var Consumo de Derivados de Petroleo - Total",
                     "Tx.Var Consumo de Alcool Hidratado",
                     "Tx.Var Consumo de Alcool Anidro",
                     "Tx.Var Consumo de Alcool Carburante",
                     "Tx.Var Volume de Vendas no Varejo",
                     "Tx.Var da Taxa de Desocupacao",
                     "Tx.Var IVG-R",
                     "Tx.Var IBC-BR")

atividade <- cbind(atividade, variacao)

