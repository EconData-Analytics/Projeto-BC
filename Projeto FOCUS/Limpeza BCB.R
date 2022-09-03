# Pacotes

library(tidtyverse)
library(janitor)

# Padronizando os nomes das variaveis

atividade <- atividade %>%
  clean_names()

inflacao <- inflacao %>%
  clean_names()

setor_externo <- setor_externo %>% 
  clean_names()

setor_publico <- setor_publico %>%
  clean_names()
