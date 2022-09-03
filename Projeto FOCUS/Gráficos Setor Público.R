# Pacotes

library(tidyverse)
library(plotly)

# Criando graficos

# Dívida Líquida do Governo Federal
setor_publico %>%
  ggplot()+
  geom_line(aes(x = date, y = gov_federal), color = 'blue')+
  geom_rect(aes(xmin = as.Date('01/04/2014', format = '%d/%m/%Y'),
                xmax = as.Date('01/10/2016', format = '%d/%m/%Y'),
                ymin = -Inf,
                ymax = Inf), alpha = 0.005)+
  geom_rect(aes(xmin = as.Date('01/01/2020', format = '%d/%m/%Y'),
                xmax = as.Date('01/10/2021', format = '%d/%m/%Y'),
                ymin = -Inf,
                ymax = Inf), alpha = 0.005)+
  theme_classic()+
  labs(x = 'Data',
       y = 'Reais',
       caption = 'Fonte: Banco Central e CODACE',
       title = 'Dívida Líquida do Governo Federal e Recessões Brasileiras')

# Dívida Líquida do BC

setor_publico %>%
  ggplot()+
  geom_line(aes(x = date, y = bc), color = 'blue')+
  geom_rect(aes(xmin = as.Date('01/04/2014', format = '%d/%m/%Y'),
                xmax = as.Date('01/10/2016', format = '%d/%m/%Y'),
                ymin = -Inf,
                ymax = Inf), alpha = 0.005)+
  geom_rect(aes(xmin = as.Date('01/01/2020', format = '%d/%m/%Y'),
                xmax = as.Date('01/10/2021', format = '%d/%m/%Y'),
                ymin = -Inf,
                ymax = Inf), alpha = 0.005)+
  theme_classic()+
  labs(x = 'Data',
       y = 'Reais',
       caption = 'Fonte: Banco Central e CODACE',
       title = 'Dívida Líquida do Banco Central e Recessões Brasileiras')

# Dívida Líquida dos Estados e Municípios e Recessões Brasileiras
setor_publico %>%
  ggplot()+
  geom_line(aes(x = date, y = estaduais_e_municipais), color = 'blue')+
  geom_rect(aes(xmin = as.Date('01/04/2014', format = '%d/%m/%Y'),
                xmax = as.Date('01/10/2016', format = '%d/%m/%Y'),
                ymin = -Inf,
                ymax = Inf), alpha = 0.005)+
  geom_rect(aes(xmin = as.Date('01/01/2020', format = '%d/%m/%Y'),
                xmax = as.Date('01/10/2021', format = '%d/%m/%Y'),
                ymin = -Inf,
                ymax = Inf), alpha = 0.005)+
  theme_classic()+
  labs(x = 'Data',
       y = 'Reais',
       caption = 'Fonte: Banco Central e CODACE',
       title = 'Dívida Líquida de Estados e Municípios e Recessões Brasileiras')
  
setor_publico %>%
  ggplot()+
  geom_line(aes(x = date, y = estatais), color = 'blue')+
  geom_rect(aes(xmin = as.Date('01/04/2014', format = '%d/%m/%Y'),
                xmax = as.Date('01/10/2016', format = '%d/%m/%Y'),
                ymin = -Inf,
                ymax = Inf), alpha = 0.005)+
  geom_rect(aes(xmin = as.Date('01/01/2020', format = '%d/%m/%Y'),
                xmax = as.Date('01/10/2021', format = '%d/%m/%Y'),
                ymin = -Inf,
                ymax = Inf), alpha = 0.005)+
  theme_classic()+
  labs(x = 'Data',
       y = 'Reais',
       caption = 'Fonte: Banco Central e CODACE',
       title = 'Dívida Líquida de Estatais e Recessões Brasileiras')
