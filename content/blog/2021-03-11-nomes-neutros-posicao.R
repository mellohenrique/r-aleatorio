knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

# Carregando pacotes
library(tidyverse)
library(knitr)

# Carregando dados
nomes <- read_csv("https://data.brasil.io/dataset/genero-nomes/nomes.csv.gz") %>% 
  mutate(posicao = rank(-frequency_total),
         decil = ntile(frequency_total, 10),
         neutro = ratio < 0.8)

nomes %>% 
  group_by(decil) %>% 
  summarise(maximo = max(frequency_total),
            minimo = min(frequency_total),
            soma = sum(neutro),
            percentual = scales::percent(soma/n(), accuracy = 0.01, decimal.mark = ",", big.mark = "."),
            palavras = n(),)


nomes %>% 
  group_by(decil) %>% 
  summarise(nomes_neutros = sum(neutro)) %>% 
  ggplot(aes(x = decil, y = nomes_neutros)) +
  geom_col()

ggplot(nomes, aes(y = frequency_total, x = posicao)) +
  geom_point() +
  scale_y_continuous(trans='log10', labels = scales::number, breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000))+
  theme_bw()
