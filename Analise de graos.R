## Bibliotecas de séries temporais
library(TSA)
library(forecast)
library(fpp3)
library(readxl)
library(scales)
library(dplyr)
library(tsibble)
library(stringr)
library(lubridate)

### Carregando banco de dados em formato csv
tute1 = read_excel("ESTATÍSTICA/8º Período/ANÁLISE DE SÉRIES TEMPORAIS/Estoques de grãos/dados.xlsx")
View(tute1)

tute1 = transform(tute1, Totais = Totais/1000000 )



### transformando para o formato tstible
serie <- tute1 %>%
  mutate(Semestres = yearmonth(Semestres)) %>%
  as_tsibble(index = Semestres)
serie


### Plot das series
serie %>%
  pivot_longer(-Semestres) %>%
  ggplot(aes(x = Semestres, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")


### ESTUDO PARA A VARIAVEL SALES 

### ACF e PACF das series 
acf_totais = serie %>% 
  ACF(Totais, lag_max = 40) %>% 
  autoplot() + labs(title="ACF da variável Totais")

pacf_totais = serie %>% 
  PACF(Totais, lag_max = 40) %>% 
  autoplot() + labs(title="PACF da variável Totais")

gridExtra::grid.arrange(acf_totais, pacf_totais, ncol=2)


### Estudando a sazonalidade


serie %>%
  gg_season(Totais, labels = "both") +
  labs(y = "Toneladas",
       title = "Sazonalidade: Totais")


### subgraficos 

serie %>%
  gg_subseries(Totais) +
  labs(
    y = "Quantidade de Toneladas",
    title = "Sazonalidade: Totais"
  )








### Extraindo os componentes de uma serie temporal 

### Decomposição aditiva
serie %>%
  model(
    classical_decomposition(Totais, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição classica aditiva do total de vendas")

### Decomposição multiplicativa
serie %>%
  model(
    classical_decomposition(Totais, type = "multiplicative")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição classica aditiva do total de vendas")

#### Forecast ---- 

## para identificar os anos iniciais e finais
head(serie)
tail(serie)

# Definindo a base de dados em treinamento e teste
train <- serie %>%
  filter_index("2007 jul" ~ "2019 jul")
tail(train)

test <- serie %>%
  filter_index("2020 jan" ~ .);test

### verificando se esta correto
head(test)
tail(test)


# Estimando o modelo de suavizacao
sales_fit <- train %>%
  model(
    # SES (Suavização Exponencial Simples)
    SES = ETS(Totais ~ error("A") + trend("N") + season("N")),
    
    # Holt aditivo
    HoltA = ETS(Totais ~ error("A") + trend("A") + season("N")),
    
    # Holt multiplicativo
    HoltM = ETS(Totais ~ error("M") + trend("A") + season("N")),
    
    # Holt-Winters aditivo
    HW_Add = ETS(Totais ~ error("A") + trend("A") + season("A")),
    
    # Holt-Winters multiplicativo
    HW_Mult = ETS(Totais ~ error("M") + trend("A") + season("M"))
  )
sales_fit



# GERANDO UMA PREVISAO h = 4 passos 
sales_fc <- sales_fit %>% forecast(h = 4)
sales_fc

# Grafico da serie com as previsoes 
sales_fc %>%
  autoplot(train, level = NULL) +
  autolayer(
    test,
    colour = "red"
  ) +
  labs(
    y = "Toneladas",
    title = "Previsão de toneladas de 2020 à 2021"
  ) +
  guides(colour = guide_legend(title = "Forecast"))


### Medidas de avaliacao 
accuracy(sales_fc, test)






#### DEPOIS QUE A ESCOLHA DO MELHOR MODELO FOR FEITA FAÇA A PREVISAO PARA VALORES FUTUROS 
## NEsse exemplo dado que o melhor modelo foi o ETS5, vamos usar ele para fazer as previsões

##

# Estimando o modelo de suavizacao
sales_fit <- serie %>%
  model(
    ETS5 = ETS( Totais ~ error("M") + trend("A") + season("M")) 
  )
sales_fit



# GERANDO UMA PREVISAO h = 4 passos 
sales_fc <- sales_fit %>% forecast(h = 4)
sales_fc

# Grafico da serie com as previsoes 
sales_fc %>%
  autoplot(serie, level = NULL) +
  autolayer(
    test,
    colour = "black"
  ) +
  labs(
    y = "Toneladas",
    title = "Previsão de toneladas para os 4 próximos semestres"
  ) +
  guides(colour = guide_legend(title = "Forecast"))


### analise de residuos 
sales_fit %>% gg_tsresiduals()
