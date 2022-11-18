# Pacote Simple-Forecasts
## Descrição

O pacote realiza projeções simples para séries de tempo.
Incluí métodos de modelagem univariada, copiar outra projeção, funções auxiliares e integração com FS e FaaS (futuramente).

## Instalação

Para instalar o pacote, pode ser utilizado a função devtools::install_github(). Como este é um repositório privado, deve ser usado um token do GitHub. Para gerá-lo, acesse [aqui](https://github.com/settings/tokens), e seleciona a permissão repo (Full control of private repositories) em token classic. 

```sh
# install.packages("devtools")
devtools::install_github("gabBelle/simple-forecasts", ref = "main", auth_token = "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")
```
## Funções

##### Utils: 
- **load_clean_series**: Carrega múltiplas séries do 4macro, limpando-as; 
- **split_series**: Seleciona apenas uma série retornada pelo _load_clean_series_; 
- **get_seas_adj**: Calcula a série ajustada sazonalmente, com parâmetros automáticos (STL ou X13 ou ambos);
- **cambio_real**: Calcula o câmbio real a partir do câmbio nominal e de índices de inflação de ambos países. 

##### Simples: 
- **sf_naive**: Projeção Naive (repete último valor);
- **sf_snaive**: Projeção Seasonal Naive (repete último valor sazonal); 
- **sf_drift**: Projeção de tendência (Utiliza tendência passada, valor final ou MoM);
- **sf_daily**: Projeção de tendência para séries diárias (valor final é a projeção da série mensal);
- **sf_aop**: Projeção usando YoY médio anual (compatibiliza YoY para os meses restantes);
- **sf_topdown**: Utiliza o mesmo YoY do mês projetado em outra série;
- **sf_seas_ratio**: Projeta uma série utilizando sua projeção dessaz, por meio do fator sazonal. 
- **sf_target**: Encapsula o  _drift_ e o _seas_ratio_  para projetar uma série original até seu valor final (ex.: provindo de uma projeção anual);
- **sf_conversao_cambio**: Utiliza dois pares de câmbio para projetar um terceiro (há projeção de BRL/USD e USD/EUR, gera-se BRL/EUR)

##### Univariado
- **sf_hw**: Suavização Exponencial Holt-Winters; 
- **sf_arima**: Auto-Arima.

##### Auxiliares
- **calc_yoy**: Calcula o YoY a partir de uma projeção;
- **check_vector_len**: Checa se o tamanho da projeção é adequado ao horizonte desejado;
- **expand_series**: aumenta o vetor de Date de um dataframe;
- **get_perodicity**: calcula a periodicidade de um dataframe.

## _Dependencies_

```sh
library(tidyverse)
library(series.4macro)
library(lubridate)
library(fable)
library(feasts)
library(tsibble)
library(seasonal)
library(zoo)
library(tsibbledata)
```

### _Example_ 

- **load_clean_series**

```sh
sids = c('BRGDP0002000ROQL', 'BRPUB0001000ROML') 
load_clean <- load_clean_series(sids, auth_path)
```
**sids** é o argumento que representa as séries temporais selecionadas da FS e, por outro lado, **auth_path** é o caminho para o arquivo de autenticação da series.4macro. 

- **split_series**
```sh
df <- split_series(df = load_clean, name_sid = "BRGDP0002000ROQL",
                   type = "realizado")
```
O argumento **type** retorna a série realizado, projetado ou ambos

- **get_perodicity**

```sh
get_periodicity(df = df)
```
O **df** Argumento é o *data.frame* selecionado.  

- **expand_series**
```sh
serie <- expand_series(df, end_projection = "2026-12-01")
```
O argumento **end_projection** representa a data limite selecionada para projeção, isto é, para preenchimento com NAs. 

- **snaive**

```sh
model_snaive = snaive(df, nmeans = 5, end_projection = "2026-12-01")
```
O argumento **nmeans** representa o número de anos selecionados para aplicação da média

- **holtWinter**

```sh
model_holtW = holtWinter(df, type = NULL, end_projection = "2026-12-01")
```
O argumento **type** representa a escolha do método do modelo Holt-Winters: **multiplicative**, **additive**, **trend** e **NULL**. O último representa o default, isto é, a escolha automática via critério de informação.

- **arimaUnivariate**

```sh
model_arima = arimaUnivariate(df, end_projection = "2026-12-01")
```
O argumento **df** representa o data.frame selecioando e, por sua vez, **end_projection** representa a data limite selecionada para projeção. 

- **get_x13**
```sh
arima_dessaz = get_x13(df)
```
O argumento **df** representa o data.frame para o dessaz.  

- **get_stl**

```sh
stl_dessaz = get_stl(df)
```
 O argumento *df* representa o data.frame para o dessaz.
 
 - **get_seas_adj**
```sh
dessaz_adj = get_seas_adj(df, type = "median")
```
O argumento **type**  representa o dessaz STL, X13 ou agregações como *mean* ou *median* (**O default é 'median'**)

