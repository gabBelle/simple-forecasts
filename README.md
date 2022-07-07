# Package simple-forecasts
## _Description_

O simple-forecasts busca ser um pacote ágil para projeções simples de série temporal. Assim, o package fornece funções como, por exemplo, Seasonal naïve, Holt-Winters, Auto-arima univariado, X-13 ARIMA-SEATS, Seasonal Decomposition of Time Series by Loess (STL). 

## _Installation_

Você pode instalar a versão de desenvolvimento do GitHub
```sh
# install.packages("remotes")
remotes::install_github("")
```
## _Functions_

- **load_clean_series** carrega e limpa as séries da FS; 
- **split_series** retorna, entre as séries importadas da FS, apenas uma das séries; 
- **get_perodicity** retorna a periodicidade de um dataframe;
- **expand_series** aumenta o vetor de datas de um dataframe, inputando NAs na coluna de observação, para ser preenchida por alguma função de forecast; 
- **snaive** retorna o modelo Seasonal naïve; 
- **holtWinter** retorna o modelo Holt-Winters; 
- **arimaUnivariate** retorna o modelo Auto ARIMA; 
- **get_seas_adj** retorna a dessazonalização de uma série com as configurações automáticas do STL ou do X13, opcional retornar a média ou a mediana entre os dois métodos. 

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

- **get_perodicity**

```sh
get_periodicity(df = load_clean)
```
O **df** Argumento é o *data.frame* selecionado.  
- **split_series**
```sh
df <- split_series(df = load_clean, name_sid = "BRGDP0002000ROQL",
                   type = "realizado")
```
O argumento **type** retorna a série realizado, projetado ou ambos

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

