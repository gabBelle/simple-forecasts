# Pacote Simple-Forecasts
## Descrição

O pacote realiza projeções simples para séries de tempo.
Incluí métodos de modelagem univariada, copiar outra projeção, funções auxiliares e integração com FS e FaaS (futuramente).

## Instalação

Para instalar o pacote, pode ser utilizado a função devtools::install_github(). Como este é um repositório privado, deve ser usado um token do GitHub. Para gerá-lo, acesse [aqui](https://github.com/settings/tokens), e seleciona a permissão repo (Full control of private repositories) em token classic. 

```sh
# install.packages("devtools")
devtools::install_github("gabBelle/simple-forecasts", ref = "main", auth_token = "token_pessoal")
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
- **sf_target**: Encapsula o  _drift_ e o _seas_ratio_  para projetar uma série original até seu valor final;
- **sf_aop**: Projeção usando YoY médio anual (compatibiliza YoY para os meses restantes);
- **sf_daily**: Projeção de tendência para séries diárias (valor final é a projeção da série mensal);
- **sf_topdown**: Utiliza o mesmo YoY do mês projetado em outra série;
- **sf_seas_ratio**: Projeta uma série utilizando sua projeção dessaz, por meio do fator sazonal. 
- **sf_conversao_cambio**: Utiliza dois pares de câmbio para projetar um terceiro (há projeção de BRL/USD e USD/EUR, gera-se BRL/EUR)

##### Univariado
- **sf_hw**: Suavização Exponencial Holt-Winters; 
- **sf_arima**: Auto-Arima.

##### Auxiliares
- **calc_yoy**: Calcula o YoY a partir de uma projeção;
- **check_vector_len**: Checa se o tamanho da projeção é adequado ao horizonte desejado;
- **expand_series**: aumenta o vetor de Date de um dataframe;
- **get_perodicity**: calcula a periodicidade de um dataframe.

## Exemplos

Para exemplos, consulte o arquivo Example.md. 
