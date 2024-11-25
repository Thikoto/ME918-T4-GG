# ME918-T4-GG

## Shiny para Séries Temporais

Este Shiny foi desenvolvido utilizando o pacote **shiny** no software Rstudio, 
para realizar análises e modelagens de séries temporais, de maneira automática e manual. 

### Conjunto de Dados

O conjunto de dados deve conter a primeira coluna com as datas e a segunda coluna 
com os respectivos registros da série.


### Janelas

#### Leitura de Dados

Passos:
  - **1**: Fazer upload do arquivo (.csv ou .tsv);
  - **2**: Selecionar a data de início desejada, informando sua respectiva linha;
  - **3**: Selecionar a data final desejada, informando sua respectiva linha.

Após a realização desses passos, será mostrado a série temporal, com o gráfico 
de comportamento temporal, o gráfico de autocorrelação e o gráfico de autocorrelação 
parcial. 

#### Automático

Ao clicar em "Estimar ARIMA Automaticamente" o R usará a função $\textit{ARIMA}$ 
  do pacote $\textit{forecast}$. Essa ação retornará um gráfico da distribuição,
autocorrelação, histograma e QQ-Plot dos resíduos. Retornará, também, os valores dos critérios 
de informação do modelo escolhido automaticamente. Além disso, o valor do teste 
de $\textit{Box-Ljung}$ é gerado. Esse deve ser maior que 0.05 para não rejeitar
a hipótese nula de ausência de autocorrelação entre os resíduos da série.

#### Manual

Passos:
  - **p: Ordem autorregressiva (AR)**: Escolha do valor de acordo com a análise 
da série.
  - **d: Número de diferenciações (I)**: Escolha do valor de acordo com a análise 
da série.
  - **q: Ordem de média móvel (MA)**: Escolha do valor de acordo com a análise 
da série.
  - **P: Ordem sazonal autorregressiva**: Escolha do valor de acordo com a análise 
da série.
  - **D: Diferenciações sazonais**: Escolha do valor de acordo com a análise 
da série.
  - **Q: Ordem sazonal de média móvel**: Escolha do valor de acordo com a análise 
da série.

Ao clicar em "Ajustar Modelo Manualmente" o R usará as informações fornecidas nos
passos anteriores. Essa ação retornará um gráfico da distribuição,
autocorrelação, histograma e QQ-Plot dos resíduos. Retornará, também, os valores 
dos critérios 
de informação do modelo escolhido manualmente. Além disso, o valor do teste 
de $\textit{Box-Ljung}$ é gerado. Esse deve ser maior que 0.05 para não rejeitar
a hipótese nula de ausência de autocorrelação entre os resíduos da série.


#### Previsão

Para prever valores futuros dessa série, é necessário indicar a quantidade de 
observações futuras desejadas e clicar em "Previsão Futura". Essa ação retornará
um gráfico da previsão do modelo (automático e/ou manual) escolhido.
