# Teorema do Limite Central por simulação de Monte Carlo

## Resumo
Neste projeto é feito a ilustração do Teorema Central do Limite utilizando-se de simulações de Monte Carlo considerando diferentes tamanhos de amostras e diferentes distribuições de probabilidade.

**Palavras-chave**: Geração de números pseudo-aleatorios; convergência em distribuição; Simulação de Monte Carlo; Teorema Central do Limite.

## Introdução

Um resultado visto em teoria das probabilidades, é que uma sequência de variáveis aleatórias converge em distribuição, se e somente se, a sequência de suas funções características convergir pontualmente para a função característica do limite. Este resultado é extremamente importante para a prova do teorema central do limite (MAGALHÃES, 2006).

O TCL (teorema central do limite) estabelece que a distribuição da soma ou da média, de variáveis aleatórias independentes e identicamente distribuídas (iid) se aproximará de uma distribuição normal a medida que o tamanho de amostra aumenta.

Por conta desse resultado a distribuição normal é tão famosa, sendo bastante utilizada em diversos testes estatísticos. Além disso, o TCL é extremamente importante para a inferência estatística, tornando possível a estimação da média e o desvio padrão populacional a partir de uma amostra extraída dessa população.

O trabalho trata-se de um experimento prático a fim de ilustrar o TCL, durante o desenvolvimento foi utilizado o método de simulação de Monte Carlo, uma técnica probabilística que visa prever possíveis resultados de um evento incerto.

## Metodologia e Resultados

O código para a demonstração do teorema central do limite e desenvolvimento do projeto foi escrito em linguagem R, e com o auxílio da biblioteca shiny para a organização e exposição dos resultados do experimento. Através da biblioteca shiny foi criada uma interface para facilitar a escolha da distribuição e diferentes tamanhos de amostras pelo usuário.

Os dados da população foram obtidos através da geração de números pseudoaleatórios e de acordo com a distribuição de probabilidade especificada. As amostras extraídas da população e a quantidade de iterações a serem feitas através das simulações de Monte Carlo podem definidas pelo usuário livremente.
Das distribuições de probabilidades selecionadas, optou-se pelas distribuições, discretas e contínuas, mais conhecidas, sendo elas; Binominal, Poisson, Exponencial, Uniforme, Gamma e Normal.

Partindo de uma semente pré definida (2022) para fins de reprodução do experimento, em todas as simulações efetuadas foram realizadas a geração de 1000 números pseudoaleatórios de cada distribuição. Foram extraídas amostras de tamanho 5, 30 e 100 que tiveram suas médias calculadas e armazenadas. Esse procedimento foi realizado 1000 vezes. Nota-se que com o aumento do tamanho da amostra fica cada vez mais claro a aproximação para a Distribuição Normal.

Para as distribuições discretas foram utilizados os parâmetros n = 10 e p = 0.6 na distribuição binomial e λ = 5 para a distribuição poisson. Já para as distribuições contínuas, foi utilizada a uniforme [0,1], a distribuição normal padrão, a exponencial com parâmetro λ = 1 e para a distribuição Gamma α = 5 e β = 1.

## Considerações Finais

Com as simulações realizadas nesse estudo é possível visualizar de forma mais prática a teoria por trás do teorema central do limite. Através de uma interface responsiva, criada com o intuito de facilitar a testagem do teorema para diferentes funções de distribuição, considerando diferentes parâmetros e tamanhos de amostra, o programa retorna em tempo real a visualização das propriedades descritas no teorema para o usuário. Ainda como resultado, é possível observar a convergência da média amostral para a populacional.

