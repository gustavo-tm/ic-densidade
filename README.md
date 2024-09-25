# Para o bem ou para o mal: análise da capacidade que o governo tem de controlar a densidade habitacional

Este repositório contém o código necessário para reproduzir os resultados encontrados na pesquisa.

## Resumo

O Plano Diretor Estratégico (PDE 2014) do município de São Paulo tem como um dos principais
objetivos gerar adensamento populacional nas áreas mais desejáveis. Para tanto, foram criados
instrumentos regulatórios e incentivos de determinados padrões construtivos, com a expectativa
de que estes sejam capazes de gerar maior densidade populacional nessas determinadas regiões.
O escopo desta pesquisa é identificar se estes padrões construtivos incentivados e regulamentados
são de fato capazes de definir a densidade da região. As estimativas apontam que cerca de
metade da cidade não responde aos incentivos ou às leis, já que não está registrada no Cadastro
Imobiliário Fiscal e portanto se encontra em situação informal. Para a outra metade, identificou-
se que entre os padrões construtivos, a densidade habitacional se demonstrou a mais efetiva para
definir a densidade populacional, seguida pela densidade construtivas. Há fracas evidências que
apontam para a relevância do número de pavimentos.

## Dados

Foram utilizados apenas dados públicos, disponíveis em https://geosampa.prefeitura.sp.gov.br/ e https://www.ibge.gov.br/. Estes dados não estão disponíveis neste repositório, já que somados apresentam mais de 5GB de armazenamento. Entre os principais dados utilizados, estão os do IPTU, dos lotes da cidade e a malha preliminar do censo de 2022.

## Utilização

O código completo está no arquivo `rmd.Rmd`, mas para não precisar rodar o código, pode ser consultado o arquivo `rmd.html`. As imagens geradas se encontram na pasta `tex/imagens/` e as tabelas em `tex/tabelas/`

## Relatório

O relatório em sua versão final se encontra em `tex/main.pdf`