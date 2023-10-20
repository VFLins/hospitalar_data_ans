# Análise e solução para negócios com dados hospitalares abertos

Este projeto usa dados hospitalares abertos do sistema de Troca de Informação em Saúde Suplementar (TISS). Aqui foi feita um procedimento completo com estes dados, passando pelas etapas de:

1. Coleta diretamente dos servidores FTP
2. Estruturamento e tratamento dos dados em ambiente local
3. Análise completa das variáveis presentes mais relevantes
4. Desenvolvimento de uma solução de Machine Learning com valor de negócio

Para acessar o relatório, [clique aqui](https://vflins.github.io/hospitalar_data-pda-tiss/). Para entender o que foi feito nas etapas acima, continue lendo.

## Etapa 1: Coleta dos dados

Uma rotina de python escrita no arquivo [`collector_saude-pda-tiss-hosp-2019.py`](https://github.com/VFLins/hospitalar_data-pda-tiss/blob/main/collector_saude-pda-tiss-hosp-2019.py) foi usada para baixar os dados do servidor, o conjunto estava armazenado como sequências de arquivos `.csv` com meses do ano dentro de sequências de pastas nomeadas com as unidades federativas do Brasil.

Esta rotina coleta o endereço de todos os arquivos `.csv` em uma lista e realiza a coleta. O processo se resume em carregar a tabela na memória e escrever num banco de dados sqlite local, cada nova tabela se transforma em uma sequência de linhas de dados armazenados abaixo dos já existentes.

Um empecilho para isto foi a presença de tabelas em formatos diferentes na base, que continham informações complementares, estas foram mescladas em uma só a partir de uma coluna de identificação em comum entre elas. Outro problema é que a maneira utilizada para coletar estes dados já não está mais disponível atualmente, então não será possível verificar o funcionamento desta rotina.

## Etapas 2 à 4

Todas estas etapas estão descritas em detalhes no próprio corpo do relatório, no link indicado anteriormente. Obrigado pela atenção!
