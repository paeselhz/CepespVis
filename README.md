# CepespVis - Portal de Visualização de Dados Eleitorais

### Introdução

Com o objetivo de participar do 1º DESAFIO DO CEPESP: DECIFRANDO AS ELEIÇÕES, a equipe PaeseLab, liderada por Luis Henrique Zanandréa Paese apresenta o projeto CepespVis, um portal de visualização de dados eleitorais, que traz de forma interativa e dinâmica os resultados das eleições desde 1998 até 2016. O projeto foi realizado utilizando software de código aberto, como R para a realização das estatísticas, e a plataforma Shiny, integrante do sistema RStudio, para o desenvolvimento das unidades de front-end e back-end.

Uma versão funcional do aplicativo está disponível em [CepespVis](https://paeselab.com/shiny/cepespvis/)

## Funcionamento da Plataforma

### Eleições Municipais

A aba de eleições municipais traz dados referentes as eleições dos anos de 2000, 2004, 2008, 2012 e 2016, cuja menor observação são os municípios do Brasil. Contendo dados das eleições para Vereadores e para Prefeitos, a aba traz através de um treemap, a visualização proporcional, como cada Coligação, Partido ou Candidato teve seus votos distribuidos em uma determinada eleição. Além disso, a subseção de Vereadores traz a opção de conhecimento do Quociente Eleitoral, medida usada pelo TSE para atribuir aos candidatos a eleição através de Quociente Partidário, ou através de Média.

### Eleições Gerais

A aba de eleições gerais traz dados referentes as eleições dos anos de 1998, 2002, 2006, 2010 e 2014. Além da visualização dos dados relacionados aos candidatos em nível estadual (e nacional no caso dos candidatos a presidente), essa aba contém mapas interativos que podem mostrar tanto a representação dos votos de um candidato, em relação ao total de uma determinada unidade geográfica, quanto qual a origem dos votos de um determinado candidato. Isso permite, além de uma visualização para os municípios, também uma visualizãção para outras agregações regionais definidas pelo IBGE (Instituto Brasileiro de Geografia e Estatística), como Microrregiões e Mesorregiões.

A visualização de dados também estende-se ao âmbito mais objetivo, de observar onde um determinado candidato conquistou mais votos, através de tabelas que podem ser ordenadas tanto pela representação dos votos de um candidato sobre os votos de uma unidade geográfica, quanto pela representação dos votos sobre o total do próprio candidato.

Todas as visualizações descritas acima estão presentes para Deputados Estaduais, Senadores, Governadores, Deputados Federais e Presidentes. 

### Mapa de Transição

Os mapas de transição que podem ser gerados através da ferramenta estão disponíveis apenas para as eleições para presidentes, e exclusivamente aos candidatos que concorrem ao segundo turno de cada eleição. Essa visualização permite visualizar, para cada um dos anos eleitorais, como se deu a transição dos votos do 1º para o 2º turno, mostrando de uma forma objetiva se um candidato manteve sua posição em uma determinada regionalização entre um turno e outro, ou se ganhou/perdeu votos.

### Compartilhe as informações vistas

Cada aba possui um botão de compartilhamento, que gera um link exclusivo para a visualização do usuário, permitindo assim que cada pessoa possa encontrar a informação que deseja, e repassá-la para quem interessar.

___

Obrigado,
Luis Henrique Zanandréa Paese