# Planejamento - Antes de começar os códigos

## Interface
- Mostrar todos os campos do batalha-naval como uma matriz quadrada de valores
- Tipos de campos: desconhecidos(com navio ou não), com navio acertado(mostrar um X nesse campo), conhecido sem navio no campo (mostrar um -)
- Colocar navios aleatoriamente pra diferentes direções horizontal e vertical

## Banco de dados e schema
- Banco de dados será MongoDB
- Armazenar recorde de n-tentativas e tempo para um campo de 10 por 10
- Schema utilizado 
    collection player:
    {
      nickname: "Player Name",
      n-tentativas: 19,
      tempo: "10 min 36 seg"
    }

# Backlog
- Função main que chama as outras e lida com fluxo do programa ✔️
- Função para criar matriz (mapa do batalha-naval) ✔️
- Função para mostrar matriz (mapa do batalha-naval) ✔️
- Função de atirar nas coordenadas passadas pelo jogador ✔️
- Função para colocar os navios aleatoriamente de acordo com o tamanho do navio e quantidade de navios passada para a função ✔️

# Pontos a melhorar
- O jogador poder ver em que posição está do jogo (função para recuperar lista de jogadores na ordem de n-tentativas e desempate tempo)
- Lidar melhor com erros de input para coordenadas escolhidas pelo jogador
- Testes unitários para cada função do jogo e para as funções de banco de dados
- Injeção de dependências (principalmente para funções do BD)
