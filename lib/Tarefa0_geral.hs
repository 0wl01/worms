module Tarefa0_geral where

--Tipos de dados
type Matriz a = [[a]]
-- | var: (a,b)
type Posicao = (Int,Int)
-- | var: (x,y) x cresce para baixo e y para a direita
type Dimensao = (Int,Int)
-- | var: (lin,col)
data Direcao = Norte | Nordeste | Este | Sudeste | Sul | Sudoeste | Oeste | Noroeste
    deriving (Eq,Ord,Show,Read,Enum)

--Funções Auxiliares

-- | Verifica se um índice é válido para uma lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido x l = x < length l && x >= 0 --evitar indices negativos enquanto o canto superior esquerdo for pos (0,0)

-- | Devolve a dimensão de uma matriz.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz [] = (0,0)
dimensaoMatriz l = (length l, length (head l))

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool 
ePosicaoMatrizValida (x, y) m =
  let (nLinhas, nColunas) = dimensaoMatriz m
  in x >= 0 && y >= 0 && x < nLinhas && y < nColunas

-- | Move uma posição uma unidade no sentido de uma direção.
movePosicao :: Direcao -> Posicao -> Posicao
movePosicao Norte    (x,y) =(x-1, y)
movePosicao Nordeste (x,y) = (x-1, y+1)
movePosicao Este     (x,y) = (x, y+1)
movePosicao Sudeste  (x,y) = (x+1, y+1)
movePosicao Sul      (x,y) = (x+1, y)
movePosicao Sudoeste (x,y) = (x+1, y-1)
movePosicao Oeste    (x,y) = (x, y-1)
movePosicao Noroeste (x,y) = (x-1, y-1)

--este sistema presume que a origem (0,0) está no canto superior esquerdo
--se começarmos a usar origem no centro temos de mudar
--troquei para pattern matching sem guardas, para melhorar a legibilidade e evitar redudância

-- | Versão da função 'movePosicao' que garante que o movimento não se desloca para fora de uma janela.
movePosicaoJanela :: Dimensao -> Direcao -> Posicao -> Posicao
movePosicaoJanela (lin,col) dir (x,y) = 
  let (x', y') = movePosicao dir (x,y)
  in if ePosicaoMatrizValida (x',y') (replicate lin (replicate col ())) --o replicate cria a tal matriz dummy
       then (x',y')
       else (x,y)

--troquei a moveposicaoJanela tambem, retirei as guardas 
--esta funciona assim: faz o movimento normal, depois verifica se a nova posicao é valida
--se for valida devolve a nova posicao senao devolve a posicao original 
--criamos também uma matriz dummy com a dimensao da janela para usar na verificacao

-- | Converte uma posição no referencial em que a origem é no canto superior esquerdo da janela numa posição em que a origem passa a estar no centro da janela.
origemAoCentro :: Dimensao -> Posicao -> Posicao
origemAoCentro (lin,col) (x,y) = (x - (div col 2), y - (div lin 2)) --exemplo dimensão 30 por 30, a posição (0,0) tem que ser transformada em -15,-15

-- | Roda um par (posição,direção) 45% para a direita.
rodaPosicaoDirecao :: (Posicao,Direcao) -> (Posicao,Direcao)
rodaPosicaoDirecao (pos, dir) = (pos, toEnum ((fromEnum dir + 1) `mod` 8)) --fromEnum norte dá 0, adicionamos 1 e chamamos to Enum 1 que é igual a Nordeste

--Retirei aqueles guardas todas e aproveitei o facto da Direcao ser um Enum
--assim podemos usar fromEnum para obter o índice numérico da direção
--e toEnum para converter de volta para Direcao
--assim devolve a posição inalterada e a direção rodada, o mod 8 está lá para garantir que ao rodar Noroeste (7) volta a Norte (0)

-- | Devolve o elemento num dado índice de uma lista.
encontraIndiceLista :: Int -> [a] -> Maybe a
encontraIndiceLista x l | eIndiceListaValido x l = Just (l !! x) --indice válido retorna o que queremos
                        | otherwise = Nothing --caso contrário Nada :D

-- | Modifica um elemento num dado índice.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista indice elemento l | eIndiceListaValido indice l = take indice l ++ [elemento] ++ drop (indice + 1) l
--take pega nos indices antes do que queremos, concatenei-a com o elemento e volta a concatenar com o resto da lista que vamos buscar com drop
--take 2 [10,20,30,40] = [10,20]
--drop 3 [10,20,30,40] = [40]
                          | otherwise = l

-- | Devolve o elemento numa dada posição de uma matriz.
encontraPosicaoMatriz :: Posicao -> Matriz a -> Maybe a
encontraPosicaoMatriz (x,y) l | ePosicaoMatrizValida (x,y) l = Just ((l!! x) !! y) -- primeiro encontra a linha (lista) (x) que queremos e depois o indice dessa lista (y)
                              | otherwise = Nothing

-- | Modifica um elemento numa dada posição de uma matriz.
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (x, y) elemento m
  | ePosicaoMatrizValida (x, y) m = atualizaIndiceLista x novaLinha m --substitui a linha x pela novaLinha
  | otherwise = m --posição invalida
  where
    linha = m !! x --encontra a linha x onde queremos colocar elemento
    novaLinha = atualizaIndiceLista y elemento linha --coloca elemento no indice y

-- | Aplica uma sequência de movimentações a uma posição, pela ordem em que ocorrem na lista.
moveDirecoesPosicao :: [Direcao] -> Posicao -> Posicao
moveDirecoesPosicao listadirecao pos = foldl (\p d -> movePosicao d p) pos listadirecao --movePosicao tem tipo Direcao -> Posicao -> Posicao, dai "\p d -> movePosicao d p"
--usamos o acumulador da foldl percorremos a lista de Direções acumulando a posição resultante

-- | Aplica a mesma movimentação a uma lista de posições.
moveDirecaoPosicoes :: Direcao -> [Posicao] -> [Posicao]
moveDirecaoPosicoes dir listapos = map (movePosicao dir) listapos --map aplica uma função a cada elemento

-- | Verifica se uma matriz é válida, no sentido em que modela um rectângulo.
eMatrizValida :: Matriz a -> Bool
eMatrizValida l = all (==b) (map length l) --aplicamos length a todas as linhas da matriz ex output: [3,3,3] e o all verifica se todas são iguais
                    where (_,b) = dimensaoMatriz l --guarda o numero de colunas para comparar