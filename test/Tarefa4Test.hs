{-
Module      : Tarefa4Test
Description : Testes para a Tarefa 4 
-}
module Main where

import tipos
import Tarefa4

-- | Lista de testes para a Tarefa 4
testesT4 :: [Estado]
testesT4 = [ estadoBase
           , estadoCombateProximo
           , estadoFarmingTerra
           , estadoPerigoExplosivos
           , estadoMuitosInimigos
           ]

-- --- DEFINIÇÃO DOS CENÁRIOS DE TESTE ---

-- 1. Mapa de exemplo simples (6x10) adaptado do enunciado
mapaSimples :: Mapa
mapaSimples = 
    [ replicate 10 Ar
    , replicate 10 Ar
    , replicate 10 Ar
    , replicate 10 Ar
    , replicate 5 Terra ++ replicate 5 Ar
    , replicate 5 Terra ++ [Pedra, Pedra] ++ replicate 3 Agua
    ]

-- Minhoca padrão com munições
minhocaPadrao :: Int -> Posicao -> Minhoca
minhocaPadrao vida pos = Minhoca 
    { posicaoMinhoca = Just pos
    , vidaMinhoca = Viva vida
    , jetpackMinhoca = 10
    , escavadoraMinhoca = 10
    , bazucaMinhoca = 50  -- Muita munição para o bot disparar à vontade
    , minaMinhoca = 10
    , dinamiteMinhoca = 10
    }

-- Teste 1: Estado Base (O bot vs 1 inimigo num mapa aberto)
estadoBase :: Estado
estadoBase = Estado 
    { mapaEstado = mapaSimples
    , objetosEstado = []
    , minhocasEstado = [ minhocaPadrao 100 (3,0) -- O teu Bot (índice 0)
                       , minhocaPadrao 100 (3,8) -- Inimigo
                       ]
    }

-- Teste 2: Combate Próximo (Ver se o bot dispara bazucas ou foge)
estadoCombateProximo :: Estado
estadoCombateProximo = Estado 
    { mapaEstado = mapaSimples
    , objetosEstado = []
    , minhocasEstado = [ minhocaPadrao 100 (3,4)
                       , minhocaPadrao 100 (3,5)
                       ]
    }

-- Teste 3: Farming de Terra (Muitos blocos de terra e nenhum inimigo próximo)
estadoFarmingTerra :: Estado
estadoFarmingTerra = Estado 
    { mapaEstado = replicate 4 (replicate 10 Ar) ++ [replicate 10 Terra, replicate 10 Pedra]
    , objetosEstado = []
    , minhocasEstado = [ minhocaPadrao 100 (3,2) ]
    }

-- Teste 4: Perigo (Barril de TNT e Dinamite perto do Bot para testar o Radar de Evasão)
estadoPerigoExplosivos :: Estado
estadoPerigoExplosivos = Estado 
    { mapaEstado = mapaSimples
    , objetosEstado = [ Barril (3,1) True
                      , Disparo (3,0) Norte Dinamite (Just 1) 1
                      ]
    , minhocasEstado = [ minhocaPadrao 100 (3,1) ]
    }

-- Teste 5: Caos (Muitas minhocas e disparos no ar)
estadoMuitosInimigos :: Estado
estadoMuitosInimigos = Estado 
    { mapaEstado = mapaSimples
    , objetosEstado = [ Barril (5,5) False ]
    , minhocasEstado = [ minhocaPadrao 100 (3,0) -- Bot
                       , minhocaPadrao 50 (3,3)  -- Inimigo A
                       , minhocaPadrao 50 (3,6)  -- Inimigo B
                       , minhocaPadrao 50 (3,9)  -- Inimigo C
                       ]
    }