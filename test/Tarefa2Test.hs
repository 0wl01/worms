{- 
Module      : Tarefa2Test
Description : Testes completos para a Tarefa 2 - Validação de Jogadas
-}
module Main where

import Labs2025
import Tarefa2
import Magic

-- MAPAS PARA TESTES
mapa3x3 :: Mapa
mapa3x3 = [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]]

mapa1x1 :: Mapa
mapa1x1 = [[Ar]]

mapa5x5 :: Mapa
mapa5x5 = replicate 5 (replicate 5 Ar)

mapaComTerra :: Mapa
mapaComTerra = [[Ar, Terra, Ar], [Terra, Ar, Terra], [Ar, Terra, Ar]]

mapaComPedra :: Mapa
mapaComPedra = [[Ar, Pedra, Ar], [Pedra, Ar, Pedra], [Ar, Pedra, Ar]]

mapaComAgua :: Mapa
mapaComAgua = [[Ar, Ar, Ar], [Ar, Agua, Ar], [Ar, Ar, Ar]]

mapaComAguaEMorte :: Mapa
mapaComAguaEMorte = [[Ar, Agua, Ar], [Agua, Ar, Agua], [Ar, Agua, Ar]]

-- ESTADOS BASE PARA TESTES
estadoBase1 :: Estado
estadoBase1 = Estado
    { mapaEstado = mapa3x3
    , objetosEstado = []
    , minhocasEstado =
        [ Minhoca { posicaoMinhoca = Just (1,1), vidaMinhoca = Viva 100
                  , jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1
                  , minaMinhoca = 1, dinamiteMinhoca = 1 }
        ]
    }

estadoBase2 :: Estado
estadoBase2 = Estado
    { mapaEstado = mapa3x3
    , objetosEstado = []
    , minhocasEstado =
        [ Minhoca { posicaoMinhoca = Just (0,0), vidaMinhoca = Viva 100
                  , jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1
                  , minaMinhoca = 1, dinamiteMinhoca = 1 }
        , Minhoca { posicaoMinhoca = Just (2,2), vidaMinhoca = Viva 80
                  , jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1
                  , minaMinhoca = 1, dinamiteMinhoca = 1 }
        ]
    }

estadoComMorta :: Estado
estadoComMorta = Estado
    { mapaEstado = mapa3x3
    , objetosEstado = []
    , minhocasEstado =
        [ Minhoca { posicaoMinhoca = Just (1,1), vidaMinhoca = Viva 100
                  , jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1
                  , minaMinhoca = 1, dinamiteMinhoca = 1 }
        , Minhoca { posicaoMinhoca = Nothing, vidaMinhoca = Morta
                  , jetpackMinhoca = 0, escavadoraMinhoca = 0, bazucaMinhoca = 0
                  , minaMinhoca = 0, dinamiteMinhoca = 0 }
        ]
    }

estadoSemMunicoes :: Estado
estadoSemMunicoes = Estado
    { mapaEstado = mapa3x3
    , objetosEstado = []
    , minhocasEstado =
        [ Minhoca { posicaoMinhoca = Just (1,1), vidaMinhoca = Viva 100
                  , jetpackMinhoca = 0, escavadoraMinhoca = 0, bazucaMinhoca = 0
                  , minaMinhoca = 0, dinamiteMinhoca = 0 }
        ]
    }

estadoComObjeto :: Estado
estadoComObjeto = Estado
    { mapaEstado = mapa3x3
    , objetosEstado = [Barril (1,2) False]
    , minhocasEstado =
        [ Minhoca { posicaoMinhoca = Just (1,1), vidaMinhoca = Viva 100
                  , jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1
                  , minaMinhoca = 1, dinamiteMinhoca = 1 }
        ]
    }

estadoComMultiplosObjetos :: Estado
estadoComMultiplosObjetos = Estado
    { mapaEstado = mapa5x5
    , objetosEstado = [Barril (2,2) False, Disparo (2,3) Este Bazuca Nothing 0]
    , minhocasEstado =
        [ Minhoca { posicaoMinhoca = Just (2,1), vidaMinhoca = Viva 100
                  , jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1
                  , minaMinhoca = 1, dinamiteMinhoca = 1 }
        ]
    }

estadoVidaBaixa :: Estado
estadoVidaBaixa = Estado
    { mapaEstado = mapa3x3
    , objetosEstado = []
    , minhocasEstado =
        [ Minhoca { posicaoMinhoca = Just (1,1), vidaMinhoca = Viva 1
                  , jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1
                  , minaMinhoca = 1, dinamiteMinhoca = 1 }
        ]
    }

estado4Minhocas :: Estado
estado4Minhocas = Estado
    { mapaEstado = mapa5x5
    , objetosEstado = []
    , minhocasEstado =
        [ Minhoca { posicaoMinhoca = Just (1,1), vidaMinhoca = Viva 100
                  , jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1
                  , minaMinhoca = 1, dinamiteMinhoca = 1 }
        , Minhoca { posicaoMinhoca = Just (1,3), vidaMinhoca = Viva 100
                  , jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1
                  , minaMinhoca = 1, dinamiteMinhoca = 1 }
        , Minhoca { posicaoMinhoca = Just (3,1), vidaMinhoca = Viva 100
                  , jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1
                  , minaMinhoca = 1, dinamiteMinhoca = 1 }
        , Minhoca { posicaoMinhoca = Just (3,3), vidaMinhoca = Viva 100
                  , jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1
                  , minaMinhoca = 1, dinamiteMinhoca = 1 }
        ]
    }

-- LISTA DE TESTES NO FORMATO OFICIAL (NumMinhoca, Jogada, Estado)
testesTarefa2 :: [(NumMinhoca, Jogada, Estado)]
testesTarefa2 =
    [ -- Movimentos básicos
      (0, Move Este, estadoBase1)
    , (0, Move Norte, Estado { mapaEstado = mapa1x1
                             , objetosEstado = []
                             , minhocasEstado = [Minhoca { posicaoMinhoca = Just (0,0), vidaMinhoca = Viva 100
                                                         , jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1
                                                         , minaMinhoca = 1, dinamiteMinhoca = 1 }] })
    , (0, Move Este, estadoComObjeto)
    , (1, Move Este, estadoComMorta)

      -- Jetpack
    , (0, Dispara Jetpack Norte, estadoBase1)
    , (0, Dispara Jetpack Norte, estadoSemMunicoes)

      -- Escavadora
    , (0, Dispara Escavadora Este, estadoBase1)
    , (0, Dispara Escavadora Este, estadoComObjeto)

      -- Bazuca
    , (0, Dispara Bazuca Este, estadoBase1)
    , (0, Dispara Bazuca Este, estadoBase1)  -- disparo duplo
    , (0, Dispara Bazuca Este, Estado { mapaEstado = mapa3x3
                                     , objetosEstado = []
                                     , minhocasEstado =
                                        [ Minhoca { posicaoMinhoca = Just (1,1), vidaMinhoca = Viva 100
                                                  , jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1
                                                  , minaMinhoca = 1, dinamiteMinhoca = 1 }
                                        , Minhoca { posicaoMinhoca = Just (1,0), vidaMinhoca = Viva 100
                                                  , jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1
                                                  , minaMinhoca = 1, dinamiteMinhoca = 1 }
                                        ] })
    , (0, Dispara Bazuca Oeste, Estado { mapaEstado = mapaComTerra
                                      , objetosEstado = []
                                      , minhocasEstado = [Minhoca { posicaoMinhoca = Just (1,1), vidaMinhoca = Viva 100
                                                                    , jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1
                                                                    , minaMinhoca = 1, dinamiteMinhoca = 1 }] })

      -- Mina
    , (0, Dispara Mina Este, estadoBase1)
    , (0, Dispara Mina Este, estadoComObjeto)

      -- Dinamite
    , (0, Dispara Dinamite Este, estadoBase1)
    , (0, Dispara Dinamite Este, estadoComObjeto)

      -- Sequências complexas
    , (0, Move Este, estadoBase2)
    , (0, Dispara Bazuca Sul, estadoBase2)
    , (1, Move Oeste, estadoBase2)

      -- Movimento após morte
    , (0, Move Sul, Estado { mapaEstado = mapaComAguaEMorte
                           , objetosEstado = []
                           , minhocasEstado = [Minhoca { posicaoMinhoca = Just (0,1), vidaMinhoca = Viva 100
                                                       , jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1
                                                       , minaMinhoca = 1, dinamiteMinhoca = 1 }] })
    ]

dataTarefa2 :: IO TaskData
dataTarefa2 = do
  let ins = testesTarefa2
  outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
  return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2
