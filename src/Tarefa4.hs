{- |
Module      : Tarefa4
Description : Bot de Elite
-}
module Tarefa4 where

import Data.List (maximumBy)
import Data.Ord (comparing)
import tipos
import Tarefa2   -- Executa a jogada (T2)
import Tarefa3   -- Avança o tempo/explosões (T3)

-- | Função Principal: Gera a sequência de 100 jogadas simulando o estado.
tatica :: Estado -> [(NumMinhoca, Jogada)]
tatica estadoInicial = simularTatica 0 estadoInicial
  where
    -- Função auxiliar recursiva para simular 100 passos
    simularTatica :: Ticks -> Estado -> [(NumMinhoca, Jogada)]
    simularTatica 100 _ = [] 
    simularTatica t est = 
        let (idx, jogada) = jogadaTatica t est
            proximoEstado = avancaEstado (efetuaJogada idx jogada est)
        in (idx, jogada) : simularTatica (t + 1) proximoEstado

-- | Define a melhor jogada individual para um dado instante.
jogadaTatica :: Ticks -> Estado -> (NumMinhoca, Jogada)
jogadaTatica t estado 
    | null vivas = (0, Move Este) 
    | otherwise = (idx, melhorAcao estado idx pos)
  where
    vivas = [(i, m) | (i, m) <- zip [0..] (minhocasEstado estado), estaMinhocaViva m]
    (idx, mAtiva) = vivas !! (fromIntegral t `mod` length vivas)
    pos = case posicaoMinhoca mAtiva of { Just p -> p; Nothing -> (0,0) }
    
    opcoes = [Move d | d <- [Norte, Sul, Este, Oeste, Nordeste, Noroeste, Sudeste, Sudoeste]] ++
             [Dispara Bazuca d | d <- [Norte, Sul, Este, Oeste, Nordeste, Noroeste, Sudeste, Sudoeste]]
    
    melhorAcao est i _ = maximumBy (comparing (avaliarFuturo i est)) opcoes

-- | O "Cheat": Avalia o estado futuro após a jogada e o avanço do tempo.
avaliarFuturo :: NumMinhoca -> Estado -> Jogada -> Int
avaliarFuturo idx est jogada =
    let futuro = avancaEstado (efetuaJogada idx jogada est)
        
        vAntes = extrairV (minhocasEstado est !! idx)
        vDepois = extrairV (minhocasEstado futuro !! idx)
        
        penalizacaoDano = if vDepois < vAntes then -10000 else 0
        
        pontosTerra = (contarTerra est - contarTerra futuro) * 20
        pontosDano = calcularDanoInimigos idx est futuro * 50
        
        gps = if pontosTerra == 0 && pontosDano == 0 
              then 100 - distanciaProximaTerra futuro idx
              else 0
              
    in pontosTerra + pontosDano + penalizacaoDano + gps

-- --- FUNÇÕES AUXILIARES ---

contarTerra :: Estado -> Int
contarTerra est = length [(1::Int) | lin <- mapaEstado est, ter <- lin, ter == Terra]

calcularDanoInimigos :: NumMinhoca -> Estado -> Estado -> Int
calcularDanoInimigos meuIdx antes depois =
    sum [ extrairV (minhocasEstado antes !! i) - extrairV (minhocasEstado depois !! i)
        | i <- [0..length (minhocasEstado antes) - 1], i /= meuIdx ]

distanciaProximaTerra :: Estado -> NumMinhoca -> Int
distanciaProximaTerra est idx =
    case posicaoMinhoca (minhocasEstado est !! idx) of
        Nothing -> 1000
        Just p -> let terras = [(x,y) | (y,l) <- zip [0..] (mapaEstado est), (x,t) <- zip [0..] l, t == Terra]
                  in if null terras then 0 else minimum [abs (x-fst p) + abs (y-snd p) | (x,y) <- terras]

estaMinhocaViva :: Minhoca -> Bool
estaMinhocaViva m = case vidaMinhoca m of { Viva v -> v > 0; Morta -> False }

extrairV :: Minhoca -> Int
extrairV m = case vidaMinhoca m of { Viva v -> v; Morta -> 0 }