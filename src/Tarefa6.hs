{- |
Module      : Tarefa6
Description : Funcionalidades Extra
-}
module Tarefa6 where

import tipos
import qualified Tarefa4 as T4 -- Importa o bot de elite da Tarefa 4
import Data.List (minimumBy)
import Data.Ord (comparing)

data Dificuldade = Facil | Medio | Dificil deriving (Eq, Show, Read)

-- | Função que integra os extras ambientais no ciclo de vida do jogo.
aplicaExtras :: Ticks -> Estado -> Estado
aplicaExtras t est = 
    let estComAgua = if t > 0 && t `mod` 20 == 0 then maréAlta est else est
    in estComAgua

-- --- IMPLEMENTAÇÃO: SISTEMA DE BOTS (DIFICULDADES) ---
-- | Seletor de IA: Escolhe o "cérebro" da minhoca com base na dificuldade.
-- Esta função é o ponto de entrada para o menu da Tarefa 5.
seletorIA :: Dificuldade -> Ticks -> Estado -> (NumMinhoca, Jogada)
seletorIA Facil   t est = botFacil t est
seletorIA Medio   t est = botMedio t est
seletorIA Dificil t est = T4.jogadaTatica t est 

-- | BOT FÁCIL: "O Errante"
-- Escolhe direções baseadas no tempo sem qualquer análise tática.
botFacil :: Ticks -> Estado -> (NumMinhoca, Jogada)
botFacil t est = (idx, jogada)
  where
    vivas = [i | (i, m) <- zip [0..] (minhocasEstado est), ehViva m]
    idx = if null vivas then 0 else vivas !! (fromIntegral t `mod` length vivas)
    direcoes = [Norte, Este, Sul, Oeste]
    jogada = if t `mod` 3 == 0 
             then Dispara Bazuca (direcoes !! (fromIntegral t `mod` 4))
             else Move (direcoes !! (fromIntegral t `mod` 4))

-- | BOT MÉDIO: "O Soldado"
-- Persegue a minhoca inimiga mais próxima e dispara se estiver perto.
botMedio :: Ticks -> Estado -> (NumMinhoca, Jogada)
botMedio t est = (idx, acao)
  where
    vivas = [i | (i, m) <- zip [0..] (minhocasEstado est), ehViva m]
    idx = if null vivas then 0 else vivas !! (fromIntegral t `mod` length vivas)
    mAtiva = minhocasEstado est !! idx
    pos = case posicaoMinhoca mAtiva of { Just p -> p; Nothing -> (0,0) }
    
    acao = case encontraInimigoMaisProximo est idx pos of
        Nothing -> Move Este
        Just pInimigo -> 
            let dir = direcaoAprox pos pInimigo
            in if distM pos pInimigo < 5
               then Dispara Bazuca dir
               else Move dir

-- --- IMPLEMENTAÇÃO: MARÉ ALTA ---
maréAlta :: Estado -> Estado
maréAlta est = est { mapaEstado = subirAgua (mapaEstado est) }
  where
    subirAgua mapa = reverse $ processaLinhas (reverse mapa) False
    processaLinhas [] _ = []
    processaLinhas (l:ls) jaSubiu
        | not jaSubiu && any (/= Agua) l = replicate (length l) Agua : ls
        | otherwise = l : processaLinhas ls jaSubiu


-- --- FUNÇÕES AUXILIARES ---

ehViva :: Minhoca -> Bool
ehViva m = case vidaMinhoca m of { Viva v -> v > 0; Morta -> False }

distM :: Posicao -> Posicao -> Int
distM (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

direcaoAprox :: Posicao -> Posicao -> Direcao
direcaoAprox (x1,y1) (x2,y2)
    | x2 > x1 = Este
    | x2 < x1 = Oeste
    | y2 > y1 = Sul
    | otherwise = Norte

encontraInimigoMaisProximo :: Estado -> NumMinhoca -> Posicao -> Maybe Posicao
encontraInimigoMaisProximo est meuIdx pos =
    let inimigos = [p | (i, m) <- zip [0..] (minhocasEstado est)
                      , i /= meuIdx, ehViva m, Just p <- [posicaoMinhoca m]]
    in if null inimigos then Nothing 
       else Just $ minimumBy (comparing (distM pos)) inimigos