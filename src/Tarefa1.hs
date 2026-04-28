module Tarefa1 where

import tipos
import Tarefa0_2025

-- | Função principal da Tarefa 1. Recebe um estado e retorna se este é válido ou não.
validaEstado :: Estado -> Bool
validaEstado estado =
    validaMapa (mapaEstado estado) &&
    all (\(i, m) -> validaMinhoca estado i m) (zip [0..] (minhocasEstado estado)) &&
--cria uma lista das minhocas com os respetivos indices para poder encontrar as minhocas
    all (validaObjeto estado) (objetosEstado estado)

--Verificar estados do mapa

validaMapa :: Mapa -> Bool
validaMapa mapa = 
    not (null mapa) && --garante matriz nao vazia
    not (null (head mapa)) && --garante listas nao vazias
    eMatrizValida mapa &&
    all eTerrenoValido (concat mapa) --concat verifica todos os terrenos [[]] -> []

--Verificar estados de minhocas
--Uma minhoca é valida se todas as suas posicoes sao validas e livres
validaMinhoca :: Estado -> NumMinhoca -> Minhoca -> Bool 
validaMinhoca estado n m =
    posicaoValida &&
    vidaValida &&
    not (minhocaEmCimaObjeto estado m) &&
    all (>= 0) municoes
  where
    mapa = mapaEstado estado

    -- Verifica se a posição da minhoca é válida
    posicaoValida = case posicaoMinhoca m of
        Nothing  -> vidaMinhoca m == Morta  -- sem posição -> deve estar morta
        Just pos -> case encontraPosicaoMatriz pos mapa of
            Nothing   -> vidaMinhoca m == Morta  -- empurrada fora do mapa = morta
            Just Agua -> vidaMinhoca m == Morta  -- água -> deve estar morta
            Just _    -> ePosicaoEstadoLivre pos (removeMinhocaAtual n estado)

    vidaValida = case vidaMinhoca m of
        Viva vida -> vida >= 0 && vida <= 100 --intervalo permitido [0,100]
        Morta  -> True

    -- Lista de todas as municoes
    municoes = [ jetpackMinhoca m
               , escavadoraMinhoca m
               , bazucaMinhoca m
               , minaMinhoca m
               , dinamiteMinhoca m
               ]

validaObjeto :: Estado -> Objeto -> Bool
validaObjeto estado obj = posicaoValida && sobreposicaoValida && tempoValido && donoValido
  where
    mapa     = mapaEstado estado
    minhocas = minhocasEstado estado
    objetos  = objetosEstado estado
    p = posicaoObjeto obj

    -- Verifica se a posição está dentro do mapa
    posicaoValida = ePosicaoMatrizValida p mapa && ePosicaoMapaLivre p mapa

    -- Verifica sobreposição de objetos
    sobreposicaoValida = case obj of
        Barril { posicaoBarril = bpos } ->
            length (filter (\o -> ehBarril o && posicaoObjeto o == bpos) objetos) == 1 && --não há 2 barris na mesma pos
            not (any (\m -> posicaoMinhoca m == Just bpos) minhocas)
        Disparo {} -> True     

    -- Verifica se o tempo do disparo é coerente
    tempoValido = case obj of
        Disparo { tipoDisparo = Bazuca, tempoDisparo = t } -> t == Nothing
        Disparo { tipoDisparo = Mina, tempoDisparo = t }   -> maybe True (\x -> x >= 0 && x <= 2) t
        Disparo { tipoDisparo = Dinamite, tempoDisparo = t } -> maybe True (\x -> x >= 0 && x <= 4) t
        Disparo { tipoDisparo = Jetpack, tempoDisparo = _ } -> False
        Disparo { tipoDisparo = Escavadora, tempoDisparo = _ } -> False
        _ -> True

    -- Verifica dono do disparo
    donoValido = case obj of
        Disparo { donoDisparo = n, tipoDisparo = t } ->
            n >= 0 && n < length minhocas &&
            not (minhocaTemDisparo t n objetos)
        _ -> True

-- | Funções Auxiliares
removeMinhocaAtual :: NumMinhoca -> Estado -> Estado
removeMinhocaAtual n estado =
    estado { minhocasEstado = map snd $ filter (\(i, _) -> i /= n) indexedMinhocas }
  where
    indexedMinhocas = zip [0..] (minhocasEstado estado)

eTerrenoValido :: Terreno -> Bool
eTerrenoValido terreno = terreno `elem` [Ar, Agua, Terra, Pedra]

eDisparoMapaValido :: Posicao -> TipoArma -> Direcao -> Mapa -> Bool
eDisparoMapaValido pos Bazuca dir mapa =
    let posAnterior = movePosicao (opostaDirecao dir) pos
    in case encontraPosicaoMatriz posAnterior mapa of
        Just terreno -> not (eTerrenoOpaco terreno)
        Nothing      -> True
eDisparoMapaValido pos _ _ mapa = ePosicaoMapaLivre pos mapa

opostaDirecao :: Direcao -> Direcao
opostaDirecao Norte     = Sul
opostaDirecao Nordeste  = Sudoeste
opostaDirecao Este      = Oeste
opostaDirecao Sudeste   = Noroeste
opostaDirecao Sul       = Norte
opostaDirecao Sudoeste  = Nordeste
opostaDirecao Oeste     = Este
opostaDirecao Noroeste  = Sudeste

minhocaEmCimaObjeto :: Estado -> Minhoca -> Bool
minhocaEmCimaObjeto estado m =
    case posicaoMinhoca m of
        Nothing -> False
        Just p  -> any (\obj -> ehBarril obj && posicaoObjeto obj == p) (objetosEstado estado)