module Tarefa3 where

import Data.Either (partitionEithers)
import tipos
import Tarefa0_2025
import Tarefa2

type Dano = Int
type Danos = [(Posicao, Dano)]

avancaEstado :: Estado -> Estado
avancaEstado estado = 
    let -- 1. Gravidade (Removido o uso de índices desnecessários)
        minhocasNovas = map (avancaMinhoca estado) (minhocasEstado estado)
        est1 = estado { minhocasEstado = minhocasNovas }
        
        -- 2. Objetos e Explosões
        (objetosNovos, listaDanos) = partitionEithers $ 
            map (avancaObjeto est1) (objetosEstado est1)
        est2 = est1 { objetosEstado = objetosNovos }
        
        -- 3. Aplicação de Danos
        danosAgregados = concat listaDanos
    in foldl (flip aplicaDanoIndividual) est2 danosAgregados

avancaMinhoca :: Estado -> Minhoca -> Minhoca
avancaMinhoca estado m = case vidaMinhoca m of
    Morta -> m
    Viva _ -> case posicaoMinhoca m of
        Nothing -> m
        Just pos -> 
            if estaNoAr m estado || estaNaAgua m estado
            then let posAbaixo = movePosicao Sul pos
                     valido = ePosicaoMatrizValida posAbaixo (mapaEstado estado)
                     terrenoAbaixo = encontraPosicaoMatriz posAbaixo (mapaEstado estado)
                 in if not valido 
                    then m { vidaMinhoca = Morta, posicaoMinhoca = Nothing }
                    else if terrenoAbaixo == Just Agua
                         then m { vidaMinhoca = Morta, posicaoMinhoca = Just posAbaixo }
                         else m { posicaoMinhoca = Just posAbaixo }
            else m

avancaObjeto :: Estado -> Objeto -> Either Objeto Danos
avancaObjeto estado obj = case obj of
    Barril pos explode -> 
        if explode || estaNoArObjeto estado pos || estaNaAguaObjeto estado pos
        then Right (geraExplosao pos 5)
        else Left obj

    Disparo pos dir tipo tempo dono -> case tipo of
        Bazuca -> avancaBazuca estado pos dir dono
        Mina -> avancaMina estado pos tempo dono
        Dinamite -> avancaDinamite estado pos dir tempo dono
        _ -> Left obj

avancaBazuca :: Estado -> Posicao -> Direcao -> NumMinhoca -> Either Objeto Danos
avancaBazuca estado pos dir dono =
    let novaPos = movePosicao dir pos
        podeMover = ePosicaoEstadoLivre novaPos estado
    in if not (ePosicaoMatrizValida novaPos (mapaEstado estado)) || not podeMover
       then Right (geraExplosao pos 5)
       else Left (Disparo novaPos dir Bazuca Nothing dono)

avancaMina :: Estado -> Posicao -> Maybe Ticks -> NumMinhoca -> Either Objeto Danos
avancaMina estado pos tempo dono =
    if estaNoArObjeto estado pos || estaNaAguaObjeto estado pos
    then Left (Disparo (movePosicao Sul pos) Norte Mina tempo dono)
    else case tempo of
        Just 0 -> Right (geraExplosao pos 3)
        Just t -> Left (Disparo pos Norte Mina (Just (t-1)) dono)
        Nothing -> if inimigoPerto estado pos dono
                   then Left (Disparo pos Norte Mina (Just 2) dono)
                   else Left (Disparo pos Norte Mina Nothing dono)

avancaDinamite :: Estado -> Posicao -> Direcao -> Maybe Ticks -> NumMinhoca -> Either Objeto Danos
avancaDinamite estado pos dir (Just t) dono
    | t <= 0 = Right (geraExplosao pos 7)
    | estaNoArObjeto estado pos = 
        let novaPos = movePosicao dir pos
            novaDir = roda45 dir
        in if ePosicaoEstadoLivre novaPos estado
           then Left (Disparo novaPos novaDir Dinamite (Just (t-1)) dono)
           else Left (Disparo (movePosicao Sul pos) Norte Dinamite (Just (t-1)) dono)
    | otherwise = Left (Disparo pos Norte Dinamite (Just (t-1)) dono)
avancaDinamite _ _ _ Nothing _ = Right [] 

-- --- FUNÇÕES AUXILIARES ---

geraExplosao :: Posicao -> Int -> Danos
geraExplosao (cx, cy) d =
    [( (x, y), danoCalc ) | x <- [cx-d..cx+d], y <- [cy-d..cy+d]
                          , let dx = abs (x - cx)
                          , let dy = abs (y - cy)
                          , let custo = 3 * min dx dy + 2 * abs (dx - dy)
                          , let danoCalc = (d - custo) * 10
                          , danoCalc > 0]

aplicaDanoIndividual :: (Posicao, Dano) -> Estado -> Estado
aplicaDanoIndividual (posExp, valor) est =
    est { minhocasEstado = map atualizaMinhoca (minhocasEstado est)
        , mapaEstado = destroiTerreno posExp valor (mapaEstado est)
        , objetosEstado = filter (\o -> posicaoObjeto o /= posExp) (objetosEstado est)
        }
  where
    atualizaMinhoca m = case posicaoMinhoca m of
        Just p | p == posExp -> case vidaMinhoca m of
            Viva v -> if v - valor <= 0 
                      then m { vidaMinhoca = Morta, posicaoMinhoca = Nothing }
                      else m { vidaMinhoca = Viva (v - valor) }
            Morta -> m
        _ -> m

destroiTerreno :: Posicao -> Dano -> Mapa -> Mapa
destroiTerreno pos dano mapa =
    case encontraPosicaoMatriz pos mapa of
        Just Terra -> if dano > 0 then atualizaPosicaoMatriz pos Ar mapa else mapa
        _ -> mapa

distManhattan :: Posicao -> Posicao -> Int
distManhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

inimigoPerto :: Estado -> Posicao -> NumMinhoca -> Bool
inimigoPerto est pos donoIdx =
    any (\(i, m) -> i /= donoIdx && estaViva m && distManhattan (posM m) pos <= 1) 
        (zip [0..] (minhocasEstado est))
  where
    estaViva m = case vidaMinhoca m of { Viva _ -> True; _ -> False }
    posM m = case posicaoMinhoca m of { Just p -> p; _ -> (-999,-999) }

estaNoArObjeto :: Estado -> Posicao -> Bool
estaNoArObjeto est p = 
    encontraPosicaoMatriz p (mapaEstado est) == Just Ar && 
    ePosicaoEstadoLivre (movePosicao Sul p) est

estaNaAguaObjeto :: Estado -> Posicao -> Bool
estaNaAguaObjeto est p = encontraPosicaoMatriz p (mapaEstado est) == Just Agua

roda45 :: Direcao -> Direcao
roda45 d = toEnum ((fromEnum d + 1) `mod` 8)