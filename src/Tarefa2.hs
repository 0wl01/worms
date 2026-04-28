{-|
Module      : Tarefa2
Description : Efetuar jogadas.

Módulo para a realização da Tarefa 2 de LI1\/LP1 em 2025\/26.
-}
module Tarefa2 where

import tipos
import Tarefa0_2025
import Tarefa1

-- | Função principal da Tarefa 2. Recebe o índice de uma minhoca na lista de minhocas, uma jogada, um estado e retorna um novo estado em que essa minhoca efetuou essa jogada.
efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada n jogada estado 
    | not (eIndiceListaValido n (minhocasEstado estado)) = estado
    | otherwise =
        case jogada of
            Move dir -> efetuaMovimento n dir estado
            Dispara arma dir -> 
                let proximoEstado = efetuaDisparo n arma dir estado
                -- <--- INTEGRAÇÃO DO EXTRA:
                in if arma == Bazuca 
                   then aplicaRecuo n dir proximoEstado 
                   else proximoEstado
                   
--Movimento

efetuaMovimento :: NumMinhoca -> Direcao -> Estado -> Estado
efetuaMovimento n dir estado =
    case encontraIndiceLista n (minhocasEstado estado) of
        Nothing -> estado --indice invalido
        Just m ->
            case posicaoMinhoca m of
                Nothing -> estado --minhoca sem posicao (morta)
                Just p ->
                    let mapa = mapaEstado estado
                        dimensao = dimensaoMatriz mapa
                        novaPos = movePosicaoJanela dimensao dir p --ja verifica se é fora da janela
                    in if novaPos == p
                        then estado --a nova posição era invalida
                        else processaMovimento m dir novaPos estado n


processaMovimento :: Minhoca -> Direcao -> Posicao -> Estado -> NumMinhoca -> Estado
processaMovimento m dir posNova estado n =
    case vidaMinhoca m of
        Morta -> estado --mortos nao falam
        Viva _ -> 
            let mapa = mapaEstado estado
                terrenoDestino = encontraPosicaoMatriz posNova mapa
                --Testes de Movimentação
                minhocaNoAr = estaNoAr m estado
                destinoLivre = ePosicaoEstadoLivre posNova estado
            in if not destinoLivre then estado --destino ocupado
            else let dirSalto = [Norte, Nordeste, Noroeste]
                 in case terrenoDestino of
                 Nothing -> assassina n True estado --fora do mapa
                 Just Agua -> assassina n False estado --morre mas mantem posicao
                 Just Ar -> 
                    if dir `elem` dirSalto
                    then if not minhocaNoAr then moveMinhoca n posNova estado else estado
                    else moveMinhoca n posNova estado
                 Just _ -> estado

--Disparos
efetuaDisparo :: NumMinhoca -> TipoArma -> Direcao -> Estado -> Estado
efetuaDisparo n arma dir estado =
    case encontraIndiceLista n (minhocasEstado estado) of
        Nothing -> estado -- índice inválido
        Just m ->
            case vidaMinhoca m of
                Morta -> estado -- minhoca morta não dispara
                Viva _ ->
                    let ammo = encontraQuantidadeArmaMinhoca arma m
                    in if ammo <= 0 || minhocaTemDisparo arma n (objetosEstado estado)
                        then estado -- sem munição ou já existe disparo da mesma arma
                        else case posicaoMinhoca m of
                            Nothing -> estado -- fora do mapa
                            Just pos ->
                                let estado1 = gastaMunicao arma n estado
                                in disparaPara n arma dir pos estado1

-- | Reduz 1 unidade de munição da arma disparada pela minhoca.
gastaMunicao :: TipoArma -> NumMinhoca -> Estado -> Estado
gastaMunicao arma n estado =
    case encontraIndiceLista n (minhocasEstado estado) of
        Nothing -> estado
        Just m ->
            let ammoAtual = encontraQuantidadeArmaMinhoca arma m
                mAtualizada = atualizaQuantidadeArmaMinhoca arma m (ammoAtual - 1)
                minhocasNova = atualizaIndiceLista n mAtualizada (minhocasEstado estado)
            in estado { minhocasEstado = minhocasNova }

-- | Realiza o efeito do disparo da arma na posição e direção dadas.
disparaPara :: NumMinhoca -> TipoArma -> Direcao -> Posicao -> Estado -> Estado
disparaPara n Jetpack dir pos estado =
    let novaPos = movePosicao dir pos
    in if ePosicaoEstadoLivre novaPos estado
        then moveMinhoca n novaPos estado
        else estado

disparaPara n Escavadora dir pos estado =
    let alvo = movePosicao dir pos
        mapa = mapaEstado estado
        estado1 = if maybe False eTerrenoDestrutivel (encontraPosicaoMatriz alvo mapa)
                    then estado { mapaEstado = destroiPosicao alvo mapa }
                    else estado
        novaPos = if ePosicaoEstadoLivre alvo estado1 then alvo else pos
    in moveMinhoca n novaPos estado1

disparaPara n Bazuca dir pos estado =
    let alvo = movePosicao dir pos
        disparo = Disparo
            { posicaoDisparo = alvo
            , direcaoDisparo = dir
            , tipoDisparo = Bazuca
            , tempoDisparo = Nothing
            , donoDisparo = n
            }
    in adicionaObjeto disparo estado

disparaPara n Mina dir pos estado =
    let alvo = movePosicao dir pos
        destino = if ePosicaoEstadoLivre alvo estado then alvo else pos
        disparo = Disparo
            { posicaoDisparo = destino
            , direcaoDisparo = dir
            , tipoDisparo = Mina
            , tempoDisparo = Nothing
            , donoDisparo = n
            }
    in adicionaObjeto disparo estado

disparaPara n Dinamite dir pos estado =
    let alvo = movePosicao dir pos
        destino = if ePosicaoEstadoLivre alvo estado then alvo else pos
        disparo = Disparo
            { posicaoDisparo = destino
            , direcaoDisparo = dir
            , tipoDisparo = Dinamite
            , tempoDisparo = Just 4
            , donoDisparo = n
            }
    in adicionaObjeto disparo estado

--Funções Auxiliares

--Função que recebe ordem de assassinato e um bool para caso de manter posição ou não
moveMinhoca :: NumMinhoca -> Posicao -> Estado -> Estado
moveMinhoca n novaPos estado =
    case encontraIndiceLista n (minhocasEstado estado) of
        Nothing -> estado --indice invalido
        Just m ->
            let minhocaNova = m { posicaoMinhoca = Just novaPos }
                minhocasAtualizadas = atualizaIndiceLista n minhocaNova (minhocasEstado estado)
            in estado { minhocasEstado = minhocasAtualizadas } 

assassina :: NumMinhoca -> Bool -> Estado -> Estado
assassina n removePosicao estado =
    case encontraIndiceLista n (minhocasEstado estado) of
        Nothing -> estado --indice invalido
        Just m ->
            let minhocaMorta = m
                    { vidaMinhoca = Morta
                    , posicaoMinhoca = if removePosicao then Nothing 
                                       else posicaoMinhoca m
                    }
                novasMinhocas = atualizaIndiceLista n minhocaMorta (minhocasEstado estado)
            in estado {minhocasEstado = novasMinhocas}

tipoSuporteAbaixo :: Minhoca -> Estado -> Maybe Terreno
tipoSuporteAbaixo m estado =
    case posicaoMinhoca m of
        Nothing -> Nothing
        Just (x,y) -> encontraPosicaoMatriz (x+1,y) (mapaEstado estado)

estaNoAr :: Minhoca -> Estado -> Bool
estaNoAr m estado =
    case posicaoMinhoca m of
        Nothing -> False --minhoca fora do mapa
        Just (x,y) -> 
            case encontraPosicaoMatriz (x,y) (mapaEstado estado) of
                Just Ar -> --terreno atual é ar
                    let posAbaixo = (x+1,y)
                    in ePosicaoEstadoLivre posAbaixo estado --se for livre "esta no ar"
                _ -> False --caso contrário não "está no ar"

estaNaAgua :: Minhoca -> Estado -> Bool
estaNaAgua m estado =
    case posicaoMinhoca m of
        Nothing -> False --minhoca fora do mapa
        Just (x,y) ->
            case encontraPosicaoMatriz (x,y) (mapaEstado estado) of
                Just Agua -> --terreno atual é agua
                    let posAbaixo = (x+1,y)
                    in ePosicaoEstadoLivre posAbaixo estado --se for livre "esta na agua"
                _ -> False

aplicaRecuo :: NumMinhoca -> Direcao -> Estado -> Estado
aplicaRecuo n dir est =
    let minhocas = minhocasEstado est
    in if n >= 0 && n < length minhocas
       then let m = minhocas !! n
                dirRecuo = opostaDirecao dir
            in case posicaoMinhoca m of
                Just pos -> 
                    let posRecuo = movePosicao dirRecuo pos
                    in if ePosicaoEstadoLivre posRecuo est
                       then est { minhocasEstado = atualizaIndiceLista n (m { posicaoMinhoca = Just posRecuo }) minhocas }
                       else est -- Não recua se houver obstáculo
                Nothing -> est
       else est