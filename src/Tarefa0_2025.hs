module Tarefa0_2025 where
    
import tipos

-- | Retorna a quantidade de munições disponíveis de uma minhoca para uma dada arma.
-- Tipos de Armas disponiveis: Jetpack | Escavadora | Bazuca | Mina | Dinamite
encontraQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int
encontraQuantidadeArmaMinhoca arma m =
    case arma of
        Jetpack   -> jetpackMinhoca m
        Escavadora -> escavadoraMinhoca m
        Bazuca    -> bazucaMinhoca m
        Mina      -> minaMinhoca m
        Dinamite  -> dinamiteMinhoca m

-- | Atualiza a quantidade de munições disponíveis de uma minhoca para uma dada arma.
atualizaQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int -> Minhoca
atualizaQuantidadeArmaMinhoca arma m ammo =
    case arma of
        Jetpack   -> m { jetpackMinhoca = ammo }
        Escavadora -> m { escavadoraMinhoca = ammo }
        Bazuca    -> m { bazucaMinhoca = ammo }
        Mina      -> m { minaMinhoca = ammo }
        Dinamite  -> m { dinamiteMinhoca = ammo }


-- | Verifica se um tipo de terreno é destrutível, i.e., pode ser destruído por explosões.
-- Tipos de Terreno disponiveis: = Ar | Agua | Terra | Pedra
eTerrenoDestrutivel :: Terreno -> Bool
eTerrenoDestrutivel Terra = True
eTerrenoDestrutivel _ = False

--removi as guardas para evitar uma comparação desnecessária "x | x == Terra = True"

-- | Verifica se um tipo de terreno é opaco, i.e., não permite que objetos ou minhocas se encontrem por cima dele.
eTerrenoOpaco :: Terreno -> Bool
eTerrenoOpaco = (`elem` [Terra,Pedra])
--removi as guardas para evitar uma comparação desnecessária "x | x == Terra || x == Pedra = True" e adicionalmente
--esta função fica pronta para mais tipos de terrenos que possam ser adicionados

-- | Verifica se uma posição do mapa está livre, i.e., pode ser ocupada por um objeto ou minhoca.
ePosicaoMapaLivre :: Posicao -> Mapa -> Bool
ePosicaoMapaLivre pos mapa =
    case (encontraPosicaoMatriz pos mapa) of --encontra a posição se for Terreno está dentro do mapa
        Just terreno -> not (eTerrenoOpaco terreno) -- se for terreno opaco, eTerrenoOpaco devolve True; aplicamos not 
        Nothing      -> False  --porque queremos que apenas terrenos não opacos sejam considerados livres.

-- | Verifica se uma posição do estado está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se o mapa estiver livre e se não estiver já uma minhoca ou um barril nessa posição.
ePosicaoEstadoLivre :: Posicao -> Estado -> Bool
ePosicaoEstadoLivre pos estado =
    ePosicaoMapaLivre pos (mapaEstado estado) &&  -- terreno não opaco
    not (any (\m -> posicaoMinhoca m == Just pos) (minhocasEstado estado)) && -- nenhuma minhoca
    not (any (\o -> ehBarril o && posicaoObjeto o == pos) (objetosEstado estado)) -- nenhum barril

-- | Verifica se numa lista de objetos já existe um disparo feito para uma dada arma por uma dada minhoca.
minhocaTemDisparo :: TipoArma -> NumMinhoca -> [Objeto] -> Bool
minhocaTemDisparo arma num =
    any (\obj -> case obj of
        Disparo { tipoDisparo = t, donoDisparo = n } -> t == arma && n == num
        _ -> False)

-- | Destrói uma dada posição no mapa (tipicamente como consequência de uma explosão).
destroiPosicao :: Posicao -> Mapa -> Mapa
destroiPosicao pos mapa =
    case encontraPosicaoMatriz pos mapa of
        Just Terra -> atualizaPosicaoMatriz pos Ar mapa
        _          -> mapa

-- Adiciona um novo objeto a um estado.
adicionaObjeto :: Objeto -> Estado -> Estado
adicionaObjeto obj estado =
    estado { objetosEstado = obj : objetosEstado estado }

posicaoObjeto :: Objeto -> Posicao
posicaoObjeto (Barril { posicaoBarril = p }) = p
posicaoObjeto (Disparo { posicaoDisparo = p }) = p

--Verifica se um objeto é barril
ehBarril :: Objeto -> Bool
ehBarril Barril{} = True
ehBarril _        = False