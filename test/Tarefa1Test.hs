{-
Module      : Tarefa1Test
Description : Testes completos para a Tarefa 1 - Validação de Estados
-}
module Main where

import tipos
import Tarefa1
import Magic

-- MAPAS PARA TESTES

mapa3x3 :: Mapa
mapa3x3 = [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]]

mapaAguaCentro :: Mapa
mapaAguaCentro = [[Ar, Ar, Ar], [Ar, Agua, Ar], [Ar, Ar, Ar]]

mapaComTerraEPedra :: Mapa
mapaComTerraEPedra = [[Ar, Terra, Ar], [Ar, Pedra, Ar], [Ar, Ar, Ar]]

mapaVazio :: Mapa
mapaVazio = []

mapaComLinhaVazia :: Mapa
mapaComLinhaVazia = [[Ar, Terra], [], [Ar]]

mapaComTodosTerrenos :: Mapa
mapaComTodosTerrenos = [[Ar, Agua, Terra], [Pedra, Ar, Agua], [Terra, Pedra, Ar]]

mapaComBordasOpacas :: Mapa
mapaComBordasOpacas = [[Pedra, Pedra, Pedra], [Pedra, Ar, Pedra], [Pedra, Pedra, Pedra]]

mapa1x1 :: Mapa
mapa1x1 = [[Ar]]

-- TESTES DE MAPA

estadoMapaVazio :: Estado
estadoMapaVazio = Estado [] [] []

estadoMapaNaoRetangular :: Estado
estadoMapaNaoRetangular = Estado [[Ar,Ar],[Ar]] [] []

estadoMapaComLinhaVazia :: Estado
estadoMapaComLinhaVazia = Estado mapaComLinhaVazia [] []

estadoMapaValidoCompleto :: Estado
estadoMapaValidoCompleto = Estado mapaComTodosTerrenos [] []

estadoMapaBordasOpacas :: Estado
estadoMapaBordasOpacas = Estado mapaComBordasOpacas [] []

estadoMapaPrimeiraLinhaNaoVaziaSegundaVazia :: Estado
estadoMapaPrimeiraLinhaNaoVaziaSegundaVazia = Estado [[Ar, Terra], [], []] [] []

estadoMinimoValido :: Estado
estadoMinimoValido = Estado mapa1x1 [] []

-- TESTES DE MINHOCAS

-- Minhoca viva em água -> INVÁLIDO
estadoMinhocaVivaNaAgua :: Estado
estadoMinhocaVivaNaAgua = Estado mapaAguaCentro [] [Minhoca (Just (1,1)) (Viva 50) 0 0 0 0 0]

-- Minhoca morta em água -> VÁLIDO
estadoMinhocaMortaNaAgua :: Estado
estadoMinhocaMortaNaAgua = Estado mapaAguaCentro [] [Minhoca (Just (1,1)) Morta 0 0 0 0 0]

-- Minhoca viva fora do mapa -> INVÁLIDO
estadoMinhocaForaDoMapa :: Estado
estadoMinhocaForaDoMapa = Estado mapa3x3 [] [Minhoca (Just (10,10)) (Viva 30) 0 0 0 0 0]

-- Minhoca morta sem posição -> VÁLIDO
estadoMinhocaMortaSemPosicao :: Estado
estadoMinhocaMortaSemPosicao = Estado mapa3x3 [] [Minhoca Nothing Morta 0 0 0 0 0]

-- Minhoca viva sem posição -> INVÁLIDO
estadoMinhocaVivaSemPosicao :: Estado
estadoMinhocaVivaSemPosicao = Estado mapa3x3 [] [Minhoca Nothing (Viva 20) 0 0 0 0 0]

-- Duas minhocas na mesma posição -> INVÁLIDO
estadoMinhocasSobrepostas :: Estado
estadoMinhocasSobrepostas = Estado mapa3x3 [] 
    [ Minhoca (Just (1,1)) (Viva 40) 0 0 0 0 0
    , Minhoca (Just (1,1)) (Viva 60) 0 0 0 0 0
    ]

-- Munições negativas -> INVÁLIDO
estadoMinhocaMunicaoNegativa :: Estado
estadoMinhocaMunicaoNegativa = Estado mapa3x3 [] [Minhoca (Just (1,1)) (Viva 40) (-1) 0 0 0 0]

-- Minhoca com vida negativa -> INVÁLIDO
estadoMinhocaVidaNegativa :: Estado
estadoMinhocaVidaNegativa = Estado mapa3x3 [] [Minhoca (Just (1,1)) (Viva (-5)) 0 0 0 0 0]

-- Minhoca com vida acima de 100 -> INVÁLIDO
estadoMinhocaVidaAcima100 :: Estado
estadoMinhocaVidaAcima100 = Estado mapa3x3 [] [Minhoca (Just (1,1)) (Viva 150) 0 0 0 0 0]

-- Minhoca viva em terreno opaco -> INVÁLIDO
estadoMinhocaVivaEmTerra :: Estado
estadoMinhocaVivaEmTerra = Estado mapaComTerraEPedra [] [Minhoca (Just (0,1)) (Viva 50) 0 0 0 0 0]

estadoMinhocaVivaEmPedra :: Estado
estadoMinhocaVivaEmPedra = Estado mapaComTerraEPedra [] [Minhoca (Just (1,1)) (Viva 50) 0 0 0 0 0]

-- Minhoca morta em terreno opaco -> VÁLIDO
estadoMinhocaMortaEmTerra :: Estado
estadoMinhocaMortaEmTerra = Estado mapaComTerraEPedra [] [Minhoca (Just (0,1)) Morta 0 0 0 0 0]

-- Várias munições negativas -> INVÁLIDO
estadoMinhocaMultiplasMunicoesNegativas :: Estado
estadoMinhocaMultiplasMunicoesNegativas = Estado mapa3x3 [] [Minhoca (Just (1,1)) (Viva 40) (-1) (-2) 0 (-1) 0]

-- Minhoca com vida zero -> INVÁLIDO (deveria ser Morta)
estadoMinhocaVidaZero :: Estado
estadoMinhocaVidaZero = Estado mapa3x3 [] [Minhoca (Just (1,1)) (Viva 0) 0 0 0 0 0]

-- TESTES DE OBJETOS (BARRIS E DISPAROS)

-- Barril sobre minhoca -> INVÁLIDO
estadoBarrilSobreMinhoca :: Estado
estadoBarrilSobreMinhoca = let p = (1,1)
    in Estado mapa3x3 [Barril p False] [Minhoca (Just p) (Viva 20) 0 0 0 0 0]

-- Dois barris na mesma posição -> INVÁLIDO
estadoBarrisSobrepostos :: Estado
estadoBarrisSobrepostos = let p = (1,1)
    in Estado mapa3x3 [Barril p False, Barril p False] []

-- Barril sobre barril -> INVÁLIDO
estadoBarrisMesmaPosicao :: Estado
estadoBarrisMesmaPosicao = let p = (1,1)
    in Estado mapa3x3 [Barril p False, Barril p True] []

-- Barril em terreno opaco -> INVÁLIDO
estadoBarrilEmTerra :: Estado
estadoBarrilEmTerra = Estado mapaComTerraEPedra [Barril (0,1) False] []

-- Barril prestes a explodir -> VÁLIDO
estadoBarrilPrestesExplodir :: Estado
estadoBarrilPrestesExplodir = Estado mapa3x3 [Barril (1,1) True] []

-- Objeto em posição inválida no mapa -> INVÁLIDO
estadoObjetoForaDoMapa :: Estado
estadoObjetoForaDoMapa = Estado mapa3x3 [Barril (10,10) False] []

-- Disparo válido normal -> VÁLIDO
estadoDisparoValido :: Estado
estadoDisparoValido = Estado mapa3x3 [Disparo (1,1) Norte Bazuca Nothing 0] []

-- Disparo de jetpack -> INVÁLIDO
estadoDisparoJetpackInvalido :: Estado
estadoDisparoJetpackInvalido = Estado mapa3x3 [Disparo (1,1) Norte Jetpack (Just 3) 0] []

-- Disparo de escavadora -> INVÁLIDO
estadoDisparoEscavadoraInvalido :: Estado
estadoDisparoEscavadoraInvalido = Estado mapa3x3 [Disparo (1,1) Norte Escavadora Nothing 0] []

-- Disparo de bazuca encostado a pedra -> INVÁLIDO
estadoDisparoBazucaEncostadoPedra :: Estado
estadoDisparoBazucaEncostadoPedra = let p = (1,0)
    in Estado mapaComTerraEPedra [Disparo p Sul Bazuca Nothing 0] []

-- Disparo de bazuca com posição anterior inválida -> INVÁLIDO
estadoDisparoBazucaPosAnteriorInvalida :: Estado
estadoDisparoBazucaPosAnteriorInvalida = let p = (0,0)
    in Estado mapa3x3 [Disparo p Noroeste Bazuca Nothing 0] []

-- Disparo de bazuca com posição anterior opaca -> INVÁLIDO
estadoDisparoBazucaPosAnteriorTerra :: Estado
estadoDisparoBazucaPosAnteriorTerra = let p = (1,1)
    in Estado mapaComTerraEPedra [Disparo p Este Bazuca Nothing 0] []

-- Disparo de bazuca com tempo -> INVÁLIDO
estadoDisparoBazucaComTempo :: Estado
estadoDisparoBazucaComTempo = Estado mapa3x3 [Disparo (1,1) Norte Bazuca (Just 1) 0] []

-- Disparo de bazuca válido (posição livre) -> VÁLIDO
estadoDisparoBazucaValido :: Estado
estadoDisparoBazucaValido = Estado mapa3x3 [Disparo (0,1) Este Bazuca Nothing 0] []

-- Disparo de mina com tempo válido -> VÁLIDO
estadoDisparoMinaTempoValido :: Estado
estadoDisparoMinaTempoValido = Estado mapa3x3 [Disparo (1,1) Norte Mina (Just 2) 0] []

-- Disparo de mina com tempo inválido -> INVÁLIDO
estadoDisparoMinaTempoInvalido :: Estado
estadoDisparoMinaTempoInvalido = Estado mapa3x3 [Disparo (1,1) Norte Mina (Just 5) 0] []

-- Disparo de mina com tempo negativo -> INVÁLIDO
estadoDisparoMinaTempoNegativo :: Estado
estadoDisparoMinaTempoNegativo = Estado mapa3x3 [Disparo (1,1) Norte Mina (Just (-1)) 0] []

-- Disparo de mina sem tempo -> VÁLIDO
estadoDisparoMinaSemTempo :: Estado
estadoDisparoMinaSemTempo = Estado mapa3x3 [Disparo (1,1) Norte Mina Nothing 0] []

-- Disparo de mina em terreno opaco -> INVÁLIDO
estadoDisparoMinaEmTerra :: Estado
estadoDisparoMinaEmTerra = Estado mapaComTerraEPedra [Disparo (0,1) Norte Mina Nothing 0] []

-- Disparo de dinamite com tempo válido -> VÁLIDO
estadoDisparoDinamiteTempoValido :: Estado
estadoDisparoDinamiteTempoValido = Estado mapa3x3 [Disparo (1,1) Norte Dinamite (Just 3) 0] []

-- Disparo de dinamite com tempo inválido -> INVÁLIDO
estadoDisparoDinamiteTempoInvalido :: Estado
estadoDisparoDinamiteTempoInvalido = Estado mapa3x3 [Disparo (1,1) Norte Dinamite (Just 6) 0] []

-- Disparo de dinamite sem tempo -> VÁLIDO
estadoDisparoDinamiteSemTempo :: Estado
estadoDisparoDinamiteSemTempo = Estado mapa3x3 [Disparo (1,1) Norte Dinamite Nothing 0] []

-- Disparo com dono inválido -> INVÁLIDO
estadoDisparoDonoInvalido :: Estado
estadoDisparoDonoInvalido = Estado mapa3x3 [Disparo (1,1) Norte Bazuca Nothing 5] []

-- Disparo fora do mapa -> INVÁLIDO
estadoDisparoForaDoMapa :: Estado
estadoDisparoForaDoMapa = Estado mapa3x3 [Disparo (5,5) Norte Bazuca Nothing 0] []

-- TESTES DE ESTADOS MISTOS

-- Estado completamente válido -> VÁLIDO
estadoCompletamenteValido :: Estado
estadoCompletamenteValido = Estado mapa3x3 
    [Barril (0,0) False, Disparo (2,2) Sul Mina (Just 1) 0]
    [Minhoca (Just (1,1)) (Viva 100) 1 1 1 1 1,
     Minhoca (Just (2,1)) (Viva 80) 0 2 0 3 1]

-- Estado com múltiplos problemas -> INVÁLIDO
estadoMultiplosProblemas :: Estado
estadoMultiplosProblemas = Estado mapaComLinhaVazia 
    [Barril (1,1) False, Disparo (1,1) Norte Bazuca Nothing 0]
    [Minhoca (Just (1,1)) (Viva 50) (-1) 0 0 0 0,
     Minhoca Nothing (Viva 30) 0 0 0 0 0]

-- Múltiplos disparos da mesma arma pela mesma minhoca -> INVÁLIDO
estadoMultiplosDisparosMina :: Estado
estadoMultiplosDisparosMina = Estado mapa3x3 
    [Disparo (1,1) Norte Mina Nothing 0, 
     Disparo (2,2) Sul Mina (Just 1) 0]
    [Minhoca (Just (0,0)) (Viva 100) 0 0 0 0 0]

-- | Lista completa de testes para a Tarefa 1
testesTarefa1 :: [Estado]
testesTarefa1 = 
    [ -- Testes de mapa
      estadoMapaVazio                              -- INVÁLIDO
    , estadoMapaNaoRetangular                      -- INVÁLIDO  
    , estadoMapaComLinhaVazia                      -- INVÁLIDO
    , estadoMapaValidoCompleto                     -- VÁLIDO
    , estadoMapaBordasOpacas                       -- VÁLIDO
    , estadoMapaPrimeiraLinhaNaoVaziaSegundaVazia  -- INVÁLIDO
    , estadoMinimoValido                           -- VÁLIDO
    
    -- Testes de minhocas
    , estadoMinhocaVivaNaAgua                      -- INVÁLIDO
    , estadoMinhocaMortaNaAgua                     -- VÁLIDO
    , estadoMinhocaForaDoMapa                      -- INVÁLIDO
    , estadoMinhocaMortaSemPosicao                 -- VÁLIDO
    , estadoMinhocaVivaSemPosicao                  -- INVÁLIDO
    , estadoMinhocasSobrepostas                    -- INVÁLIDO
    , estadoMinhocaMunicaoNegativa                 -- INVÁLIDO
    , estadoMinhocaVidaNegativa                    -- INVÁLIDO
    , estadoMinhocaVidaAcima100                    -- INVÁLIDO
    , estadoMinhocaVivaEmTerra                     -- INVÁLIDO
    , estadoMinhocaVivaEmPedra                     -- INVÁLIDO
    , estadoMinhocaMortaEmTerra                    -- VÁLIDO
    , estadoMinhocaMultiplasMunicoesNegativas      -- INVÁLIDO
    , estadoMinhocaVidaZero                        -- INVÁLIDO
    
    -- Testes de objetos - Barris
    , estadoBarrilSobreMinhoca                     -- INVÁLIDO
    , estadoBarrisSobrepostos                      -- INVÁLIDO
    , estadoBarrisMesmaPosicao                     -- INVÁLIDO
    , estadoBarrilEmTerra                          -- INVÁLIDO
    , estadoBarrilPrestesExplodir                  -- VÁLIDO
    , estadoObjetoForaDoMapa                       -- INVÁLIDO
    
    -- Testes de objetos - Disparos gerais
    , estadoDisparoValido                          -- VÁLIDO
    , estadoDisparoJetpackInvalido                 -- INVÁLIDO
    , estadoDisparoEscavadoraInvalido              -- INVÁLIDO
    , estadoDisparoDonoInvalido                    -- INVÁLIDO
    , estadoDisparoForaDoMapa                      -- INVÁLIDO
    
    -- Testes de objetos - Disparos Bazuca
    , estadoDisparoBazucaEncostadoPedra            -- INVÁLIDO
    , estadoDisparoBazucaPosAnteriorInvalida       -- INVÁLIDO
    , estadoDisparoBazucaPosAnteriorTerra          -- INVÁLIDO
    , estadoDisparoBazucaComTempo                  -- INVÁLIDO
    , estadoDisparoBazucaValido                    -- VÁLIDO
    
    -- Testes de objetos - Disparos Mina
    , estadoDisparoMinaTempoValido                 -- VÁLIDO
    , estadoDisparoMinaTempoInvalido               -- INVÁLIDO
    , estadoDisparoMinaTempoNegativo               -- INVÁLIDO
    , estadoDisparoMinaSemTempo                    -- VÁLIDO
    , estadoDisparoMinaEmTerra                     -- INVÁLIDO
    
    -- Testes de objetos - Disparos Dinamite
    , estadoDisparoDinamiteTempoValido             -- VÁLIDO
    , estadoDisparoDinamiteTempoInvalido           -- INVÁLIDO
    , estadoDisparoDinamiteSemTempo                -- VÁLIDO
    
    -- Testes mistos
    , estadoCompletamenteValido                    -- VÁLIDO
    , estadoMultiplosProblemas                     -- INVÁLIDO
    , estadoMultiplosDisparosMina                  -- INVÁLIDO
    ]

dataTarefa1 :: IO TaskData 
dataTarefa1 = do
    let ins = testesTarefa1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1

