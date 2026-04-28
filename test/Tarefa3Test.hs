{-
Module      : Tarefa3Test
Description : Testes  para a Tarefa 3 - Avançar tempo do jogo
-}
module Main where

import tipos
import Tarefa3
import Magic

-- | Teste 1: Gravidade - minhoca no ar deve cair
estadoGravidadeMinhoca :: Estado
estadoGravidadeMinhoca = Estado 
    [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]] 
    [] 
    [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]

-- | Teste 2: Morte na água - minhoca na água morre instantaneamente
estadoMorteNaAgua :: Estado
estadoMorteNaAgua = Estado 
    [[Ar, Ar, Ar], [Ar, Agua, Ar], [Ar, Ar, Ar]] 
    [] 
    [Minhoca (Just (1,1)) (Viva 100) 1 1 1 1 1]

-- | Teste 3: Bazuca em movimento - deve avançar na direção
estadoBazucaMovimento :: Estado
estadoBazucaMovimento = Estado 
    [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]] 
    [Disparo (1,1) Este Bazuca Nothing 0] 
    []

-- | Teste 4: Bazuca colide com terra e explode
estadoBazucaColisao :: Estado
estadoBazucaColisao = Estado 
    [[Ar, Terra, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]] 
    [Disparo (1,1) Este Bazuca Nothing 0] 
    []

-- | Teste 5: Mina com gravidade - deve cair e direção ficar Norte
estadoMinaGravidade :: Estado
estadoMinaGravidade = Estado 
    [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]] 
    [Disparo (0,1) Sul Mina Nothing 0] 
    []

-- | Teste 7: Mina NÃO ativa por dono - deve permanecer sem tempo (CORRIGIDO)
estadoMinaNaoAtivaDono :: Estado
estadoMinaNaoAtivaDono = Estado 
    [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]] 
    [Disparo (1,1) Sul Mina Nothing 0]  -- Dono 0
    [Minhoca (Just (1,2)) (Viva 100) 1 1 1 1 1]  -- Minhoca índice 0 (mesmo dono)
    -- Comportamento esperado: NÃO ativar

-- | Teste 8: Mina contagem decrescente - tempo 1 → tempo 0 → explode
estadoMinaContagemDecrescente :: Estado
estadoMinaContagemDecrescente = Estado 
    [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]] 
    [Disparo (1,1) Sul Mina (Just 1) 0] 
    []

-- | Teste 9: Dinamite com gravidade - deve cair e direção ficar Norte
estadoDinamiteGravidade :: Estado
estadoDinamiteGravidade = Estado 
    [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]] 
    [Disparo (0,1) Sul Dinamite (Just 3) 0] 
    []

-- | Teste 10: Dinamite contagem decrescente - tempo 1 → tempo 0 → explode
estadoDinamiteExplosao :: Estado
estadoDinamiteExplosao = Estado 
    [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]] 
    [Disparo (1,1) Sul Dinamite (Just 1) 0] 
    []

-- | Teste 11: Dinamite sem tempo - não explode, apenas sofre gravidade
estadoDinamiteSemTempo :: Estado
estadoDinamiteSemTempo = Estado 
    [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]] 
    [Disparo (0,1) Sul Dinamite Nothing 0] 
    []

-- | Teste 12: Barril explode quando marcado
estadoBarrilExplosao :: Estado
estadoBarrilExplosao = Estado 
    [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]] 
    [Barril (1,1) True] 
    []

-- | Teste 13: Barril não explode sem marcação
estadoBarrilEstavel :: Estado
estadoBarrilEstavel = Estado 
    [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]] 
    [Barril (1,1) False] 
    []

-- | Teste 14: Dano de explosão a minhocas
estadoDanoExplosao :: Estado
estadoDanoExplosao = Estado 
    [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]] 
    [Disparo (1,1) Sul Dinamite (Just 1) 0] 
    [Minhoca (Just (1,2)) (Viva 10) 1 1 1 1 1]

-- | Teste 15: Morte por explosão
estadoMorteExplosao :: Estado
estadoMorteExplosao = Estado 
    [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]] 
    [Disparo (1,1) Sul Dinamite (Just 1) 0] 
    [Minhoca (Just (1,2)) (Viva 2) 1 1 1 1 1]

-- | Teste 16: Destruição de terreno por explosão
estadoDestruicaoTerra :: Estado
estadoDestruicaoTerra = Estado 
    [[Ar, Terra, Ar], [Terra, Terra, Terra], [Ar, Terra, Ar]] 
    [Disparo (1,1) Sul Dinamite (Just 1) 0] 
    []

-- | Teste 17: Cadeia de explosões (CORRIGIDO)
estadoCadeiaExplosoes :: Estado
estadoCadeiaExplosoes = Estado 
    [[Ar, Ar, Ar, Ar, Ar], [Ar, Ar, Ar, Ar, Ar], [Ar, Ar, Ar, Ar, Ar], [Ar, Ar, Ar, Ar, Ar], [Ar, Ar, Ar, Ar, Ar]] 
    [ Disparo (2,2) Sul Dinamite (Just 1) 0
    , Barril (2,3) False  -- Barril NÃO marcado, deve explodir pela explosão de dinamite
    ] 
    []

-- | Teste 18: Mina ativada por minhoca diferente (NOVO TESTE)
estadoMinaAtivadaPorDiferente :: Estado
estadoMinaAtivadaPorDiferente = Estado 
    [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]] 
    [Disparo (1,1) Sul Mina Nothing 0]  -- Dono 0
    [ Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1  -- Dono 0 (não ativa)
    , Minhoca (Just (1,2)) (Viva 100) 1 1 1 1 1  -- Dono 1 (deve ativar)
    ]

-- | Teste 19: Jetpack não é afetado pelo tempo
estadoJetpackEstatico :: Estado
estadoJetpackEstatico = Estado 
    [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]] 
    [Disparo (1,1) Sul Jetpack Nothing 0] 
    []

-- | Teste 20: Escavadora não é afetada pelo tempo
estadoEscavadoraEstatica :: Estado
estadoEscavadoraEstatica = Estado 
    [[Ar, Ar, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]] 
    [Disparo (1,1) Sul Escavadora Nothing 0] 
    []

testesTarefa3 :: [Estado]
testesTarefa3 =
    [ estadoGravidadeMinhoca
    , estadoMorteNaAgua
    , estadoBazucaMovimento
    , estadoBazucaColisao 
    , estadoMinaGravidade
    , estadoMinaNaoAtivaDono
    , estadoMinaContagemDecrescente
    , estadoDinamiteGravidade
    , estadoDinamiteExplosao
    , estadoDinamiteSemTempo
    , estadoBarrilExplosao
    , estadoBarrilEstavel
    , estadoDanoExplosao
    , estadoMorteExplosao
    , estadoDestruicaoTerra
    , estadoCadeiaExplosoes
    , estadoMinaAtivadaPorDiferente
    , estadoJetpackEstatico
    , estadoEscavadoraEstatica
    ]

dataTarefa3 :: IO TaskData
dataTarefa3 = do
    let ins = testesTarefa3
    outs <- mapM (runTest . avancaEstado) ins
    return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3