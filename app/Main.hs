module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Labs2025
import Worms
import Desenhar
import Eventos
import Tempo
import Tarefa6

-- | Configuração da Janela.
janela :: Display
janela = InWindow "Worms da Wish" (1920, 1080) (0, 0)

fundo :: Color
fundo = black

fr :: Int
fr = 60

-- | Função principal.
main :: IO ()
main = do
    putStrLn "A carregar Assets da pasta 'assets'..."
    
    maybeMenu <- loadJuicy "assets/worms_menu.png"
    let imgMenu = case maybeMenu of
            Just img -> img
            Nothing  -> Pictures 
                [ Color blue $ rectangleSolid 1920 1080
                , Translate (-300) 0 $ Scale 0.4 0.4 $ Color white $ Text "ERRO: assets/worms_menu.png"
                ]

    -- Carregar Sprites
    wAzul    <- carregaOuErro "assets/worm_azul.png"
    wVerm    <- carregaOuErro "assets/worm_vermelha.png"
    texTerra <- carregaOuErro "assets/worms_terra.png"
    texPedra <- carregaOuErro "assets/worms_pedra.png"
    texAgua  <- carregaOuErro "assets/worms_agua.png"
    bgJogo   <- carregaOuErro "assets/worms_fundomapa.png"
    -- NOVO: Imagens de Fim de Jogo
    bgVitoria <- carregaOuErro "assets/worms_vitoria.png"
    bgDerrota <- carregaOuErro "assets/worms_derrota.png"
    
    -- A ordem é importante para os índices: 
    -- [0:Azul, 1:Verm, 2:Terra, 3:Pedra, 4:Agua, 5:Fundo, 6:Vitoria, 7:Derrota]
    let cacheSprites = [wAzul, wVerm, texTerra, texPedra, texAgua, bgJogo, bgVitoria, bgDerrota]

    -- Estado Inicial Vazio
    let estadoInit = Estado [] [] []

    putStrLn "Jogo pronto!"
    -- Inicia no Menu, Fase 0 (Modo), Opção 0, Dificuldade Facil (default)
    play janela fundo fr (Menu imgMenu 0 estadoInit cacheSprites 0 0 Normal Facil) desenha reageEventos reageTempo

-- | Tenta carregar imagem, retorna placeholder em caso de erro.
carregaOuErro :: FilePath -> IO Picture
carregaOuErro path = do
    maybeImg <- loadJuicy path
    return $ case maybeImg of
        Just img -> img
        Nothing  -> Pictures 
            [ Color red $ rectangleSolid 30 30
            , Color white $ Scale 0.1 0.1 $ Translate (-100) (-50) $ Text ("?" ++ path)
            ]