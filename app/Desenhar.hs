module Desenhar where

import Graphics.Gloss
import tipos
import Worms
import Tarefa6

desenha :: Worms -> Picture

-- 1. Desenha o MENU
desenha (Menu img t _ _ opcao fase _ _) = Pictures 
    [ img 
    , conteudoMenu fase opcao t
    , Translate (-450) (-450) $ Scale 0.3 0.3 $ Color white $ Text "Setas: Escolher | Enter: Confirmar"
    ]
    where
        conteudoMenu 0 op tempo = Pictures 
            [ Translate (-500) 50 $ Scale 0.5 0.5 $ Color white $ Text "ESCOLHE O MODO:"
            , desenhaBotao 0 "MODO NORMAL" op tempo
            , desenhaBotao 1 "MARE ALTA" op tempo
            ]
        conteudoMenu 1 op tempo = Pictures 
            [ Translate (-500) 50 $ Scale 0.5 0.5 $ Color white $ Text "ESCOLHE O MAPA:"
            , desenhaBotao 0 "CLASSICO" op tempo
            , desenhaBotao 1 "O VALE" op tempo
            , desenhaBotao 2 "ILHAS" op tempo
            ]
        conteudoMenu 2 op tempo = Pictures 
            [ Translate (-500) 50 $ Scale 0.5 0.5 $ Color white $ Text "DIFICULDADE DO BOT:"
            , desenhaBotao 0 "FACIL (Aleatorio)" op tempo
            , desenhaBotao 1 "MEDIO (Agressivo)" op tempo
            , desenhaBotao 2 "DIFICIL (Tatico)" op tempo
            ]
        conteudoMenu _ _ _ = Blank

        desenhaBotao indice texto selecionado tempo =
            let y = -100 - (fromIntegral indice * 100)
                xBase = -500 
                ehSelecionado = indice == selecionado
                cor = if ehSelecionado then yellow else white
                escala = if ehSelecionado then 0.4 + 0.05 * sin (tempo * 5) else 0.3
                seta = if ehSelecionado then Translate (xBase - 80) (y - 10) $ Color red $ Text ">" else Blank
            in Pictures [ Translate xBase y $ Scale escala escala $ Color cor $ Text texto, seta ]

-- 2. Desenha o JOGO
desenha (A_Jogar estado cache _ vezBot projetilVoar _ _ modo dif _ _) = Pictures 
    [ safeGet 5 Blank 
    , desenhaMapa (mapaEstado estado) cache
    , desenhaObjetos (objetosEstado estado)
    , desenhaMinhocas (minhocasEstado estado) cache
    , desenhaHUD vezBot projetilVoar modo dif
    , desenhaControlos 
    ]
    where
        safeGet i def = if length cache > i then (cache !! i) else def

-- 3. Desenha FIM DE JOGO
desenha (FimDeJogo vitoria cache _ _ _) = Pictures
    [ if vitoria 
      then safeGet 6 (Color green $ rectangleSolid 1920 1080)
      else safeGet 7 (Color red $ rectangleSolid 1920 1080)
    , Translate (-300) (-300) $ Pictures 
        [ Color (makeColorI 0 0 0 150) $ rectangleSolid 400 100
        , Translate (-130) (-10) $ Scale 0.3 0.3 $ Color white $ Text "< MENU (Esq)"
        ]
    , Translate 300 (-300) $ Pictures 
        [ Color (makeColorI 0 0 0 150) $ rectangleSolid 400 100
        , Translate (-150) (-10) $ Scale 0.3 0.3 $ Color white $ Text "REINICIAR (Dir) >"
        ]
    ]
    where
        safeGet i def = if length cache > i then (cache !! i) else def

-- --- FUNÇÕES AUXILIARES ---
desenhaMapa :: Mapa -> [Picture] -> Picture
desenhaMapa mapa cache = Pictures 
    [ Translate (fromIntegral x * 30 - 600) (400 - fromIntegral y * 30) (desenhaBloco t)
    | (y, linha) <- zip ([0..] :: [Int]) mapa
    , (x, t)     <- zip ([0..] :: [Int]) linha
    ]
    where 
      safeGet i def = if length cache > i then (cache !! i) else def
      imgTerra = safeGet 2 (Color (makeColorI 139 69 19 255) $ rectangleSolid 30 30)
      imgPedra = safeGet 3 (Color (greyN 0.5) $ rectangleSolid 30 30)
      imgAgua  = safeGet 4 (Color (light blue) $ rectangleSolid 30 30)
      desenhaBloco Terra = imgTerra
      desenhaBloco Pedra = imgPedra
      desenhaBloco Agua  = imgAgua
      desenhaBloco Ar    = Blank

desenhaMinhocas :: [Minhoca] -> [Picture] -> Picture
desenhaMinhocas ms cache = Pictures 
    [ case posicaoMinhoca m of
        Just (y, x) -> Translate (fromIntegral x * 30 - 600) (400 - fromIntegral y * 30) $ 
                       Scale 0.5 0.5 $ 
                       Pictures [ sprite 
                                , Translate (-10) 25 $ Scale 0.15 0.15 $ Color white $ Text (show vida)
                                ]
        Nothing -> Blank
    -- CORREÇÃO: Anotação :: [Int]
    | (i, m) <- zip ([0..] :: [Int]) ms
    , let vida = case vidaMinhoca m of { Viva v -> v; _ -> 0 }
    , let sprite = if length cache > 1 
                   then if i == 0 then (cache !! 0) else (cache !! 1)
                   else Color (if i == 0 then green else red) (circleSolid 12)
    ]

desenhaObjetos :: [Objeto] -> Picture
desenhaObjetos objs = Pictures 
    [ case obj of
        Disparo (y, x) _ _ _ _ -> Translate (fromIntegral x * 30 - 600) (400 - fromIntegral y * 30) $ 
                                     Color white $ circleSolid 10
        Barril (y, x) _ -> Translate (fromIntegral x * 30 - 600) (400 - fromIntegral y * 30) $ 
                           Color yellow $ rectangleSolid 15 20
    | obj <- objs
    ]

desenhaHUD :: Bool -> Bool -> ModoJogo -> Dificuldade -> Picture
desenhaHUD vezBot projetilVoar modo dif = Pictures
    [ if projetilVoar then Translate (-400) 450 $ Scale 0.3 0.3 $ Color yellow $ Text "A PROCESSAR..."
      else if vezBot  then Translate (-400) 450 $ Scale 0.3 0.3 $ Color red    $ Text "VEZ DO BOT"
      else                 Translate (-400) 450 $ Scale 0.3 0.3 $ Color black  $ Text "TUA VEZ"
    , Translate 200 450 $ Scale 0.2 0.2 $ Color black $ Text ("Modo: " ++ show modo ++ " | Bot: " ++ show dif)
    ]

desenhaControlos :: Picture
desenhaControlos = Translate (-550) (-450) $ Scale 0.15 0.15 $ Color white $ 
    Pictures 
        [ Text "CONTROLOS:"
        , Translate 0 (-150) $ Text "Setas: Mover / Saltar"
        , Translate 0 (-300) $ Text "Espaco: Disparar"
        , Translate 0 (-450) $ Text "BACKSPACE: Sair para Menu"
        , Translate 1500 0   $ Text "NOTA: Mira Dinamica"
        ]