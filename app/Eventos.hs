module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import Labs2025
import Tarefa2 (efetuaJogada, aplicaRecuo)
import Worms
import Tarefa6 (Dificuldade(..))

reageEventos :: Event -> Worms -> Worms

-- 1. MENU: FASE 0 (MODO)
reageEventos (EventKey (SpecialKey KeyUp) Down _ _) (Menu img t est cache _ 0 mG dG) =
    Menu img t est cache 0 0 mG dG
reageEventos (EventKey (SpecialKey KeyDown) Down _ _) (Menu img t est cache _ 0 mG dG) =
    Menu img t est cache 1 0 mG dG
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (Menu img t est cache opcao 0 _ dG) =
    let modoEscolhido = if opcao == 0 then Normal else MareAlta
    in Menu img t est cache 0 1 modoEscolhido dG

-- 2. MENU: FASE 1 (MAPA)
reageEventos (EventKey (SpecialKey KeyUp) Down _ _) (Menu img t est cache op 1 mG dG) =
    let novaOp = max 0 (op - 1) in Menu img t est cache novaOp 1 mG dG
reageEventos (EventKey (SpecialKey KeyDown) Down _ _) (Menu img t est cache op 1 mG dG) =
    let novaOp = min 2 (op + 1) in Menu img t est cache novaOp 1 mG dG
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (Menu img t _ cache opcao 1 modo dG) =
    let mapaEscolhido = case opcao of
            0 -> geraMapa1
            1 -> geraMapa2
            _ -> geraMapa3
    in Menu img t (Estado (mapaEstado mapaEscolhido) [] (minhocasEstado mapaEscolhido)) cache 0 2 modo dG

-- 3. MENU: FASE 2 (DIFICULDADE)
reageEventos (EventKey (SpecialKey KeyUp) Down _ _) (Menu img t est cache op 2 mG dG) =
    let novaOp = max 0 (op - 1) in Menu img t est cache novaOp 2 mG dG
reageEventos (EventKey (SpecialKey KeyDown) Down _ _) (Menu img t est cache op 2 mG dG) =
    let novaOp = min 2 (op + 1) in Menu img t est cache novaOp 2 mG dG
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (Menu img _ est cache opcao 2 modo _) =
    let dificuldadeEscolhida = case opcao of
            0 -> Facil
            1 -> Medio
            _ -> Dificil
    in A_Jogar est cache 0 False False 0 0 modo dificuldadeEscolhida Este img

-- 4. JOGO (A_Jogar)
reageEventos (EventKey (SpecialKey KeyDelete) Down _ _) (A_Jogar _ cache t _ _ _ _ _ _ _ imgMenu) =
    Menu imgMenu t (Estado [] [] []) cache 0 0 Normal Facil
reageEventos (EventKey (SpecialKey KeyBackspace) Down _ _) (A_Jogar _ cache t _ _ _ _ _ _ _ imgMenu) =
    Menu imgMenu t (Estado [] [] []) cache 0 0 Normal Facil

reageEventos (EventKey (SpecialKey key) Down _ _) (A_Jogar est cache t ehBot projetilVoar espera turnos modo dif dirMira imgMenu)
    | not ehBot && not projetilVoar && espera <= 0 =
        case key of
            KeyUp    -> A_Jogar (efetuaJogada 0 (Move Norte) est) cache t False True 0 turnos modo dif dirMira imgMenu
            KeyDown  -> A_Jogar (efetuaJogada 0 (Move Sul) est) cache t False False 0 turnos modo dif dirMira imgMenu
            KeyLeft  -> A_Jogar (efetuaJogada 0 (Move Oeste) est) cache t False False 0 turnos modo dif Oeste imgMenu
            KeyRight -> A_Jogar (efetuaJogada 0 (Move Este) est) cache t False False 0 turnos modo dif Este imgMenu
            KeySpace -> 
                let estDisparo = efetuaJogada 0 (Dispara Bazuca dirMira) est
                    estComRecuo = aplicaRecuo 0 dirMira estDisparo
                in A_Jogar estComRecuo cache t True True 0.0 turnos modo dif dirMira imgMenu
            _ -> A_Jogar est cache t ehBot projetilVoar espera turnos modo dif dirMira imgMenu

-- 5. FIM DE JOGO
reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) (FimDeJogo _ cache imgMenu _ _) =
    Menu imgMenu 0 (Estado [] [] []) cache 0 0 Normal Facil
reageEventos (EventKey (SpecialKey KeyRight) Down _ _) (FimDeJogo _ cache imgMenu modo dif) =
    let mapa = geraMapa1 
    in A_Jogar mapa cache 0 False False 0 0 modo dif Este imgMenu

reageEventos _ w = w

-- Mapas
geraMapa1 :: Estado
geraMapa1 = 
    let mapa = replicate 12 (replicate 30 Ar) ++ replicate 8 (replicate 30 Terra)
        m1 = Minhoca (Just (11, 5)) (Viva 100) 10 10 10 10 10
        m2 = Minhoca (Just (11, 24)) (Viva 100) 10 10 10 10 10
    in Estado mapa [] [m1, m2]

geraMapa2 :: Estado
geraMapa2 = 
    let 
        linhaSegura = replicate 30 Ar
        topo = replicate 4 (replicate 5 Pedra ++ replicate 20 Ar ++ replicate 5 Pedra)
        meio = replicate 10 (replicate 5 Terra ++ replicate 20 Ar ++ replicate 5 Terra)
        chao = replicate 5 (replicate 30 Terra)
        mapa = [linhaSegura] ++ topo ++ meio ++ chao
        m1 = Minhoca (Just (0, 6)) (Viva 100) 10 10 10 10 10
        m2 = Minhoca (Just (0, 23)) (Viva 100) 10 10 10 10 10
    in Estado mapa [] [m1, m2]

geraMapa3 :: Estado
geraMapa3 = 
    let ar = replicate 30 Ar
        chaoIlhas = replicate 10 Terra ++ replicate 10 Ar ++ replicate 10 Terra
        baseAgua  = replicate 30 Agua
        mapa = replicate 15 ar ++ replicate 3 chaoIlhas ++ replicate 2 baseAgua
        m1 = Minhoca (Just (14, 8)) (Viva 100) 10 10 10 10 10
        m2 = Minhoca (Just (14, 21)) (Viva 100) 10 10 10 10 10
    in Estado mapa [] [m1, m2]