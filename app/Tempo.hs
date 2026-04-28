module Tempo where

import tipos
import Tarefa3 (avancaEstado)
import Tarefa2 (efetuaJogada, aplicaRecuo)
import Tarefa6 (aplicaExtras, seletorIA) 
import Worms

reageTempo :: Float -> Worms -> Worms
reageTempo dt (Menu img t est cache op fase mG dG) = Menu img (t + dt) est cache op fase mG dG
reageTempo _ (FimDeJogo v c i m d) = FimDeJogo v c i m d 

reageTempo dt (A_Jogar est cache tTotal vezBot projetilVoar espera turnos modo dif dirMira imgMenu) =
    let minhocas = minhocasEstado est
        jogadorVivo = length minhocas > 0 && vidaOk (minhocas !! 0)
        botVivo = length minhocas > 1 && vidaOk (minhocas !! 1)
        vidaOk m = case vidaMinhoca m of { Viva v -> v > 0; _ -> False }
    in
    if not jogadorVivo then FimDeJogo False cache imgMenu modo dif 
    else if not botVivo then FimDeJogo True cache imgMenu modo dif
    else
        let 
            novoTempo = tTotal + dt
            haProjeteis = not (null (objetosEstado est))
            posMinhocas = map posicaoMinhoca (minhocasEstado est)
            numMinhocas = max 1 (length minhocas)
            nivelAgua = turnos `div` numMinhocas
        
            stateFisica = 
                if haProjeteis || projetilVoar
                then 
                    let estFisica = avancaEstado est
                        posNovas = map posicaoMinhoca (minhocasEstado estFisica)
                        estFinal = case modo of
                            MareAlta -> aplicaExtras nivelAgua estFisica
                            Normal   -> estFisica
                        estavel = null (objetosEstado estFisica) && (posMinhocas == posNovas)
                        (novoProjetilVoar, novosTurnos) = if estavel then (False, turnos + 1) else (True, turnos)

                    in A_Jogar estFinal cache novoTempo vezBot novoProjetilVoar 0 novosTurnos modo dif dirMira imgMenu
                else 
                    processaTurnos dt (A_Jogar est cache novoTempo vezBot False espera turnos modo dif dirMira imgMenu)
        in stateFisica

processaTurnos :: Float -> Worms -> Worms
processaTurnos dt (A_Jogar est cache t vezBot _ espera turnos modo dif dirMira imgMenu)
    | espera > 0 = A_Jogar est cache t vezBot False (espera - dt) turnos modo dif dirMira imgMenu

    | vezBot = 
        let (iBot, jogada) = seletorIA dif (floor t) est
            estBot = efetuaJogada iBot jogada est
            estFinal = case jogada of
                Dispara Bazuca dir -> aplicaRecuo iBot dir estBot
                _                  -> estBot
            ativarProjetil = case jogada of { Dispara _ _ -> True; _ -> False }
            
        in A_Jogar estFinal cache t False ativarProjetil 0 turnos modo dif dirMira imgMenu

    | otherwise = A_Jogar est cache t False False 0 turnos modo dif dirMira imgMenu

processaTurnos _ w = w