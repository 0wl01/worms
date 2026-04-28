module Worms where

import Graphics.Gloss
import tipos
import Tarefa6 (Dificuldade(..)) -- Importa Facil | Medio | Dificil

-- | Define os modos de jogo.
data ModoJogo = Normal | MareAlta deriving (Eq, Show)

-- | O estado geral da aplicação gráfica.
data Worms 
    = Menu 
        { imagemFundo :: Picture       
        , tempoDecorrido :: Float      
        , estadoInicial :: Estado      
        , assetsGuardados :: [Picture] 
        , opcaoSelecionada :: Int      
        , faseMenu :: Int              
        , modoGuardado :: ModoJogo     
        , difGuardada :: Dificuldade   
        }
    | A_Jogar 
        { estadoJogo :: Estado         
        , cacheImagens :: [Picture]    
        , tempoTotal :: Float          
        , vezDoBot :: Bool             
        , projetilEmVoo :: Bool        
        , tempoEspera :: Float         
        , contadorTurnos :: Int        
        , modoAtual :: ModoJogo        
        , dificuldadeAtual :: Dificuldade 
        , ultimaDirecao :: Direcao
        , menuBackground :: Picture    
        }
    | FimDeJogo
        { ganhou :: Bool               
        , cacheImagensFim :: [Picture]
        , imagemMenuFim :: Picture     
        , modoReiniciar :: ModoJogo    
        , difReiniciar :: Dificuldade
        }