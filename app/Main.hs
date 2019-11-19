{-# LANGUAGE OverloadedStrings #-}

module Main where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Data.Monoid ((<>))
import Control.Monad (void)
import qualified Data.Text as T
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as BT
import Brick.Util (fg, on)
import qualified Brick.AttrMap as A
import Brick.Types
  ( Widget
  )

import Brick.Widgets.Core
  ( (<=>)
  , (<+>)
  , withAttr
  , vLimit
  , hLimit
  , hBox             -- cria uma caixa
  , updateAttrMap
  , withBorderStyle  -- constroi o estilo de borda
  , txt              -- campo de texto
  , str              -- string para ser colocada no Widget
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

titleAttr :: A.AttrName
titleAttr = "title"

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
    [ (B.borderAttr,         V.yellow `on` V.black)
    , (titleAttr,            fg V.cyan)
    ]

botaoNumero :: Integer -> Widget ()
botaoNumero (-1) = 
    withBorderStyle BS.unicodeBold $
    vLimit 3 $
    hLimit 6 $
    B.borderWithLabel (str "") $
    C.vCenter $
    C.center $
    txt $ T.pack "    "

botaoNumero numero = 
    withBorderStyle BS.unicodeBold $
    vLimit 3 $
    hLimit 6 $
    B.borderWithLabel (str "") $
    C.vCenter $
    C.center $
    txt $ T.pack $ show numero

desenhaUI :: [[Integer]] -> [Widget ()]  -- precisa ser [Widget ()] para interface da app
desenhaUI [[ pos0,  pos1,  pos2,  pos3],
           [ pos4,  pos5,  pos6,  pos7],
           [ pos8,  pos9, pos10, pos11],
           [pos12, pos13, pos14, pos15]
          ] = [
                  hBox [ botaoNumero pos0,  botaoNumero pos1,  botaoNumero pos2,  botaoNumero pos3]
              <=> hBox [ botaoNumero pos4,  botaoNumero pos5,  botaoNumero pos6,  botaoNumero pos7]
              <=> hBox [ botaoNumero pos8,  botaoNumero pos9, botaoNumero pos10, botaoNumero pos11]
              <=> hBox [botaoNumero pos12, botaoNumero pos13, botaoNumero pos14, botaoNumero pos15]
              ]


-- Estrutura tratadorEventos :: s -> BrickEvent n e -> EventM n (Next s)
tratadorEventos :: [[Integer]] -> BT.BrickEvent () e -> BT.EventM () (BT.Next [[Integer]])
tratadorEventos [[ pos0,  pos1,  pos2,  pos3],
                 [ pos4,  pos5,  pos6,  pos7],
                 [ pos8,  pos9, pos10, pos11],
                 [pos12, pos13, pos14, pos15]] (BT.VtyEvent e) = 
    case e of
        V.EvKey (V.KChar 'a') [] -> 
                M.continue [[ (-1),  pos1,  pos2,  pos3],
                            [ (-1),  pos5,  pos6,  pos7],
                            [ (-1),  pos9, pos10, pos11],
                            [ (-1), pos13, pos14, pos15]] 

        V.EvKey (V.KChar 's') [] -> 
                M.continue [[ pos0,  pos1,  2,  pos3],
                           [ pos4,  pos5,  pos6,  pos7],
                          [ pos8,  pos9, pos10, pos11],
                          [(-1), (-1), (-1), (-1)]]

        V.EvKey (V.KChar 'd') [] -> 
                M.continue [[ pos0,  pos1,  pos2,  (-1)],
                           [ pos4,  pos5,  pos6,  (-1)],
                          [ pos8,  pos9, pos10, (-1)],
                          [pos12, pos13, pos14, (-1)]]

        V.EvKey (V.KChar 'w') [] -> 
                M.continue [[ (-1),  (-1),  (-1),  (-1)],
                           [ pos4,  pos5,  pos6,  pos7],
                          [ pos8,  pos9, pos10, pos11],
                          [pos12, pos13, pos14, pos15]]

        V.EvKey V.KEsc [] -> M.halt [[ pos0,  pos1,  pos2,  pos3],
                                     [ pos4,  pos5,  pos6,  pos7],
                                     [ pos8,  pos9, pos10, pos11],
                                     [pos12, pos13, pos14, pos15]]

        ev -> M.continue [[ pos0,  pos1,  pos2,  pos3],
                          [ pos4,  pos5,  pos6,  pos7],
                          [ pos8,  pos9, pos10, pos11],
                          [pos12, pos13, pos14, pos15]]
tratadorEventos estadoJogo2048 _ = M.continue estadoJogo2048


mapaDeAttributos :: [[Integer]] -> A.AttrMap
mapaDeAttributos _ = A.attrMap V.defAttr borderMappings

aplicacao2048 :: M.App [[Integer]] e ()
aplicacao2048 =
    M.App { M.appDraw         = desenhaUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent  = tratadorEventos
          , M.appStartEvent   = return
          , M.appAttrMap      = mapaDeAttributos
          }


main :: IO ()
-- inicia a aplicacao com o estado inicial do tabuleiro
main = void $ M.defaultMain aplicacao2048 [[  2,   4,(-1),(-1)],
                                           [  4,   4,   8,  16],
                                           [(-1),(-1),  4,   2],
                                           [(-1),(-1),(-1),(-1)]]

