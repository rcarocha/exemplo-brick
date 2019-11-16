{-# LANGUAGE OverloadedStrings #-}

module Main where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.AttrMap as A

import Brick.Util (fg, on)
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<=>)            -- (layout vertical de caixas) coloca os Widgets alinhados verticalmente 
  , (<+>)            -- (layout horizontal de caixas) coloca os Widgets alinhados horizontalmente 
  , vLimit           -- limitam o espaco vertical de um Widget
  , hLimit           -- limitam o espaco horizontal de um Widget
  , hBox             -- layout de caixa horizontal (agrupamento horizontal de listas de Widgets)
  , withBorderStyle  -- constroi o estilo de borda de caixa de texto
  , txt              -- campo de texto
  , str              -- cria string para ser colocada no Widget
  )

import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS


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
    -- show numero converte inteiro para String




ui :: Widget ()
ui =
        hBox [botaoNumero 1024, botaoNumero 4, botaoNumero (-1), botaoNumero (-1)]
    <=> hBox [botaoNumero 1024, botaoNumero 4, botaoNumero (-1), botaoNumero (-1)]
    <=> hBox [botaoNumero 1024, botaoNumero 4, botaoNumero (-1), botaoNumero (-1)]
    <=> hBox [botaoNumero 1024, botaoNumero 4, botaoNumero (-1), botaoNumero (-1)]

main :: IO ()
main = M.simpleMain ui

