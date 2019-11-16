{-# LANGUAGE OverloadedStrings #-}

module Main where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Graphics.Vty as V

import qualified Brick.Main as M
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
  , hBox   -- cria uma caixa
  , updateAttrMap
  , withBorderStyle  -- constroi o estilo de borda
  , txt  -- campo de texto
  , str  -- string para ser colocada no Widget
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

styles :: [(T.Text, BS.BorderStyle)]
styles =
    [ ("ascii", BS.ascii)
    , ("unicode", BS.unicode)
    , ("unicode bold", BS.unicodeBold)
    , ("unicode rounded", BS.unicodeRounded)
    , ("custom", custom)
    , ("from 'x'", BS.borderStyleFromChar 'x')
    ]

custom :: BS.BorderStyle
custom =
    BS.BorderStyle { BS.bsCornerTL = '/'
                   , BS.bsCornerTR = '\\'
                   , BS.bsCornerBR = '/'
                   , BS.bsCornerBL = '\\'
                   , BS.bsIntersectFull = '.'
                   , BS.bsIntersectL = '.'
                   , BS.bsIntersectR = '.'
                   , BS.bsIntersectT = '.'
                   , BS.bsIntersectB = '.'
                   , BS.bsHorizontal = '*'
                   , BS.bsVertical = '!'
                   }

borderDemos :: [Widget ()]
borderDemos = mkBorderDemo <$> styles

mkBorderDemo :: (T.Text, BS.BorderStyle) -> Widget ()
mkBorderDemo (styleName, sty) =
    withBorderStyle sty $
    B.borderWithLabel (str "label") $
    vLimit 5 $
    C.vCenter $
    txt $ "  " <> styleName <> " style  "

titleAttr :: A.AttrName
titleAttr = "title"

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
    [ (B.borderAttr,         V.yellow `on` V.black)
    , (titleAttr,            fg V.cyan)
    ]

colorDemo :: Widget ()
colorDemo =
    updateAttrMap (A.applyAttrMappings borderMappings) $     -- é o mapa de atributos geral (retornado por updateAttrMap)
    B.borderWithLabel (withAttr titleAttr $ str "title") $
    hLimit 20 $
    vLimit 3 $
    C.center $
    str $ "colors!"

{-
The default border style is Brick.Widgets.Border.Style.unicode. To change border styles, use the Brick.Widgets.Core.withBorderStyle combinator to wrap a widget and change the border style it uses when rendering. For example, this will use the ascii border style instead of unicode:
-}

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




ui :: Widget ()
ui =
--    hBox borderDemos
    hBox [botaoNumero 1024, botaoNumero 4, botaoNumero (-1), botaoNumero (-1)]
    <=> hBox [botaoNumero 1024, botaoNumero 4, botaoNumero (-1), botaoNumero (-1)]
    <=> hBox [botaoNumero 1024, botaoNumero 4, botaoNumero (-1), botaoNumero (-1)]
    <=> hBox [botaoNumero 1024, botaoNumero 4, botaoNumero (-1), botaoNumero (-1)]

main :: IO ()
main = M.simpleMain ui

