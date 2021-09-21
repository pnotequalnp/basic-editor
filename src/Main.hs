module Main where

import Brick qualified as B
import Brick.AttrMap (attrMap)
import Brick.Widgets.Edit qualified as B
import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Graphics.Vty qualified as V
import System.Environment (getArgs)

type Editor = B.Editor Text ()

app :: B.App Editor e ()
app =
  B.App
    { B.appDraw = pure . B.renderEditor (B.txt . T.unlines) True,
      B.appChooseCursor = B.showFirstCursor,
      B.appHandleEvent = handleEvent,
      B.appStartEvent = pure,
      B.appAttrMap = const $ attrMap V.defAttr []
    }

main :: IO ()
main = do
  [fp] <- getArgs
  contents <- T.readFile fp <|> pure ""
  editor <- B.defaultMain app $ B.editor () Nothing contents
  let contents' = T.intercalate "\n" $ B.getEditContents editor
  T.writeFile fp contents'

handleEvent :: Editor -> B.BrickEvent () e -> B.EventM () (B.Next Editor)
handleEvent ed = \case
  B.VtyEvent ev -> case ev of
    V.EvKey V.KEsc [] -> B.halt ed
    _ -> B.handleEditorEvent ev ed >>= B.continue
  _ -> B.continue ed
