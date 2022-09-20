{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Brick
  ( App (..),
    AttrMap,
    BrickEvent (..),
    EventM,
    Padding (Pad),
    Widget,
    attrMap,
    customMain,
    fill,
    gets,
    hLimit,
    halt,
    modify,
    on,
    padBottom,
    padTop,
    str,
    txt,
    vLimit,
    zoom,
    (<+>),
    (<=>),
  )
import Brick.BChan (newBChan, writeBChan)
import Brick.Focus
  ( focusRingCursor,
  )
import Brick.Forms
  ( Form,
    editTextField,
    focusedFormInputAttr,
    formFocus,
    formState,
    handleFormEvent,
    invalidFormInputAttr,
    newForm,
    renderForm,
    setFieldValid,
    (@@=),
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Monad (forever, void)
import Data.Text (Text, pack, unpack)
import Graphics.Vty (text)
import qualified Graphics.Vty as V
import Lens.Micro ((%~), (^.))
import Lens.Micro.TH (makeLenses)

newtype MathExpression = MathExpression
  { _exprInput :: Text
  }
  deriving (Show)

makeLenses ''MathExpression

data AppResource
  = ExpressionInput
  | VariableBindings Text
  deriving (Show, Eq, Ord)

data AppState e = AppState
  { _expressionForm :: !(Form MathExpression e AppResource),
    _exprDerivative :: !Text,
    _variablesBinding :: ![(Text, Text)],
    _value :: !Integer
  }

makeLenses ''AppState

data UserEvent
  = UserEvent Integer
  deriving (Show, Eq)

calculator :: App (AppState UserEvent) UserEvent AppResource
calculator =
  App
    { appDraw = drawAppState,
      appHandleEvent = handleCalculatorEvent,
      appChooseCursor = focusRingCursor (formFocus . _expressionForm),
      appStartEvent = return (),
      appAttrMap = const theMap
    }

-- | render app state into UI
drawAppState :: AppState e -> [Widget AppResource]
drawAppState state =
  [ C.vCenter $
      C.hCenter
        ( B.borderWithLabel
            (txt "Enter math expresion")
            ( padTop (Pad 1) $
                hLimit 50 $ renderForm (state ^. expressionForm)
            )
            <=> B.borderWithLabel
              (txt "Derivative")
              ( padTop (Pad 1) $ hLimit 50 $ str $ unpack (state ^. exprDerivative)
              )
        )
  ]

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.black),
      (E.editFocusedAttr, V.black `on` V.yellow),
      (invalidFormInputAttr, V.white `on` V.red),
      (focusedFormInputAttr, V.black `on` V.yellow)
    ]

mkExpressionForm :: MathExpression -> Form MathExpression e AppResource
mkExpressionForm =
  let label s w =
        padBottom (Pad 1) $
          vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
   in newForm
        [ editTextField exprInput ExpressionInput (Just 1)
        ]

handleCalculatorEvent :: BrickEvent AppResource UserEvent -> EventM AppResource (AppState UserEvent) ()
handleCalculatorEvent ev = do
  f <- zoom expressionForm $ gets formFocus
  case ev of
    VtyEvent V.EvResize {} -> return ()
    VtyEvent (V.EvKey V.KEsc []) -> halt
    -- Enter quits only when we aren't in the multi-line editor.
    AppEvent (UserEvent i) -> void $ zoom exprDerivative $ modify (\v -> v <> pack (show i))
    _ -> do
      v <- zoom expressionForm $ do
        handleFormEvent ev
        -- Example of external validation:
        -- Require age field to contain a value that is at least 18.
        st <- gets formState
        -- modify $ setFieldValid (st ^. age >= 18) AgeField
        modify $ setFieldValid True ExpressionInput
        pure $ st ^. exprInput
      zoom exprDerivative $
        modify $ const v

main :: IO ()
main = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
      initMathExpr = MathExpression ""
      initialAppState =
        AppState
          { _expressionForm = mkExpressionForm initMathExpr,
            _exprDerivative = "1",
            _variablesBinding = [],
            _value = 0
          }

  eventChan <- newBChan 10

  initialVty <- buildVty
  void $
    async $
      forever $ threadDelay 2000000 <* writeBChan eventChan (UserEvent 1)
  f' <- customMain initialVty buildVty (Just eventChan) calculator initialAppState
  print $ formState $ _expressionForm f'
  pure ()
