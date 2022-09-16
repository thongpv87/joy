{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Brick
  ( App (..),
    AttrMap,
    BrickEvent (VtyEvent),
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
    vLimit,
    zoom,
    (<+>),
    (<=>),
  )
import Brick.BChan (newBChan)
import Brick.Focus
  ( focusGetCurrent,
    focusRingCursor,
  )
import Brick.Forms
  ( Form,
    allFieldsValid,
    checkboxField,
    editPasswordField,
    editShowableField,
    editTextField,
    focusedFormInputAttr,
    formFocus,
    formState,
    handleFormEvent,
    invalidFields,
    invalidFormInputAttr,
    newForm,
    radioField,
    renderForm,
    setFieldValid,
    (@@=),
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Edit (handleEditorEvent)
import qualified Brick.Widgets.Edit as E
import Control.Monad.Cont (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)

newtype MathExpression = MathExpression
  { _expressionInput :: Text
  }
  deriving (Show)

makeLenses ''MathExpression

data AppResource
  = ExpressionInput
  | VariableBindings Text
  deriving (Show, Eq, Ord)

data AppState e = AppState
  { _expressionForm :: !(Form MathExpression e AppResource),
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

drawAppState :: AppState e -> [Widget AppResource]
drawAppState state =
  [ C.vCenter $
      C.hCenter $
        B.border $
          padTop (Pad 1) $
            hLimit 50 $
              renderForm (_expressionForm state)
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
   in newForm [label "Enter Expression: " @@= editTextField expressionInput ExpressionInput (Just 1)]

handleCalculatorEvent :: BrickEvent AppResource UserEvent -> EventM AppResource (AppState UserEvent) ()
handleCalculatorEvent ev = do
  f <- zoom expressionForm $ gets formFocus
  case ev of
    VtyEvent (V.EvResize {}) -> return ()
    VtyEvent (V.EvKey V.KEsc []) -> halt
    -- Enter quits only when we aren't in the multi-line editor.
    _ ->
      do
        zoom expressionForm $ do
          handleFormEvent ev
          -- Example of external validation:
          -- Require age field to contain a value that is at least 18.
          st <- gets formState
          -- modify $ setFieldValid (st ^. age >= 18) AgeField
          modify $ setFieldValid True ExpressionInput

main :: IO ()
main = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
      initMathExpr = MathExpression "x"
      initialAppState =
        AppState
          { _expressionForm = mkExpressionForm initMathExpr,
            _variablesBinding = [],
            _value = 0
          }

  eventChan <- newBChan 10

  initialVty <- buildVty
  f' <- customMain initialVty buildVty (Just eventChan) calculator initialAppState
  print $ formState $ _expressionForm f'
  pure ()
