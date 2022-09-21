{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

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
import Brick.Focus
  ( focusGetCurrent,
    focusRingCursor,
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
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import Control.Monad (when)
import Data.Text (Text, unpack)
import Graphics.Vty (Event)
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
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

data AppUI e = AppUI
  { _expressionForm :: !(Form MathExpression e AppResource),
    _exprDerivative :: !Text
  }

makeLenses ''AppUI

data AppEnv = AppEnv

data AppState e = AppState
  { _appUI :: !(AppUI e),
    _appEnv :: !AppEnv
  }

makeLenses ''AppState

data UserAcion
  = InputEvent Event
  | CustomEvent
  deriving (Show, Eq)

calculator :: App (AppState e) e AppResource
calculator =
  App
    { appDraw = drawAppState,
      appHandleEvent = handleCalculatorEvent,
      appChooseCursor = focusRingCursor $ formFocus . (^. appUI . expressionForm),
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
                hLimit 50 $ renderForm (state ^. (appUI . expressionForm))
            )
            <=> B.borderWithLabel
              (txt "Derivative")
              ( padTop (Pad 1) $ hLimit 50 $ str $ unpack (state ^. (appUI . exprDerivative))
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

handleCalculatorEvent :: BrickEvent AppResource e -> EventM AppResource (AppState e) ()
handleCalculatorEvent ev =
  case ev of
    VtyEvent V.EvResize {} -> return ()
    VtyEvent (V.EvKey V.KEsc []) -> halt
    _ -> do
      v <- zoom (appUI . expressionForm) $ do
        handleFormEvent ev
        -- Example of external validation:
        -- Require age field to contain a value that is at least 18.
        st <- gets formState
        -- modify $ setFieldValid (st ^. age >= 18) AgeField
        modify $ setFieldValid True ExpressionInput
        pure $ st ^. exprInput
      zoom (appUI . exprDerivative) $
        modify $ const v

main :: IO ()
main = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
      initialAppUI =
        AppUI
          { _expressionForm = mkExpressionForm $ MathExpression "",
            _exprDerivative = ""
          }
      initialAppState =
        AppState
          { _appUI = initialAppUI,
            _appEnv = AppEnv
          }

  initialVty <- buildVty
  f' <- customMain initialVty buildVty Nothing calculator initialAppState
  print $ formState $ f' ^. (appUI . expressionForm)
  pure ()
