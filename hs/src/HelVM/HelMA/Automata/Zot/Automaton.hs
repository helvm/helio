module HelVM.HelMA.Automata.Zot.Automaton (
  evalParams,
  eval,
) where

import           HelVM.HelMA.Automata.Zot.Evaluator
import           HelVM.HelMA.Automata.Zot.Expression
import           HelVM.HelMA.Automata.Zot.Parser

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.Common.Containers.Util
import           HelVM.Common.Control.Safe

import           HelVM.Common.Digit.Digitable
import           HelVM.Common.Digit.ToDigit

import           HelVM.Common.ListLikeUtil

import           Control.Monad.Writer.Lazy

evalParams :: BIO m => EvalParams -> m ()
evalParams p = wPutStr =<< eval (asciiLabel p) (source p) =<< wGetContents

eval :: MonadSafe m => Bool -> Source -> Input -> m Output
eval False source input = pure $ showFoldable $ evalSource source input
eval True  source input = (makeAsciiText28 . convert . evalSource source) . showExpressionList =<< textToDL input

evalSource :: Source -> Input -> ExpressionDList
evalSource source input = evalText $ source <> input

evalText :: Text -> ExpressionDList
evalText = execWriter . evalExpressionList . parse