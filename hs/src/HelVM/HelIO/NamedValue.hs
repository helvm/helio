module HelVM.HelIO.NamedValue where

data NamedValue a = NamedValue { name :: !String , value :: !a}
