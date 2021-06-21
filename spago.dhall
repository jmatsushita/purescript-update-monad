{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "update-monad"
, dependencies =
  [ "arrays"
  , "assert"
  , "console"
  , "effect"
  , "free"
  , "generics-rep"
  , "integers"
  , "monoid-action"
  , "newtype"
  , "pairing"
  , "psci-support"
  , "run"
  , "smash"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
