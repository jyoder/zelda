{ name = "halogen"
, dependencies =
  [ "aff"
  , "arrays"
  , "canvas"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "halogen"
  , "integers"
  , "math"
  , "maybe"
  , "now"
  , "prelude"
  , "psci-support"
  , "refs"
  , "transformers"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "halogen/**/*.purs" ]
}
