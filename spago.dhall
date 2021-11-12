{ name = "halogen"
, dependencies =
  [ "aff"
  , "canvas"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "halogen"
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
