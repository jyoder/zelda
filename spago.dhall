{ name = "halogen"
, dependencies =
  [ "aff"
  , "canvas"
  , "console"
  , "effect"
  , "halogen"
  , "math"
  , "psci-support"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "halogen/**/*.purs" ]
}
