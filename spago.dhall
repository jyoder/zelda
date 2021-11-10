{ name = "halogen"
, dependencies =
  [ "canvas", "console", "effect", "halogen", "psci-support", "web-html" ]
, packages = ./packages.dhall
, sources = [ "halogen/**/*.purs" ]
}
