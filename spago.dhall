{ name = "mercury-parser"
, license = "ISC"
, repository = "https://github.com/malob/purescript-mercury-parser"
, dependencies =
  [ "aff-promise"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "console"
  , "effect"
  , "maybe"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
