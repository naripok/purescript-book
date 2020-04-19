{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "book-chapter5"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "effect"
  , "globals"
  , "integers"
  , "math"
  , "maybe"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
