{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "book-chapter6"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "functions"
  , "psci-support"
  , "strings"
  , "tuples"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
