{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "control"
  , "datetime"
  , "effect"
  , "elmish-html"
  , "elmish"
  , "enums"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "strings"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
