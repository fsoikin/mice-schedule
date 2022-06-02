let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.15.2-20220531/src/packages.dhall
        sha256:278d3608439187e51136251ebf12fabda62d41ceb4bec9769312a08b56f853e3

in  upstream
  with elmish =
    { repo = "https://github.com/collegevine/purescript-elmish.git"
    , version = "v0.8.0"
    , dependencies =
      [ "aff"
      , "argonaut-core"
      , "arrays"
      , "bifunctors"
      , "console"
      , "debug"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "foreign"
      , "foreign-object"
      , "functions"
      , "integers"
      , "js-date"
      , "maybe"
      , "nullable"
      , "partial"
      , "prelude"
      , "refs"
      , "strings"
      , "typelevel-prelude"
      , "unsafe-coerce"
      , "undefined-is-not-a-problem"
      , "web-dom"
      , "web-html"
      ]
    }
  with elmish-enzyme =
    { dependencies = [ "prelude", "aff-promise" ]
    , repo = "https://github.com/collegevine/purescript-elmish-enzyme.git"
    , version = "v0.1.0"
    }
  with elmish-html =
    { dependencies = [ "prelude", "record" ]
    , repo = "https://github.com/collegevine/purescript-elmish-html.git"
    , version = "v0.7.0"
    }
  with elmish-hooks =
    { dependencies = [ "prelude" ]
    , repo = "https://github.com/collegevine/purescript-elmish-hooks.git"
    , version = "v0.8.0"
    }
  with undefined-is-not-a-problem =
    { repo =
        "https://github.com/fsoikin/purescript-undefined-is-not-a-problem.git"
    , version = "dd812f4ea152ad663d5760e7c1d73bdd85989ccc"
    , dependencies =
      [ "assert"
      , "effect"
      , "either"
      , "foreign"
      , "maybe"
      , "prelude"
      , "random"
      , "tuples"
      , "unsafe-coerce"
      ]
    }
  with uuid =
    { repo = "https://github.com/megamaddu/purescript-uuid.git"
    , version = "7bb5a90c9b11f6a33ac7610608a650e4d58aeac9"
    , dependencies = [ "prelude", "foreign-generic" ]
    }
  with foreign-generic =
    { repo =
        "https://github.com/working-group-purescript-es/purescript-foreign-generic.git"
    , dependencies =
      [ "foreign"
      , "foreign-object"
      , "ordered-collections"
      , "exceptions"
      , "record"
      , "identity"
      ]
    , version = "e7fa22dc9fc2351485f2e915fa7d418ca1965c6d"
    }
