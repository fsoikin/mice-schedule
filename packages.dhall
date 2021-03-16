let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.14.0-20210315/src/packages.dhall sha256:b94bb40844a78703075733d646da563a1dd42a884541841f8d708c08084bbf7e

in  upstream
  with elmish.version = "v0.5.1"
  with elmish-html.version = "v0.3.1"
  with debug.version = "v5.0.0"
