let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200822/packages.dhall sha256:b4f151f1af4c5cb6bf5437489f4231fbdd92792deaf32971e6bcb0047b3dd1f8

let additions =
      { elmish =
          https://raw.githubusercontent.com/collegevine/purescript-elmish/master/elmish.dhall sha256:b09a2cec99cd53d59399ad9eb2cf0fe923da7d6a80c58d21d3ef881ecd582a6b
          "v0.2.1"
      , elmish-html =
          https://raw.githubusercontent.com/collegevine/purescript-elmish-html/master/elmish-html.dhall sha256:30e58781aa349b39d388d4570ad31a830a4160dc0b2d6d619a4eb8dd0d4cf040
          "v0.1.2"
      }

in  upstream // additions
