let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs" ],
  dependencies =
    conf.dependencies #
    [ "spec"
    , "either"
    , "foldable-traversable"
    , "newtype"
    , "aff"
    , "lists"
    , "ordered-collections"
    , "node-buffer"
    , "node-fs"
    , "bifunctors"
    , "debug"
    ]
}