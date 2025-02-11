module Org.Test.Test04h where


import Prelude (($), (#))

import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Types (ListType(..))
import Data.Text.Format.Org.Construct as Org


test :: OrgFile
test =
    Org.f
        $ Org.ds
            [ Org.sec1 1 (Org.text "Node Properties") Org.emptyDoc
                # Org.wprop "NAME" "VALUE"
                # Org.wprop_ "NAME"
                # Org.wprop "NAME+" "VALUE"
                # Org.wprop_ "NAME+"
            , Org.sec1 1 (Org.text "Heading")
                    (Org.db
                        [ Org.with_kws
                            [ Org.kw "KEY" "VALUE"
                            , Org.kwopt "KEY" "VALUE" "OPTVAL"
                            , Org.kw "attr_BACKEND" "VALUE"
                            ]
                            Org.blank
                        , Org.with_kws
                            [ Org.kw "name" "image-name"
                            , Org.kw "caption" "This is a caption for"
                            , Org.kw "caption" "the image linked below"
                            ]
                            $ Org.para1 $ Org.img_ $ Org.iloc "some/image.png"
                        ]
                    )
                # Org.wprop "CUSTOM_ID" "someid"
            ]