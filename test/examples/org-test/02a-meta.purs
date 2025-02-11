module Org.Test.Test02a where


import Prelude ((#))

import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Construct as Org


test :: OrgFile
test =
    Org.empty
        # Org.meta "title" "The glories of Org"
        # Org.meta "author" "A. Org Author"