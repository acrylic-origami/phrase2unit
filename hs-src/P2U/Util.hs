module P2U.Util where
import Control.Arrow ( (***) )

both f = f *** f

lor [] b = b
lor a _ = a