{-# LANGUAGE OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module P2U.Lang where
  
import Data.Map.Lazy ( Map(..) )
import qualified Data.Map.Lazy as M
import Happstack.Server ( ToMessage(..) )
import Data.Aeson ( ToJSON(..), FromJSON(..), genericToEncoding, defaultOptions )
import qualified Data.Aeson as Aeson
import GHC.Generics ( Generic(..) )

type Exp = Int
-- data SIPiece = SIP {
--   name :: String,
--   exp :: Exp
-- }
type Symbol = String
data SI = SI {
  si_fac :: Double,
  si_syms :: Map Symbol Exp -- originally [SIPiece]
} deriving (Generic, Show)

instance Eq SI where
  (SI _ a) == (SI _ b) = a == b

instance Ord SI where
  (SI _ a) `compare` (SI _ b) = a `compare` b

instance Semigroup SI where
  (SI lf ls) <> (SI rf rs) = SI (lf * rf) (M.filter (/=0) $ M.unionWith (+) ls rs)
  
instance Monoid SI where
  mempty = SI 1.0 mempty
  mappend = (<>)
    
data Unit = U {
  u_sym :: Symbol,
  u_name :: String,
  u_link :: String,
  u_ut_name :: String,
  u_si :: SI
} deriving (Generic, Show)

data UType = UT {
  ut_name :: String,
  ut_si :: SI
} deriving (Generic, Show)

instance FromJSON SI
instance FromJSON Unit
instance FromJSON UType

-- data Sign = Mul | Div deriving (Show, Enum)

data ResultPiece = RPc {
  rpc_rng :: (Int, Int), -- start position needed because of skipping, for representation later
  rpc_sgn :: Exp, -- Sign
  rpc_m_prefix :: Maybe Prefix,
  rpc_unit :: Unit,
  rpc_stash :: SI
} deriving (Generic, Show)

data Result = R {
  nice :: Maybe [(Sign, UType)],
  term_raw :: String,
  terms :: [ResultPiece]
} deriving (Generic, Show)

instance ToMessage Result where
  toContentType _ = "application/json"
  toMessage = Aeson.encode

type DP = [[ResultPiece]] -- originally `data DP = DP [Stash] (Maybe DP)`
data Prefix = P {
  p_sym :: Symbol,
  p_name :: String,
  p_fac :: Int
} deriving (Generic, Show)

instance FromJSON Prefix

instance ToJSON SI where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Unit where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON UType where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON ResultPiece where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Result where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Prefix where
  toEncoding = genericToEncoding defaultOptions

type Sign = Exp