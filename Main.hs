{-# LANGUAGE DeriveGeneric, OverloadedStrings, GeneralizedNewtypeDeriving #-} -- DeriveFunctor, 
import P2U.Util ( lor, both )

import Data.Map.Strict ( Map(..) )
import qualified Data.Map.Strict as M
import Data.Trie ( Trie(..) )
import qualified Data.Trie as Tr
import Data.ByteString.Lazy ( ByteString(..) )
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as W
import qualified Data.ByteString.Char8 as CS
import qualified Data.ByteString as WS
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BSU
import Data.Char ( toLower )
import Control.Monad.IO.Class ( liftIO )
import Control.Applicative ( liftA2 )
import Control.Arrow ( first, second, (&&&), (***) )
import Data.Maybe ( fromMaybe, listToMaybe )
import GHC.Generics
import System.Environment (getArgs)
import Data.Aeson ( ToJSON(..), FromJSON(..), genericToEncoding, defaultOptions )
import qualified Data.Aeson as Aeson ( encode, decode )
import Codec.Text.IConv ( convertFuzzy, Fuzzy(..) )

import Happstack.Server ( Response(..), ServerPart(..), Method(..), ToMessage(..), decodeBody, defaultBodyPolicy, dir, look, nullConf, simpleHTTP, method, toResponse, ok )
import qualified Happstack.Server as HS

import Debug.Trace ( trace )

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
  nice :: [(Sign, UType)],
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
sgns :: [Sign]
sgns = [-1, 1]

scale :: Exp -> SI -> SI
scale n (SI f s) = SI (f ** (realToFrac n)) (M.map (*n) s)

score :: SI -> Exp
score (SI _ s) = M.foldr ((+) . abs) 0 s

-- TODO String -> ByteString
-- TODO regex-ish stuff (mostly just limiting to alphanum, no regex actually required)

-- proc :: ByteString -> [Stash]
-- proc s0 = head $ dp True 0 where
  
--     in snd $ foldr (\rs z@(s, l) ->
--         let s0 = score (rpc_stash $ head rs)
--             z0 = (Just s0, [rs])
--         in case s of
--           Nothing -> z0
--           Just s' -> case compare s0 s' of
--             LT -> z0
--             EQ -> (Just s', rs : l)
--             GT -> z
--       ) (Nothing, []) (us' <> pus)
    -- trie should take care of strings too short by excluding from `matches`: isn't possible to go too far if check only for empty string

json_load :: FromJSON a => String -> IO (Maybe a)
json_load = fmap (Aeson.decode . BLU.fromString) . readFile

solve :: String -> IO (Maybe Result)
solve ph = do
  Just raw_units <- json_load "u2si.json"
  Just raw_pfs <- json_load "prefixes.json"
  Just utypes <- json_load "utypes.json"
  -- (Just _raw_units, (Just raw_pfs, Just utypes)) <- liftIO $
  --   liftA2 (,) (return Nothing) (liftA2 (,) (json_load "prefixes.json") (json_load "utypes.json"))
  let units :: Trie Unit
      units = Tr.fromList $ map (CS.pack . map toLower . u_sym &&& id) raw_units
      
      prefixes :: Trie Prefix
      prefixes = Tr.fromList $ map (CS.pack . map toLower . p_sym &&& id) raw_pfs
      
      ks :: SI -> [(Sign, UType)] -- brute-force knapsack problem BFS with heuristic
      ks si =
        snd $ head $ filter ((==0) . score . fst) $ concat
        $ iterate (
            concatMap (
                uncurry (flip map)
                . (ks' *** (second . flip (:)))
              )
          ) [(si, [])] where
            ks' :: SI -> [(SI, (Sign, UType))]
            ks' si = 
              let rscs = [
                      (scale (-sgn) (ut_si ut) <> si, (sgn, ut)) -- invert because we're _solving_ for units
                      | ut <- utypes
                      , sgn <- sgns
                    ]
                  lsc = score si
              in filter ((<=(lsc + 1)) . score . fst) rscs
      
      proc :: C.ByteString -> Maybe [ResultPiece]
      proc s0 = listToMaybe $ dp True 0 where
        dp :: Bool -> Int -> DP
        dp b = (map (dp' b) [0..] !!)
        dp' :: Bool -> Int -> DP -- dp' must be over int instead of string because we need `start`
        dp' p_ n
          | n >= (fromIntegral $ C.length s0) = []
          | otherwise = -- p_ -> flag for: should include prefixes?
            let s = C.drop (fromIntegral n) s0
                get :: Bool -> Trie a -> [(DP, a)]
                get b t = map (\(pre, a, _post) -> (dp b (n + CS.length pre), a)) (Tr.matches t (C.toStrict s))
                us = get True units
                pus | p_ = concatMap (\(r0s:rz, pf) -> map (\r0 -> r0 {
                          rpc_m_prefix = Just pf,
                          rpc_stash = (rpc_stash r0) {
                            si_fac = (10.0 ** (fromIntegral $ p_fac pf)) * (si_fac $ rpc_stash r0)
                          }
                        }) r0s : rz)
                      $ filter (not . null . fst)
                      $ get False prefixes -- only succeed if non-empty is right -- rpc_m_prefix = Just a -- (Tr.matches prefixes s)
                    | otherwise = mempty
                us' = [ RPc {
                      rpc_rng = (n, n + (length $ u_sym u) - 1), -- inclusive range, to avoid later lookup failure in ph_map
                      rpc_sgn = sgn,
                      rpc_m_prefix = Nothing,
                      rpc_unit = u,
                      rpc_stash = scale sgn (u_si u) <> fromMaybe mempty (rpc_stash <$> listToMaybe rs)
                    } : rs
                    | (rss, u) <- us -- ONLY OVER `us` and not also pus: this is because we take prefixes straight without augmentation
                    , rs <- rss `lor` [[]]
                    , sgn <- sgns
                  ]
                resultn = snd $ foldr (\rs z@(s, l) ->
                    let s0 = (-(length rs), score (rpc_stash $ head rs))
                        z0 = (Just s0, [rs])
                    in case s of
                      Nothing -> z0
                      Just s' -> case compare s0 s' of
                        LT -> z0
                        EQ -> (Just s', rs : l)
                        GT -> z
                  ) (Nothing, []) (us' <> pus)
              -- (flip const (n, s, us, pus, us', s0)) $ 
              in resultn `lor` (dp p_ (n + 1)) -- keep going even if it's empty
      ph_ascii = map toLower $ C.unpack $ convertFuzzy Discard "UTF-8" "ASCII" (BLU.fromString ph)
      (ph_alpha_only, ph_map) = unzip $ filter (uncurry (&&) . ((>= 'A') &&& (<= 'z')) . fst) (zip ph_ascii [0..])
      m_terms = const (proc (C.pack ph_alpha_only)) (ph_ascii, ph)
      finalize terms = R {
          nice = ks (rpc_stash $ head terms),
          term_raw = ph,
          terms = map (\rpc -> rpc {
              rpc_rng = both (ph_map !!) $ rpc_rng rpc
            }) terms
        }
  return (finalize <$> m_terms)
  
solve_ep :: ServerPart Response
solve_ep = do
    method POST
    ph <- look "term_raw"
    terms <- liftIO $ solve ph
    ok $ toResponse terms

main :: IO ()
main = do
  putStrLn "Listening..."
  simpleHTTP nullConf $ do
    decodeBody (defaultBodyPolicy "/tmp" 0 65536 65536)
    dir "q" solve_ep