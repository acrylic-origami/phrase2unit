{-# LANGUAGE OverloadedStrings #-}
import P2U.Util ( lor, both )
import P2U.Lang
import P2U.Config

import Data.Map.Lazy ( Map(..) )
import qualified Data.Map.Lazy as M
import Data.Trie ( Trie(..) )
import qualified Data.Trie as Tr
import Data.ByteString.Lazy ( ByteString(..) )
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as W
import qualified Data.ByteString.Char8 as CS
import qualified Data.ByteString as WS
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BSU
import Data.Bits ( shift )
import Data.Char ( toLower )
import Control.Monad.IO.Class ( liftIO )
import Control.Applicative ( liftA2 )
import Control.Arrow ( first, second, (&&&), (***) )
import Data.Maybe ( fromMaybe, listToMaybe, isNothing )
import GHC.Generics
import System.Environment (getArgs)
import Data.Aeson ( ToJSON(..), FromJSON(..), genericToEncoding, defaultOptions )
import qualified Data.Aeson as Aeson ( encode, decode )
import Codec.Text.IConv ( convertFuzzy, Fuzzy(..) )

import Happstack.Server ( Response(..), ServerPart(..), Method(..), ToMessage(..), decodeBody, defaultBodyPolicy, dir, look, nullConf, simpleHTTP, method, toResponse, ok, badRequest )
import qualified Happstack.Server as HS

import Debug.Trace ( trace )

sgns :: [Sign]
sgns = [-1, 1]

scale :: Exp -> SI -> SI
scale n (SI f s) = SI (f ** (realToFrac n)) (M.map (*n) s)

score :: SI -> (Int, Exp)
score (SI _ s) = (M.size $ M.filter (/=0) s, M.foldr ((+) . abs) 0 s)

is_unitless :: SI -> Bool
is_unitless = (==0) . fst . score

json_load :: FromJSON a => String -> IO (Maybe a)
json_load = fmap (Aeson.decode . BLU.fromString) . readFile

solve :: String -> IO (Maybe Result)
solve ph = do
  Just raw_units <- json_load "u2si.json"
  Just raw_pfs <- json_load "prefixes.json"
  Just utypes <- json_load "lim_utypes.json"
  -- (Just _raw_units, (Just raw_pfs, Just utypes)) <- liftIO $
  --   liftA2 (,) (return Nothing) (liftA2 (,) (json_load "prefixes.json") (json_load "utypes.json"))
  let units :: Trie Unit
      units = Tr.fromList $ map (CS.pack . map toLower . u_sym &&& id) raw_units
      
      prefixes :: Trie Prefix
      prefixes = Tr.fromList $ map (CS.pack . map toLower . p_sym &&& id) raw_pfs
      
      ks :: SI -> Maybe [(Sign, UType)] -- brute-force knapsack problem BFS with heuristic
      ks si0 =
        fmap snd $ listToMaybe $ filter (is_unitless . fst)
        $ M.toList $ mconcat
        $ take kS_ITER_LIM
        $ iterate (
            (\x -> trace (show $ M.size x) x) . mconcat . map (
                uncurry (flip M.map)
                . (ks' *** (flip (:)))
              ) . M.toList
          ) (M.fromList [(si0, [])]) where
            ks' :: SI -> Map SI (Sign, UType)
            ks' si = 
              let rscs = [
                      (scale (-sgn) (ut_si ut) <> si, (sgn, ut)) -- invert because we're _solving_ for units
                      | ut <- utypes
                      , sgn <- sgns
                    ]
                  lsc = score si
              in M.fromList $ filter ((<=lsc) . score . fst) rscs -- (\x -> trace (show (si_syms si, map (M.toList . si_syms . ut_si . snd . snd) x)) x )
      
      proc :: C.ByteString -> Maybe [ResultPiece]
      proc s0 = listToMaybe $ dp 0 where
        dp :: Int -> DP
        dp = (map dp' [0..] !!)
        dp' :: Int -> DP -- dp' must be over int instead of string because we need `start`
        dp' n
          | n >= (fromIntegral $ C.length s0) = []
          | otherwise = -- p_ -> flag for: should include prefixes?
            let s = C.drop (fromIntegral n) s0
                get :: Trie a -> [(DP, a)]
                get t = map (\(pre, a, _post) -> (dp (n + CS.length pre), a)) (Tr.matches t (C.toStrict s))
                us = get units
                pus = concatMap (\(rs, pf) -> 
                          map (\(r0:rn) -> r0 {
                              rpc_rng = first (const n) (rpc_rng r0),
                              rpc_m_prefix = Just pf,
                              rpc_stash = (rpc_stash r0) {
                                si_fac = (10.0 ** (fromIntegral $ p_fac pf)) * (si_fac $ rpc_stash r0)
                              }
                            } : rn)
                          $ filter ((==(n+1)) . fst . rpc_rng . head)
                          $ filter (isNothing . rpc_m_prefix . head) rs
                        )
                      $ filter (not . null . fst)
                      $ get prefixes -- only succeed if non-empty is right -- rpc_m_prefix = Just a -- (Tr.matches prefixes s)
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
                resultn = snd $ foldr (\rs z@(sc, l) ->
                    let sc0 = (sum $ map (uncurry (-) . rpc_rng) rs, score (rpc_stash $ head rs))
                        z0 = (Just sc0, [rs])
                    in case sc of
                      Nothing -> z0
                      Just sc' -> case compare sc0 sc' of
                        LT -> z0
                        EQ -> (Just sc', rs : l)
                        GT -> z
                  ) (Nothing, []) (us' <> pus)
              -- (flip const (n, s, us, pus, us', s0)) $ 
              in resultn `lor` (dp (n + 1)) -- keep going even if it's empty
      ph_ascii = map toLower $ C.unpack $ convertFuzzy Discard "UTF-8" "ASCII" (BLU.fromString ph)
      (ph_alpha_only, ph_map) = unzip $ filter (uncurry (&&) . ((>= 'A') &&& (<= 'z')) . fst) (zip ph_ascii [0..])
      m_terms = proc (C.pack ph_alpha_only)
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
    if length ph > mAX_TERM_LEN
      then badRequest $ toResponse $ "Phrase too long (>" ++ (show mAX_TERM_LEN) ++ " characters)"
    else do
      terms <- liftIO $ solve ph
      ok $ toResponse terms

main :: IO ()
main = do
  -- s <- solve "usgal"
  -- putStrLn $ show s
  putStrLn "Listening..."
  simpleHTTP nullConf $ do
    decodeBody (defaultBodyPolicy "/tmp" 0 65536 65536)
    dir "q" solve_ep