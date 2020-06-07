{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative (some)
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Coerce (coerce)
import Data.Foldable
import Data.Function (fix)
import Data.List
import qualified Data.Semigroup as Semigroup
import Data.Semigroup.Generic
import Data.String (IsString (..))
import GHC.Generics (Generic)
import qualified Options.Applicative as Opt
import System.Exit
import System.IO (IOMode (WriteMode), withFile)
import qualified System.IO as IO
import qualified System.Process as Process
import Text.Printf (printf)

main :: IO ()
main =
  join (Opt.customExecParser prefs info)
  where
    prefs = Opt.prefs (Opt.showHelpOnError <> Opt.showHelpOnEmpty)
    info = Opt.info (Opt.helper <*> parser) (Opt.fullDesc <> Opt.progDesc "haskell-pref-comparator")

parser :: Opt.Parser (IO ())
parser =
  run <$> runs <*> some prog
  where
    runs =
      Opt.option
        Opt.auto
        (Opt.help "Number of runs" <> Opt.metavar "INT" <> Opt.short 'n' <> Opt.showDefault <> Opt.value 1)
    prog = Opt.strArgument (Opt.metavar "PROGRAM")

run :: Int -> [String] -> IO ()
run runs commands = do
  summaries <- traverse (replicateM runs . run1 . words) commands
  putStrLn (renderTable (summariesToTable (map fold summaries)))

run1 :: [String] -> IO Summary
run1 ~(command : arguments) =
  readSummary
    <$> withFile "/dev/null" WriteMode \devNull ->
      bracket
        ( Process.createProcess
            (Process.proc command (arguments ++ ["+RTS", "-t", "--machine-readable", "-RTS"]))
              { Process.std_in = Process.NoStream,
                Process.std_out = Process.UseHandle devNull,
                Process.std_err = Process.CreatePipe
              }
        )
        (\(_, _, _, processHandle) -> Process.terminateProcess processHandle)
        ( \(_, _, Just stderr, processHandle) -> do
            code <- Process.waitForProcess processHandle
            bytes <- ByteString.hGetContents stderr
            case code of
              ExitSuccess -> pure bytes
              ExitFailure _ -> do
                IO.hPutStrLn IO.stderr ("`" ++ command ++ "` failed: " ++ show bytes)
                exitWith code
        )

data Summary = Summary
  { allocated_bytes :: Avg,
    copied_bytes :: Avg,
    cumulative_live_bytes :: Avg,
    cumulative_par_balanced_copied_bytes :: Avg,
    cumulative_par_max_copied_bytes :: Avg,
    exit_cpu_seconds :: Avg,
    exit_wall_seconds :: Avg,
    fragmentation_bytes :: Avg,
    gc_cpu_seconds :: Avg,
    gc_wall_seconds :: Avg,
    hc_cpu_seconds :: Maybe Avg,
    hc_wall_seconds :: Maybe Avg,
    init_cpu_seconds :: Avg,
    init_wall_seconds :: Avg,
    major_gcs :: Avg,
    max_compact_bytes :: Max,
    max_large_objects_bytes :: Max,
    max_live_bytes :: Max,
    max_mem_in_use_bytes :: Max,
    max_slop_bytes :: Max,
    mut_cpu_seconds :: Avg,
    mut_wall_seconds :: Avg,
    n_capabilities :: Maybe (Semigroup.First Rational),
    num_gcs :: Avg,
    par_copied_bytes :: Avg,
    peak_worker_count :: Maybe Avg,
    rp_cpu_seconds :: Maybe Avg,
    rp_wall_seconds :: Maybe Avg,
    sparks_converted :: Maybe Avg,
    sparks_dud :: Maybe Avg,
    sparks_fizzled :: Maybe Avg,
    sparks_gcd :: Maybe Avg,
    sparks_overflowed :: Maybe Avg,
    task_count :: Maybe Avg,
    total_cpu_seconds :: Avg,
    total_wall_seconds :: Avg,
    worker_count :: Maybe Avg
  }
  deriving stock (Show, Generic)

instance Semigroup Summary where
  (<>) = gmappend

instance Monoid Summary where
  mempty = gmempty
  mappend = (<>)

readSummary :: ByteString -> Summary
readSummary =
  foldMap @[] toSummary . read . ByteString.Char8.unpack

toSummary :: (String, String) -> Summary
toSummary = \case
  ("GC_cpu_seconds", n) -> mempty {gc_cpu_seconds = readAvg n}
  ("GC_wall_seconds", n) -> mempty {gc_wall_seconds = readAvg n}
  ("allocated_bytes", n) -> mempty {allocated_bytes = readAvg n}
  ("copied_bytes", n) -> mempty {copied_bytes = readAvg n}
  ("cumulative_live_bytes", n) -> mempty {cumulative_live_bytes = readAvg n}
  ("cumulative_par_balanced_copied_bytes", n) -> mempty {cumulative_par_balanced_copied_bytes = readAvg n}
  ("cumulative_par_max_copied_bytes", n) -> mempty {cumulative_par_max_copied_bytes = readAvg n}
  ("exit_cpu_seconds", n) -> mempty {exit_cpu_seconds = readAvg n}
  ("exit_wall_seconds", n) -> mempty {exit_wall_seconds = readAvg n}
  ("fragmentation_bytes", n) -> mempty {fragmentation_bytes = readAvg n}
  ("hc_cpu_seconds", n) -> mempty {hc_cpu_seconds = Just (readAvg n)}
  ("hc_wall_seconds", n) -> mempty {hc_wall_seconds = Just (readAvg n)}
  ("init_cpu_seconds", n) -> mempty {init_cpu_seconds = readAvg n}
  ("init_wall_seconds", n) -> mempty {init_wall_seconds = readAvg n}
  ("major_gcs", n) -> mempty {major_gcs = readAvg n}
  ("max_compact_bytes", n) -> mempty {max_compact_bytes = readMax n}
  ("max_large_objects_bytes", n) -> mempty {max_large_objects_bytes = readMax n}
  ("max_live_bytes", n) -> mempty {max_live_bytes = readMax n}
  ("max_mem_in_use_bytes", n) -> mempty {max_mem_in_use_bytes = readMax n}
  ("max_slop_bytes", n) -> mempty {max_slop_bytes = readMax n}
  ("mut_cpu_seconds", n) -> mempty {mut_cpu_seconds = readAvg n}
  ("mut_wall_seconds", n) -> mempty {mut_wall_seconds = readAvg n}
  ("num_GCs", n) -> mempty {num_gcs = readAvg n}
  ("par_copied_bytes", n) -> mempty {par_copied_bytes = readAvg n}
  ("rp_cpu_seconds", n) -> mempty {rp_cpu_seconds = Just (readAvg n)}
  ("rp_wall_seconds", n) -> mempty {rp_wall_seconds = Just (readAvg n)}
  ("total_cpu_seconds", n) -> mempty {total_cpu_seconds = readAvg n}
  ("total_wall_seconds", n) -> mempty {total_wall_seconds = readAvg n}
  ("alloc_rate", _) -> mempty
  ("average_bytes_used", _) -> mempty
  ("bytes allocated", _) -> mempty
  ("gc_cpu_percent", _) -> mempty
  ("gc_wall_percent", _) -> mempty
  ("max_bytes_used", _) -> mempty
  ("num_byte_usage_samples", _) -> mempty
  ("peak_megabytes_allocated", _) -> mempty
  ("productivity_cpu_percent", _) -> mempty
  ("productivity_wall_percent", _) -> mempty
  (s, _) | "gen_" `isPrefixOf` s -> mempty
  (s, _) -> error ("Unknown metric: " ++ show s)
  where
    readAvg :: String -> Avg
    readAvg s =
      Avg (readRational s) 1
    readMax :: String -> Max
    readMax =
      coerce readRational
    readRational :: String -> Rational
    readRational =
      realToFrac . read @Double

summariesToTable :: [Summary] -> [Row]
summariesToTable ~(summary : summaries) =
  Header header : table ++ [Footer]
  where
    header :: [String]
    header =
      (if length summaries > 1 then (++ ["Total"]) else id)
        ("" : "1" : concat (take (length summaries) (map (\i -> ["δ", show i]) [(2 :: Int) ..])))
    table :: [Row]
    table =
      [ seconds "Time" (getAvg . total_wall_seconds) (>),
        seconds "Mutator time" (getAvg . mut_wall_seconds) (>),
        percentage "Mutator time %" (\s -> divide (getAvg (mut_wall_seconds s)) (getAvg (total_wall_seconds s))) (<),
        seconds "GC time" (getAvg . gc_wall_seconds) (>),
        percentage "GC time %" (\s -> divide (getAvg (gc_wall_seconds s)) (getAvg (total_wall_seconds s))) (>),
        -- seconds "Time (rts)" (\s -> getAvg (init_wall_seconds s) + getAvg (exit_wall_seconds s)) (>),
        seconds "CPU time" (getAvg . total_cpu_seconds) (>),
        seconds "Mutator CPU time" (getAvg . mut_cpu_seconds) (>),
        percentage "Mutator CPU time %" (\s -> divide (getAvg (mut_cpu_seconds s)) (getAvg (total_cpu_seconds s))) (<),
        seconds "GC CPU time" (getAvg . gc_cpu_seconds) (>),
        percentage "GC CPU time %" (\s -> divide (getAvg (gc_cpu_seconds s)) (getAvg (total_cpu_seconds s))) (>),
        -- seconds "Time (rts cpu)" (\s -> getAvg (init_cpu_seconds s) + getAvg (exit_cpu_seconds s)) (>),
        Line,
        bytes "Average live memory" average_live_data (>),
        bytes "Max live memory" (coerce max_live_bytes) (>),
        bytes "Memory allocated" (getAvg . allocated_bytes) (>),
        bytes "Memory copied during GC" (getAvg . copied_bytes) (>),
        bytes "Max memory reserved" (coerce max_mem_in_use_bytes) (>),
        Line,
        number "GCs" (getAvg . num_gcs) (>)
      ]
    metric :: (Rational -> String) -> Cell -> (Summary -> Rational) -> (Rational -> Rational -> Bool) -> Row
    metric render name f g =
      Row (name : white (render (f summary)) : cols summary summaries)
      where
        cols :: Summary -> [Summary] -> [Cell]
        cols s0 = \case
          [] ->
            case summaries of
              [] -> []
              _ ->
                let v0 = f summary
                    v1 = f (last summaries)
                 in [delta (g v0 v1) v0 v1]
          s1 : ss ->
            let v0 = f s0
                v1 = f s1
             in delta (g v0 v1) v0 v1 : white (render v1) : cols s1 ss
    bytes :: Cell -> (Summary -> Rational) -> (Rational -> Rational -> Bool) -> Row
    bytes = metric prettyBytes
    number :: Cell -> (Summary -> Rational) -> (Rational -> Rational -> Bool) -> Row
    number = metric (show . round @_ @Int)
    percentage :: Cell -> (Summary -> Rational) -> (Rational -> Rational -> Bool) -> Row
    percentage = metric prettyPercentage
    seconds :: Cell -> (Summary -> Rational) -> (Rational -> Rational -> Bool) -> Row
    seconds = metric prettySeconds
    delta :: Bool -> Rational -> Rational -> Cell
    delta b v1 v2 =
      (if b then green else red)
        ( printf
            "%+.1f%%"
            (realToFrac (divide ((v2 - v1) * 100) v1) :: Double)
        )

alloc_per_second :: Summary -> Rational
alloc_per_second s =
  divide (getAvg (allocated_bytes s)) (getAvg (total_wall_seconds s))

average_live_data :: Summary -> Rational
average_live_data s =
  divide (getAvg (cumulative_live_bytes s)) (getAvg (major_gcs s))

copy_per_second :: Summary -> Rational
copy_per_second s =
  divide (getAvg (copied_bytes s)) (getAvg (total_wall_seconds s))

divide :: Rational -> Rational -> Rational
divide n d =
  if d == 0 then 0 else n / d

--------------------------------------------------------------------------------
-- Pretty-printing
--------------------------------------------------------------------------------

prettyBytes :: Rational -> String
prettyBytes n0
  | n1 <- n0 / 1_000_000_000, n1 >= 1 = prettyNumber n1 ++ " gb"
  | n1 <- n0 / 1_000_000, n1 >= 1 = prettyNumber n1 ++ " mb"
  | n1 <- n0 / 1_000, n1 >= 1 = prettyNumber n1 ++ " kb"
  | otherwise = show (round n0 :: Int) ++ " b"

prettyPercentage :: Rational -> String
prettyPercentage =
  printf "%.1f%%" . realToFrac @_ @Double . (* 100)

prettyNumber :: Rational -> String
prettyNumber n =
  printf "%.*f" p (realToFrac n :: Double)
  where
    p :: Int
    p =
      if n >= 10 then 0 else if n >= 1 then 1 else 2

prettySeconds :: Rational -> String
prettySeconds n0
  | n0 >= 1 = prettyNumber n0 ++ " s"
  | n1 <- n0 * 1_000, n1 >= 1 = prettyNumber n1 ++ " ms"
  | n1 <- n0 * 1_000_000, n1 >= 1 = prettyNumber n1 ++ " µs"
  | n1 <- n0 * 1_000_000_000 = prettyNumber n1 ++ " ns"

--------------------------------------------------------------------------------
-- Table rendering
--------------------------------------------------------------------------------

data Row
  = Row [Cell]
  | Line
  | Header [String]
  | Footer

data Cell
  = Cell Color String

instance IsString Cell where
  fromString =
    white

data Color
  = White
  | Red
  | Green

white :: String -> Cell
white = Cell White

green :: String -> Cell
green = Cell Green

red :: String -> Cell
red = Cell Red

renderTable :: [Row] -> String
renderTable rows =
  unlines (map renderRow rows)
  where
    renderRow :: Row -> String
    renderRow = \case
      Row row -> "│ " ++ intercalate " │ " (map renderCell (zip widths row)) ++ " │"
      Line -> "├" ++ intercalate "┼" (map (\n -> replicate (n + 2) '─') widths) ++ "┤"
      Header labels ->
        "┌"
          ++ intercalate
            "┬"
            (map (\(s, n) -> s ++ replicate (n + 2 - length s) '─') (zip labels widths))
          ++ "┐"
      Footer -> "└" ++ intercalate "┴" (map (\n -> replicate (n + 2) '─') widths) ++ "┘"
    renderCell :: (Int, Cell) -> String
    renderCell (n, Cell color s) =
      case color of
        White -> s'
        Green -> "\ESC[32m" ++ s' ++ "\ESC[39m"
        Red -> "\ESC[31m" ++ s' ++ "\ESC[39m"
      where
        s' = replicate (n - length s) ' ' ++ s
    widths :: [Int]
    widths =
      foldl'
        ( \acc -> \case
            Row cs -> map (\(Cell _ s, n) -> max n (length s)) (zip cs acc)
            Line -> acc
            Header labels -> map (\(label, n) -> max n (length label - 2)) (zip labels acc)
            Footer -> acc
        )
        (take cols (repeat 0))
        rows
    cols :: Int
    cols =
      flip fix rows \loop -> \case
        [] -> error "no colums in table"
        Row cs : _ -> length cs
        _ : rs -> loop rs

--------------------------------------------------------------------------------
-- Random monoids
--------------------------------------------------------------------------------

data Avg
  = Avg !Rational !Rational
  deriving stock (Eq, Show)

instance Monoid Avg where
  mempty = Avg 0 0
  mappend = (<>)

instance Semigroup Avg where
  Avg x1 n1 <> Avg x2 n2 =
    Avg (x1 + x2) (n1 + n2)

getAvg :: Avg -> Rational
getAvg (Avg x n) =
  divide x n

newtype Max = Max {unMax :: Rational}
  deriving stock (Show)

instance Monoid Max where
  mempty = Max 0
  mappend = (<>)

instance Semigroup Max where
  Max x <> Max y =
    Max (max x y)
