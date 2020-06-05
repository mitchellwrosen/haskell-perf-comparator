{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative (optional)
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
  run <$> runs <*> prog <*> optional prog
  where
    runs =
      Opt.option
        Opt.auto
        (Opt.help "Number of runs" <> Opt.metavar "INT" <> Opt.short 'n' <> Opt.showDefault <> Opt.value 1)
    prog = Opt.strArgument (Opt.metavar "PROGRAM")

run :: Int -> String -> Maybe String -> IO ()
run runs command1 = \case
  Nothing -> do
    summaries <- replicateM runs (run1 (words command1))
    putStrLn (renderTable (summaryToTable (fold summaries)))
  Just command2 -> do
    summaries1 <- replicateM runs (run1 (words command1))
    summaries2 <- replicateM runs (run1 (words command2))
    putStrLn (renderTable (summariesToTable (fold summaries1) (fold summaries2)))

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

summaryToTable :: Summary -> [Row]
summaryToTable s@Summary {..} =
  Header : header : Line : table ++ [Footer]
  where
    header = Row ["Metric", "Value"]
    table =
      [ bytes "Memory (avg)" (average_live_data s),
        bytes "Memory (max)" (coerce max_live_bytes),
        bytesPerSecond "Memory (alloc)" (alloc_per_second s),
        seconds "Time (total)" (getAvg total_wall_seconds),
        seconds "Time (program)" (getAvg mut_wall_seconds),
        seconds "Time (gc)" (getAvg gc_wall_seconds),
        seconds "Time (total cpu)" (getAvg total_cpu_seconds),
        seconds "Time (program cpu)" (getAvg mut_cpu_seconds),
        seconds "Time (gc cpu)" (getAvg gc_cpu_seconds),
        percentage "Program (time)" (divide (getAvg mut_wall_seconds) (getAvg total_wall_seconds)),
        percentage "Program (cpu time)" (divide (getAvg mut_cpu_seconds) (getAvg total_cpu_seconds)),
        number "GC (total)" (getAvg num_gcs),
        bytesPerSecond "GC (copy)" (copy_per_second s),
        percentage "GC (time)" (divide (getAvg gc_wall_seconds) (getAvg total_wall_seconds)),
        percentage "GC (cpu time)" (divide (getAvg gc_cpu_seconds) (getAvg total_cpu_seconds))
      ]
    bytes name v = Row [name, white (prettyBytes v)]
    bytesPerSecond name v = Row [name, white (prettyBytesPerSecond v)]
    number name v = Row [name, white (show (round v :: Int))]
    percentage name v = Row [name, white (prettyPercentage v)]
    seconds name v = Row [name, white (prettySeconds v)]

summariesToTable :: Summary -> Summary -> [Row]
summariesToTable s1 s2 =
  Header : header : Line : table ++ [Footer]
  where
    header = Row ["Metric", "Before", "After", "Change"]
    table =
      [ bytes "Memory (avg)" average_live_data (>),
        bytes "Memory (max)" (coerce max_live_bytes) (>),
        bytesPerSecond "Memory (alloc)" alloc_per_second (>),
        seconds "Time (total)" (getAvg . total_wall_seconds) (>),
        seconds "Time (program)" (getAvg . mut_wall_seconds) (>),
        seconds "Time (gc)" (getAvg . gc_wall_seconds) (>),
        -- seconds "Time (rts)" (\s -> getAvg (init_wall_seconds s) + getAvg (exit_wall_seconds s)) (>),
        seconds "Time (total cpu)" (getAvg . total_cpu_seconds) (>),
        seconds "Time (program cpu)" (getAvg . mut_cpu_seconds) (>),
        seconds "Time (gc cpu)" (getAvg . gc_cpu_seconds) (>),
        -- seconds "Time (rts cpu)" (\s -> getAvg (init_cpu_seconds s) + getAvg (exit_cpu_seconds s)) (>),
        percentage
          "Program (time)"
          (\s -> divide (getAvg (mut_wall_seconds s)) (getAvg (total_wall_seconds s)))
          (<),
        percentage
          "Program (cpu time)"
          (\s -> divide (getAvg (mut_cpu_seconds s)) (getAvg (total_cpu_seconds s)))
          (<),
        number "GC (total)" (getAvg . num_gcs),
        bytesPerSecond "GC (copy)" copy_per_second (>),
        percentage
          "GC (time)"
          (\s -> divide (getAvg (gc_wall_seconds s)) (getAvg (total_wall_seconds s)))
          (>),
        percentage
          "GC (cpu time)"
          (\s -> divide (getAvg (gc_cpu_seconds s)) (getAvg (total_cpu_seconds s)))
          (>)
      ]
    bytes :: Cell -> (Summary -> Rational) -> (Rational -> Rational -> Bool) -> Row
    bytes name f g =
      Row [name, white (prettyBytes v1), white (prettyBytes v2), delta (g v1 v2) v1 v2]
      where
        v1 = f s1
        v2 = f s2
    bytesPerSecond :: Cell -> (Summary -> Rational) -> (Rational -> Rational -> Bool) -> Row
    bytesPerSecond name f g =
      Row [name, white (prettyBytesPerSecond v1), white (prettyBytesPerSecond v2), delta (g v1 v2) v1 v2]
      where
        v1 = f s1
        v2 = f s2
    delta :: Bool -> Rational -> Rational -> Cell
    delta b v1 v2 =
      (if b then green else red)
        ( printf
            "%+.1f%%"
            (realToFrac (divide ((v2 - v1) * 100) v1) :: Double)
        )
    number :: Cell -> (Summary -> Rational) -> Row
    number name f =
      Row [name, white (show (round v1 :: Int)), white (show (round v2 :: Int)), delta (v1 > v2) v1 v2]
      where
        v1 = f s1
        v2 = f s2
    percentage :: Cell -> (Summary -> Rational) -> (Rational -> Rational -> Bool) -> Row
    percentage name f g =
      Row [name, white (prettyPercentage v1), white (prettyPercentage v2), delta (g v1 v2) v1 v2]
      where
        v1 = f s1
        v2 = f s2
    seconds :: Cell -> (Summary -> Rational) -> (Rational -> Rational -> Bool) -> Row
    seconds name f g =
      Row [name, white (prettySeconds v1), white (prettySeconds v2), delta (g v1 v2) v1 v2]
      where
        v1 = f s1
        v2 = f s2

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

prettyBytesPerSecond :: Rational -> String
prettyBytesPerSecond =
  (++ "/s") . prettyBytes

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
  | Header
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
      Row row -> "┃ " ++ intercalate " ┃ " (map renderCell (zip widths row)) ++ " ┃"
      Line -> "┠" ++ intercalate "╂" (map (\n -> replicate (n + 2) '─') widths) ++ "┨"
      Header -> "┏" ++ intercalate "┳" (map (\n -> replicate (n + 2) '━') widths) ++ "┓"
      Footer -> "┗" ++ intercalate "┻" (map (\n -> replicate (n + 2) '━') widths) ++ "┛"
    renderCell :: (Int, Cell) -> String
    renderCell (n, Cell color s) =
      case color of
        White -> s'
        Green -> "\ESC[32m" ++ s' ++ "\ESC[39m"
        Red -> "\ESC[31m" ++ s' ++ "\ESC[39m"
      where
        s' = s ++ replicate (n - length s) ' '
    widths :: [Int]
    widths =
      foldl'
        ( \acc -> \case
            Row cs -> map (\(Cell _ s, n) -> max n (length s)) (zip cs acc)
            Line -> acc
            Header -> acc
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
