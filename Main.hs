{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Coerce (coerce)
import Data.Foldable
import Data.List
import Data.Maybe
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
  run <$> runs <*> some prog <*> many label
  where
    runs =
      Opt.option
        Opt.auto
        (Opt.help "Number of runs" <> Opt.metavar "≪runs≫" <> Opt.short 'n' <> Opt.showDefault <> Opt.value 1)
    prog = Opt.strArgument (Opt.metavar "≪program≫+")
    label = Opt.strOption (Opt.help "Label for corresponding program" <> Opt.long "label" <> Opt.metavar "≪label≫" <> Opt.short 'l')

run :: Int -> [String] -> [String] -> IO ()
run runs commands names = do
  summaries <- traverse (replicateM runs . run1 . words) commands
  putStrLn (renderTable (summariesToTable names (map fold summaries)))

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
    -- The number of generations is tweakable, but I don't feel like handling that right now
    gen_0_collections :: Avg,
    gen_0_cpu_seconds :: Avg,
    gen_0_max_pause_seconds :: Max,
    gen_0_par_collections :: Avg,
    gen_0_sync_spin :: Avg,
    gen_0_sync_yield :: Avg,
    gen_0_wall_seconds :: Avg,
    gen_1_collections :: Avg,
    gen_1_cpu_seconds :: Avg,
    gen_1_max_pause_seconds :: Max,
    gen_1_par_collections :: Avg,
    gen_1_sync_spin :: Avg,
    gen_1_sync_yield :: Avg,
    gen_1_wall_seconds :: Avg,
    --
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
  ("gen_0_collections", n) -> mempty {gen_0_collections = readAvg n}
  ("gen_0_cpu_seconds", n) -> mempty {gen_0_cpu_seconds = readAvg n}
  ("gen_0_max_pause_seconds", n) -> mempty {gen_0_max_pause_seconds = readMax n}
  ("gen_0_par_collections", n) -> mempty {gen_0_par_collections = readAvg n}
  ("gen_0_sync_spin", n) -> mempty {gen_0_sync_spin = readAvg n}
  ("gen_0_sync_yield", n) -> mempty {gen_0_sync_yield = readAvg n}
  ("gen_0_wall_seconds", n) -> mempty {gen_0_wall_seconds = readAvg n}
  ("gen_1_collections", n) -> mempty {gen_1_collections = readAvg n}
  ("gen_1_cpu_seconds", n) -> mempty {gen_1_cpu_seconds = readAvg n}
  ("gen_1_max_pause_seconds", n) -> mempty {gen_1_max_pause_seconds = readMax n}
  ("gen_1_par_collections", n) -> mempty {gen_1_par_collections = readAvg n}
  ("gen_1_sync_spin", n) -> mempty {gen_1_sync_spin = readAvg n}
  ("gen_1_sync_yield", n) -> mempty {gen_1_sync_yield = readAvg n}
  ("gen_1_wall_seconds", n) -> mempty {gen_1_wall_seconds = readAvg n}
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
  ("n_capabilities", n) -> mempty {n_capabilities = Just (Semigroup.First (readRational n))}
  ("par_copied_bytes", n) -> mempty {par_copied_bytes = readAvg n}
  ("peak_worker_count", n) -> mempty {peak_worker_count = Just (readAvg n)}
  ("rp_cpu_seconds", n) -> mempty {rp_cpu_seconds = Just (readAvg n)}
  ("rp_wall_seconds", n) -> mempty {rp_wall_seconds = Just (readAvg n)}
  ("sparks_converted", n) -> mempty {sparks_converted = Just (readAvg n)}
  ("sparks_dud ", n) -> mempty {sparks_dud = Just (readAvg n)}
  ("sparks_fizzled", n) -> mempty {sparks_fizzled = Just (readAvg n)}
  ("sparks_gcd", n) -> mempty {sparks_gcd = Just (readAvg n)}
  ("sparks_overflowed", n) -> mempty {sparks_overflowed = Just (readAvg n)}
  ("task_count", n) -> mempty {task_count = Just (readAvg n)}
  ("total_cpu_seconds", n) -> mempty {total_cpu_seconds = readAvg n}
  ("total_wall_seconds", n) -> mempty {total_wall_seconds = readAvg n}
  ("worker_count", n) -> mempty {worker_count = Just (readAvg n)}
  -- Look into these, maybe
  ("any_work", _) -> mempty
  ("gc_alloc_block_sync_spin", _) -> mempty
  ("gc_alloc_block_sync_yield", _) -> mempty
  ("gc_spin_spin", _) -> mempty
  ("gc_spin_yield", _) -> mempty
  ("mut_spin_spin", _) -> mempty
  ("mut_spin_yield", _) -> mempty
  ("no_work", _) -> mempty
  ("scav_find_work", _) -> mempty
  ("waitForGcThreads_spin", _) -> mempty
  ("waitForGcThreads_yield", _) -> mempty
  ("whitehole_executeMessage_spin", _) -> mempty
  ("whitehole_gc_spin", _) -> mempty
  ("whitehole_lockClosure_spin", _) -> mempty
  ("whitehole_lockClosure_yield", _) -> mempty
  ("whitehole_threadPaused_spin", _) -> mempty
  -- Don't need these
  ("alloc_rate", _) -> mempty
  ("average_bytes_used", _) -> mempty
  ("bound_task_count", _) -> mempty
  ("bytes allocated", _) -> mempty
  ("gc_cpu_percent", _) -> mempty
  ("gc_wall_percent", _) -> mempty
  ("gen_0_avg_pause_seconds", _) -> mempty
  ("gen_1_avg_pause_seconds", _) -> mempty
  ("max_bytes_used", _) -> mempty
  ("num_byte_usage_samples", _) -> mempty
  ("num_GCs", _) -> mempty
  ("peak_megabytes_allocated", _) -> mempty
  ("productivity_cpu_percent", _) -> mempty
  ("productivity_wall_percent", _) -> mempty
  ("sparks_count", _) -> mempty
  ("work_balance", _) -> mempty
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

summariesToTable :: [String] -> [Summary] -> Table
summariesToTable names ~(summary : summaries) =
  Table header rowGroup
  where
    header :: [String]
    header =
      (if length summaries > 1 then (++ ["Total"]) else id) do
        name <- take (length summaries + 1) (halfZipWith const names (map show [(1 :: Int) ..]))
        ["", name]
    rowGroup :: [RowGroup]
    rowGroup =
      [ RowGroup
          "Elapsed time"
          [ seconds "Total" (getAvg . total_wall_seconds) (>),
            seconds "Mutator" (getAvg . mut_wall_seconds) (>),
            percentage "Mutator %" mut_wall_percent (<),
            seconds "Garbage collector" (getAvg . gc_wall_seconds) (>),
            percentage "Garbage collector %" gc_wall_percent (>)
          ],
        -- seconds "Time (rts)" (\s -> getAvg (init_wall_seconds s) + getAvg (exit_wall_seconds s)) (>),
        RowGroup
          "CPU time"
          [ seconds "Total" (getAvg . total_cpu_seconds) (>),
            seconds "Mutator" (getAvg . mut_cpu_seconds) (>),
            percentage "Mutator %" mut_cpu_percent (<),
            seconds "Garbage collector" (getAvg . gc_cpu_seconds) (>),
            percentage "Garbage collector %" gc_cpu_percent (>)
          ],
        -- seconds "Time (rts cpu)" (\s -> getAvg (init_cpu_seconds s) + getAvg (exit_cpu_seconds s)) (>),
        RowGroup
          "Memory"
          [ bytes "Average residency" average_live_data (>),
            bytes "Max residency" (coerce max_live_bytes) (>),
            bytes "Allocated" (getAvg . allocated_bytes) (>),
            bytesPerSecond "Allocated per second" allocated_bytes_per_second (>),
            bytes "Copied during GC" (getAvg . copied_bytes) (>),
            bytes "Allocated from OS" (coerce max_mem_in_use_bytes) (>),
            bytes "Wasted by GHC" (coerce max_slop_bytes) (>),
            bytes "Fragmented" (getAvg . fragmentation_bytes) (>)
          ],
        RowGroup
          "GC generation 0"
          [ number "Collections" (getAvg . gen_0_collections) (>),
            number "Parallel collections" (getAvg . gen_0_par_collections) (>),
            seconds "Time" (getAvg . gen_0_wall_seconds) (>),
            seconds "CPU time" (getAvg . gen_0_cpu_seconds) (>),
            seconds "Average time" average_gen_0_time (>),
            seconds "Max time" (coerce gen_0_max_pause_seconds) (>)
          ],
        RowGroup
          "GC generation 1"
          [ number "Collections" (getAvg . gen_1_collections) (>),
            number "Parallel collections" (getAvg . gen_1_par_collections) (>),
            seconds "Time" (getAvg . gen_1_wall_seconds) (>),
            seconds "CPU time" (getAvg . gen_1_cpu_seconds) (>),
            seconds "Average time" average_gen_1_time (>),
            seconds "Max time" (coerce gen_1_max_pause_seconds) (>)
          ],
        RowGroup
          "Sparks"
          [ maybeNumber "Converted" (fmap getAvg . sparks_converted) (<),
            maybeNumber "Overflowed" (fmap getAvg . sparks_overflowed) (>),
            maybeNumber "Not sparked" (fmap getAvg . sparks_dud) (>),
            maybeNumber "Fizzled" (fmap getAvg . sparks_fizzled) (>),
            maybeNumber "Garbage collected" (fmap getAvg . sparks_fizzled) (>)
          ]
      ]
    metric :: (Rational -> String) -> Cell -> (Summary -> Rational) -> (Rational -> Rational -> Bool) -> Row
    metric render name f = maybeMetric render name (Just . f)
    maybeMetric :: (Rational -> String) -> Cell -> (Summary -> Maybe Rational) -> (Rational -> Rational -> Bool) -> Row
    maybeMetric render name f g =
      if all isEmptyCell cols
        then Empty
        else Row (name : cols)
      where
        cols :: [Cell]
        cols =
          white (maybe "" render (f summary)) : makeCols summary summaries
        makeCols :: Summary -> [Summary] -> [Cell]
        makeCols s0 = \case
          [] ->
            case summaries of
              [] -> []
              _ ->
                case (f summary, f (last summaries)) of
                  (Just v0, Just v1) -> [delta (g v0 v1) v0 v1]
                  _ -> []
          s1 : ss ->
            case (f s0, f s1) of
              (Just v0, Just v1) -> delta (g v0 v1) v0 v1 : white (render v1) : makeCols s1 ss
              _ -> "" : "" : makeCols s1 ss
    bytes = metric prettyBytes
    bytesPerSecond = metric prettyBytesPerSecond
    number = metric (show . round @_ @Int)
    maybeNumber = maybeMetric (show . round @_ @Int)
    percentage = metric prettyPercentage
    seconds = metric prettySeconds
    delta :: Bool -> Rational -> Rational -> Cell
    delta b v1 v2 =
      if abs pct <= cutoff
        then ""
        else (if b then green else red) (printf "%+.1f%%" (realToFrac pct :: Double))
      where
        cutoff :: Rational
        cutoff =
          0
        pct :: Rational
        pct =
          ((v2 - v1) * 100) `divide` v1
    halfZipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
    halfZipWith _ [] ys = ys
    halfZipWith _ xs [] = xs
    halfZipWith f (x : xs) (y : ys) = f x y : halfZipWith f xs ys

allocated_bytes_per_second :: Summary -> Rational
allocated_bytes_per_second s =
  getAvg (allocated_bytes s) `divide` getAvg (total_wall_seconds s)

average_gen_0_time :: Summary -> Rational
average_gen_0_time s =
  getAvg (gen_0_wall_seconds s) `divide` getAvg (gen_0_collections s)

average_gen_1_time :: Summary -> Rational
average_gen_1_time s =
  getAvg (gen_1_wall_seconds s) `divide` getAvg (gen_1_collections s)

average_live_data :: Summary -> Rational
average_live_data s =
  getAvg (cumulative_live_bytes s) `divide` getAvg (major_gcs s)

gc_cpu_percent :: Summary -> Rational
gc_cpu_percent s =
  getAvg (gc_cpu_seconds s) `divide` getAvg (total_cpu_seconds s)

gc_wall_percent :: Summary -> Rational
gc_wall_percent s =
  getAvg (gc_wall_seconds s) `divide` getAvg (total_wall_seconds s)

mut_cpu_percent :: Summary -> Rational
mut_cpu_percent s =
  getAvg (mut_cpu_seconds s) `divide` getAvg (total_cpu_seconds s)

mut_wall_percent :: Summary -> Rational
mut_wall_percent s =
  getAvg (mut_wall_seconds s) `divide` getAvg (total_wall_seconds s)

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

data Table
  = Table [String] [RowGroup]

data RowGroup
  = RowGroup String [Row]

data Row
  = Row [Cell]
  | Empty

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

isEmptyCell :: Cell -> Bool
isEmptyCell (Cell _ s) =
  null s

renderTable :: Table -> String
renderTable (Table labels rowGroups) =
  intercalate "\n" (header : mapMaybe renderRowGroup rowGroups ++ [footer])
  where
    header :: String
    header =
      let middle = intercalate "┬" (map (\(s, n) -> s ++ replicate (n + 2 - length s) '─') (zip labels widths))
       in '┌' : middle ++ "┐"
    line :: String -> String
    line label =
      let label' = "\ESC[1m\ESC[4m\ESC[97m" ++ label ++ "\ESC[39m\ESC[24m\ESC[22m"
          segment = label' ++ replicate (head widths + 2 - length label) '─'
          segments = concatMap (\n -> '┼' : replicate (n + 2) '─') (tail widths)
       in '├' : segment ++ segments ++ "┤"
    footer :: String
    footer =
      '└' : intercalate "┴" (map (\n -> replicate (n + 2) '─') widths) ++ "┘"
    renderRowGroup :: RowGroup -> Maybe String
    renderRowGroup (RowGroup label rows) =
      case mapMaybe renderRow rows of
        [] -> Nothing
        s -> Just (intercalate "\n" (line label : s))
    renderRow :: Row -> Maybe String
    renderRow = \case
      Row row -> Just ("│ " ++ intercalate " │ " (map renderCell (zip widths row)) ++ " │")
      Empty -> Nothing
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
        ( \acc (RowGroup label rows) ->
            foldl'
              ( \acc2 -> \case
                  Row [] -> error "empty row"
                  Row (Cell _ s0 : cols) ->
                    zipWith max (max (length label) (length s0) : map (\(Cell _ s) -> length s) cols) acc2
                  Empty -> acc2
              )
              acc
              rows
        )
        (map (subtract 1 . length) labels)
        rowGroups

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
