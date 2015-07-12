module WirelessIMU.ReadSensors (
    Sensor(..),
    Result(..),
    State(..),
    calibrate,
    procMsg,
    process,
    getMessages,
    getMeasurements
    ) where

import Control.Monad
import Data.List
import Data.Maybe
import Network.Socket

import Data.List.Split
import Pipes
import qualified Pipes.Prelude as P
import Linear

import WirelessIMU.UDPSource

data Sensor = Accel | Gyro | Mag deriving (Show, Eq)

readSensor 3 = Accel
readSensor 4 = Gyro
readSensor 5 = Mag

data State = State {
    accel :: V3 Double,
    gyro  :: V3 Double,
    mag   :: V3 Double
} deriving (Show)

vecZero = V3 0 0 0

data Result = Result {
    tis :: Double,
    me  :: [(Sensor, V3 Double)]
} deriving (Show)

process :: String -> Result
process msg = Result (read ts) (mes rst)
    where
    ts : rst = splitOn "," msg
    mes (id : x : y : z : rst) = (readSensor (read id), V3 (read x) (read y) (read z)) : mes rst
    mes _                      = []

updateState :: V3 Double -> [(Sensor, V3 Double)] -> State -> State
updateState bias ((Accel, v) : rst) st = updateState bias rst $ st {accel = v}
updateState bias ((Gyro,  v) : rst) st = updateState bias rst $ st {gyro  = v - bias}
updateState bias ((Mag,   v) : rst) st = updateState bias rst $ st {mag   = v}
updateState bias []                 st = st

calibrate what n = calibrate' n []
    where
    calibrate' n accum = do
        res <- await 
        let gVal = find ((==what) . fst) (me res)
        case gVal of 
            Nothing -> calibrate' n accum
            Just x  -> case n==0 of
                True  -> return $ sum accum / fromIntegral (length accum)
                False -> calibrate' (n-1) (snd x : accum)

procMsg :: (Monad m) => Pipe Result (Double, State) m ()
procMsg = do
    bias <- calibrate Gyro 100 
    (ts, x, y, z) <- getInit 
    func bias (ts, State x y z)
    where 
    func bias (ts, st) = do
        res <- await
        let st' = updateState bias (me res) st
        yield (tis res - ts, st')
        func bias (tis res, st')
    getInit = do
        res <- await
        case length (me res) of
            3 -> return (tis res, f Accel res, f Gyro res, f Mag res)
            _ -> getInit
        where f a b = snd $ fromJust $ find ((==a) . fst) (me b)

getMessages :: PortNumber -> Producer Result IO ()
getMessages port = prodUDP port 1000 >-> P.map process

getMeasurements :: PortNumber -> Producer (Double, State) IO ()
getMeasurements port = getMessages port >-> procMsg

