{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( mainloop
    ) where

import           Text.Read (readMaybe)

-- | Int or Float 型のNumberという型を定義する。
data Number = IntVal Int | FloatVal Float deriving (Eq, Ord)

-- | Number型の値を受け取り、その値を文字列に変換する関数を定義する。
instance Read Number where
    readsPrec _ s =
        case readMaybe s :: Maybe Int of
            Just n  -> [(IntVal n, "")]
            Nothing ->
                case readMaybe s :: Maybe Float of
                    Just n  -> [(FloatVal n, "")]
                    Nothing -> []


-- | Number型の値を受け取り、その値を文字列に変換する関数を定義する。
instance Show Number where
    show (IntVal n)   = show n
    show (FloatVal n) = show n

-- | Convert a temperature from Fahrenheit to Celsius
calcFtoC :: Number -> Number
calcFtoC (IntVal f)   = FloatVal ((fromIntegral f - 32) * 5 / 9)
calcFtoC (FloatVal f) = FloatVal ((f - 32) * 5 / 9)

-- | Convert a temperature from Celsius to Fahrenheit
calcCtoF :: Number -> Number
calcCtoF (IntVal c)   = FloatVal ((fromIntegral c * 9 / 5) + 32)
calcCtoF (FloatVal c) = FloatVal ((c * 9 / 5) + 32)

mainloop :: IO ()
mainloop = do
    putStrLn "Enter 0 to exit, 1 to convert F to C, 2 to convert C to F"
    input <- getLine
    case input of
        "0" -> return ()
        "1" -> do
            putStrLn "Enter a temperature in Fahrenheit"
            f <- getLine
            putStrLn $ "That's " ++ show (calcFtoC (read f :: Number)) ++ " in Celsius"
            mainloop
        "2" -> do
            putStrLn "Enter a temperature in Celsius"
            c <- getLine
            putStrLn $ "That's " ++ show (calcCtoF (read c :: Number)) ++ " in Fahrenheit"
            mainloop
        _ -> do
            putStrLn "Invalid input"
            mainloop
