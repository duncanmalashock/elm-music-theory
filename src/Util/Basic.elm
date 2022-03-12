module Util.Basic exposing (applyNTimes)


applyNTimes : Int -> (a -> a) -> a -> a
applyNTimes n fn v =
    applyNTimesHelp 0 n fn v


applyNTimesHelp : Int -> Int -> (a -> a) -> a -> a
applyNTimesHelp i n fn v =
    if i < n then
        applyNTimesHelp (i + 1) n fn (fn v)

    else
        v
