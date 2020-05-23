module Util.Basic exposing (applyNTimes, while)


applyNTimes : Int -> (a -> a) -> a -> a
applyNTimes n fn v =
    applyNTimesHelp 0 n fn v


applyNTimesHelp : Int -> Int -> (a -> a) -> a -> a
applyNTimesHelp i n fn v =
    if i < n then
        applyNTimesHelp (i + 1) n fn (fn v)

    else
        v


while : (a -> Bool) -> (a -> a) -> a -> a
while predicate fn val =
    if predicate val then
        while predicate fn (fn val)

    else
        val
