module MusicTheory.VoicingClass exposing
    ( IntervalRange
    , builder
    , execute
    , withFactor
    , withFactorFrom
    , withThreeFactorsFrom
    , withTwoFactorsFrom
    )

import List.Extra
import MusicTheory.Interval as Interval


type VoicingClassBuilder a
    = VoicingClassBuilder (List ( a, List Interval.Interval ))


builder : (a -> b) -> VoicingClassBuilder (a -> b)
builder construct =
    VoicingClassBuilder [ ( construct, [] ) ]


withFactor :
    Interval.Interval
    -> { mustBeUnique : Bool }
    -> VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClassBuilder a
withFactor newInterval { mustBeUnique } (VoicingClassBuilder voicingsSoFar) =
    let
        addIntervalToVoicing :
            Interval.Interval
            -> ( Interval.Interval -> a, List Interval.Interval )
            -> Maybe ( a, List Interval.Interval )
        addIntervalToVoicing intervalToApply ( theVoicing, intervalsUsed ) =
            if mustBeUnique && List.member intervalToApply intervalsUsed then
                Nothing

            else
                Just
                    ( theVoicing intervalToApply
                    , intervalToApply :: intervalsUsed
                    )
    in
    VoicingClassBuilder
        (List.filterMap
            (\voicingWithIntervals ->
                addIntervalToVoicing newInterval voicingWithIntervals
            )
            voicingsSoFar
        )


withFactorFrom :
    List Interval.Interval
    -> { mustBeUnique : Bool }
    -> VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClassBuilder a
withFactorFrom options { mustBeUnique } (VoicingClassBuilder voicingsSoFar) =
    let
        addIntervalToVoicing :
            Interval.Interval
            -> ( Interval.Interval -> a, List Interval.Interval )
            -> Maybe ( a, List Interval.Interval )
        addIntervalToVoicing intervalToApply ( theVoicing, intervalsUsed ) =
            if mustBeUnique && List.member intervalToApply intervalsUsed then
                Nothing

            else
                Just
                    ( theVoicing intervalToApply
                    , intervalToApply :: intervalsUsed
                    )
    in
    VoicingClassBuilder
        (List.concatMap
            (\voicingWithIntervals ->
                List.filterMap
                    (\interval ->
                        addIntervalToVoicing interval voicingWithIntervals
                    )
                    options
            )
            voicingsSoFar
        )


withTwoFactorsFrom :
    List Interval.Interval
    -> { mustBeUnique : Bool }
    -> VoicingClassBuilder (Interval.Interval -> Interval.Interval -> a)
    -> VoicingClassBuilder a
withTwoFactorsFrom options { mustBeUnique } (VoicingClassBuilder voicingsSoFar) =
    let
        addIntervalsToVoicing :
            ( Interval.Interval, Interval.Interval )
            ->
                ( Interval.Interval
                  -> Interval.Interval
                  -> a
                , List Interval.Interval
                )
            -> Maybe ( a, List Interval.Interval )
        addIntervalsToVoicing ( interval1, interval2 ) ( theVoicing, intervalsUsed ) =
            if
                mustBeUnique
                    && List.any
                        (\i -> List.member i intervalsUsed)
                        [ interval1, interval2 ]
            then
                Nothing

            else
                Just
                    ( theVoicing interval1 interval2
                    , [ interval1, interval2 ] ++ intervalsUsed
                    )

        pairs :
            List Interval.Interval
            -> List ( Interval.Interval, Interval.Interval )
        pairs theList =
            List.Extra.permutations theList
                |> List.map
                    (\permutation ->
                        case permutation of
                            a :: b :: _ ->
                                Just ( a, b )

                            _ ->
                                Nothing
                    )
                |> List.filterMap identity
    in
    VoicingClassBuilder
        (List.concatMap
            (\voicingWithIntervals ->
                List.filterMap
                    (\twoIntervals ->
                        addIntervalsToVoicing twoIntervals voicingWithIntervals
                    )
                    (pairs options)
            )
            voicingsSoFar
        )


withThreeFactorsFrom :
    List Interval.Interval
    -> { mustBeUnique : Bool }
    ->
        VoicingClassBuilder
            (Interval.Interval
             -> Interval.Interval
             -> Interval.Interval
             -> a
            )
    -> VoicingClassBuilder a
withThreeFactorsFrom options { mustBeUnique } (VoicingClassBuilder voicingsSoFar) =
    let
        addIntervalsToVoicing :
            ( Interval.Interval, Interval.Interval, Interval.Interval )
            ->
                ( Interval.Interval
                  -> Interval.Interval
                  -> Interval.Interval
                  -> a
                , List Interval.Interval
                )
            -> Maybe ( a, List Interval.Interval )
        addIntervalsToVoicing ( interval1, interval2, interval3 ) ( theVoicing, intervalsUsed ) =
            if
                mustBeUnique
                    && List.any
                        (\i -> List.member i intervalsUsed)
                        [ interval1, interval2, interval3 ]
            then
                Nothing

            else
                Just
                    ( theVoicing interval1 interval2 interval3
                    , [ interval1, interval2, interval3 ] ++ intervalsUsed
                    )

        triplets :
            List Interval.Interval
            -> List ( Interval.Interval, Interval.Interval, Interval.Interval )
        triplets theList =
            List.Extra.permutations theList
                |> List.map
                    (\permutation ->
                        case permutation of
                            a :: b :: c :: _ ->
                                Just ( a, b, c )

                            _ ->
                                Nothing
                    )
                |> List.filterMap identity
    in
    VoicingClassBuilder
        (List.concatMap
            (\voicingWithIntervals ->
                List.filterMap
                    (\twoIntervals ->
                        addIntervalsToVoicing twoIntervals voicingWithIntervals
                    )
                    (triplets options)
            )
            voicingsSoFar
        )


execute : { placeFactors : a -> List a } -> VoicingClassBuilder a -> List a
execute { placeFactors } (VoicingClassBuilder voicingsWithIntervals) =
    List.map (\( v, i ) -> v) voicingsWithIntervals
        |> List.concatMap placeFactors


type alias IntervalRange =
    { max : Interval.Interval
    , min : Interval.Interval
    }
