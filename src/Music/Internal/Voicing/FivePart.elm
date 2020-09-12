module Music.Internal.Voicing.FivePart exposing
    ( Pitches
    , Ranges
    , SpacingLimits
    , VoiceIntervalLimits
    , Voicing
    , VoicingClass
    , VoicingMethod
    , adjustToBeAboveMinimum
    , allFactors
    , allIntervals
    , allRanges
    , allVoices
    , combineVoicingMethods
    , containsParallelFifths
    , custom
    , getVoiceFive
    , getVoiceFour
    , getVoiceOne
    , getVoiceThree
    , getVoiceTwo
    , orderWeighted
    , placeFactors
    , placeSelectedFactors
    , selectFactors
    , sortWeighted
    , toPitches
    , violatesLowIntervalLimits
    , voicing
    , voicingClassesFromMethod
    , withFactor
    , withFactorFrom
    , withThreeFactorsFrom
    , withTwoFactorsFrom
    , withUniqueFactor
    , withUniqueFactorFrom
    , withUniqueThreeFactorsFrom
    , withUniqueTwoFactorsFrom
    )

import Music.Internal.Chord as Chord
import Music.Internal.ChordType as ChordType
import Music.Internal.Interval as Interval
import Music.Internal.Octave exposing (Octave)
import Music.Internal.Pitch as Pitch
import Music.Internal.PitchClass as PitchClass
import Music.Internal.Voicing as Voicing
import Music.Internal.VoicingClass as VoicingClass
import Util.Basic


type alias Voicing =
    Voicing.Voicing VoicingClass


type alias Pitches =
    { voiceOne : Pitch.Pitch
    , voiceTwo : Pitch.Pitch
    , voiceThree : Pitch.Pitch
    , voiceFour : Pitch.Pitch
    , voiceFive : Pitch.Pitch
    }


type VoicingMethod
    = VoicingMethod (ChordType.ChordType -> List VoicingClass)


voicing : Pitches -> Chord.Chord -> Result (List PitchClass.PitchClass) Voicing
voicing pitches theChord =
    let
        pitchClasses : List PitchClass.PitchClass
        pitchClasses =
            [ pitches.voiceOne
            , pitches.voiceTwo
            , pitches.voiceThree
            , pitches.voiceFour
            , pitches.voiceFive
            ]
                |> List.map Pitch.pitchClass

        rootOctave : Octave
        rootOctave =
            Pitch.octave pitches.voiceFour

        newRoot : Pitch.Pitch
        newRoot =
            Chord.root theChord
                |> Pitch.fromPitchClass rootOctave

        newVoicingClass =
            { voiceOne = Pitch.intervalBetween newRoot pitches.voiceOne
            , voiceTwo = Pitch.intervalBetween newRoot pitches.voiceTwo
            , voiceThree = Pitch.intervalBetween newRoot pitches.voiceThree
            , voiceFour = Pitch.intervalBetween newRoot pitches.voiceFour
            , voiceFive = Pitch.intervalBetween newRoot pitches.voiceFive
            }

        pitchClassesNotInChord =
            List.filter (\pc -> Chord.containsPitchClass pc theChord |> not) pitchClasses
    in
    if List.isEmpty pitchClassesNotInChord then
        Ok (Voicing.voicing theChord rootOctave newVoicingClass)

    else
        Err pitchClassesNotInChord


orderWeighted :
    List ( Voicing -> Voicing -> Order, Float )
    -> (Voicing -> Voicing -> Order)
orderWeighted weightedSortFns =
    let
        orderToNumber : Order -> Float
        orderToNumber ord =
            case ord of
                LT ->
                    -1

                EQ ->
                    0

                GT ->
                    1

        score : Voicing -> Voicing -> Float
        score a b =
            weightedSortFns
                |> List.map
                    (\( comp, weight ) ->
                        orderToNumber (comp a b) * weight
                    )
                |> List.sum

        sortFn : Voicing -> Voicing -> Order
        sortFn a b =
            compare (score a b) 0
    in
    sortFn


sortWeighted :
    List ( Voicing -> Voicing -> Order, Float )
    -> List Voicing
    -> List Voicing
sortWeighted weightedSortFns voicings =
    List.sortWith (orderWeighted weightedSortFns) voicings


containsParallelFifths : Voicing -> Voicing -> Bool
containsParallelFifths a b =
    Voicing.containsParallelFifths Voicing.root allFactors a b


voicingClassesFromMethod : ChordType.ChordType -> VoicingMethod -> List VoicingClass
voicingClassesFromMethod chordType (VoicingMethod fn) =
    fn chordType


combineVoicingMethods : List VoicingMethod -> VoicingMethod
combineVoicingMethods voicingMethodsToCombine =
    (\chordType ->
        List.concatMap
            (\(VoicingMethod fn) ->
                fn chordType
            )
            voicingMethodsToCombine
    )
        |> VoicingMethod


toPitches : Voicing -> Pitches
toPitches theVoicing =
    let
        theRoot =
            Voicing.root theVoicing

        vc =
            Voicing.voicingClass theVoicing
    in
    { voiceOne = Pitch.transposeUp vc.voiceOne theRoot
    , voiceTwo = Pitch.transposeUp vc.voiceTwo theRoot
    , voiceThree = Pitch.transposeUp vc.voiceThree theRoot
    , voiceFour = Pitch.transposeUp vc.voiceFour theRoot
    , voiceFive = Pitch.transposeUp vc.voiceFive theRoot
    }


allVoices : List (Voicing -> Pitch.Pitch)
allVoices =
    [ getVoiceOne
    , getVoiceTwo
    , getVoiceThree
    , getVoiceFour
    , getVoiceFive
    ]


getVoiceOne : Voicing -> Pitch.Pitch
getVoiceOne =
    toPitches >> .voiceOne


getVoiceTwo : Voicing -> Pitch.Pitch
getVoiceTwo =
    toPitches >> .voiceTwo


getVoiceThree : Voicing -> Pitch.Pitch
getVoiceThree =
    toPitches >> .voiceThree


getVoiceFour : Voicing -> Pitch.Pitch
getVoiceFour =
    toPitches >> .voiceFour


getVoiceFive : Voicing -> Pitch.Pitch
getVoiceFive =
    toPitches >> .voiceFive


type alias VoicingClass =
    { voiceOne : Interval.Interval
    , voiceTwo : Interval.Interval
    , voiceThree : Interval.Interval
    , voiceFour : Interval.Interval
    , voiceFive : Interval.Interval
    }


allFactors : List (Voicing -> Interval.Interval)
allFactors =
    [ getFactorInVoiceOne
    , getFactorInVoiceTwo
    , getFactorInVoiceThree
    , getFactorInVoiceFour
    ]


getFactorInVoiceOne : Voicing -> Interval.Interval
getFactorInVoiceOne =
    Voicing.voicingClass >> .voiceOne


getFactorInVoiceTwo : Voicing -> Interval.Interval
getFactorInVoiceTwo =
    Voicing.voicingClass >> .voiceTwo


getFactorInVoiceThree : Voicing -> Interval.Interval
getFactorInVoiceThree =
    Voicing.voicingClass >> .voiceThree


getFactorInVoiceFour : Voicing -> Interval.Interval
getFactorInVoiceFour =
    Voicing.voicingClass >> .voiceFour


type alias InstrumentRanges =
    { voiceOne : Pitch.Range
    , voiceTwo : Pitch.Range
    , voiceThree : Pitch.Range
    , voiceFour : Pitch.Range
    }


type alias SpacingLimits =
    { twoToOne : Interval.Range
    , threeToTwo : Interval.Range
    , fourToThree : Interval.Range
    , fiveToFour : Interval.Range
    }


custom :
    (ChordType.ChordType -> Maybe categorized)
    -> (categorized -> List VoicingClass)
    -> VoicingMethod
custom categorizeFn buildFromCategorized =
    VoicingMethod
        (categorizeFn
            >> Maybe.map buildFromCategorized
            >> Maybe.withDefault []
        )


withFactor :
    Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withFactor factor builder =
    VoicingClass.withFactor factor { mustBeUnique = False } builder


withUniqueFactor :
    Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withUniqueFactor factor builder =
    VoicingClass.withFactor factor { mustBeUnique = True } builder


withFactorFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withFactorFrom options builder =
    VoicingClass.withFactorFrom options { mustBeUnique = False } builder


withUniqueFactorFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withUniqueFactorFrom options builder =
    VoicingClass.withFactorFrom options { mustBeUnique = True } builder


withTwoFactorsFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withTwoFactorsFrom options builder =
    VoicingClass.withTwoFactorsFrom options { mustBeUnique = False } builder


withUniqueTwoFactorsFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withUniqueTwoFactorsFrom options builder =
    VoicingClass.withTwoFactorsFrom options { mustBeUnique = True } builder


withThreeFactorsFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> Interval.Interval -> Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withThreeFactorsFrom options builder =
    VoicingClass.withThreeFactorsFrom options { mustBeUnique = False } builder


withUniqueThreeFactorsFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> Interval.Interval -> Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withUniqueThreeFactorsFrom options builder =
    VoicingClass.withThreeFactorsFrom options { mustBeUnique = True } builder


selectFactors :
    VoicingClass.VoicingClassBuilder
        (Interval.Interval
         -> Interval.Interval
         -> Interval.Interval
         -> Interval.Interval
         -> Interval.Interval
         -> VoicingClass
        )
selectFactors =
    VoicingClass.builder VoicingClass


noSelection : List VoicingClass
noSelection =
    []


placeSelectedFactors :
    SpacingLimits
    -> VoicingClass.VoicingClassBuilder VoicingClass
    -> List VoicingClass
placeSelectedFactors voiceIntervalLimits builder =
    VoicingClass.execute
        { placeFactors =
            placeFactors voiceIntervalLimits
        }
        builder



-- Ranges


type alias Ranges =
    { voiceOne : Pitch.Range
    , voiceTwo : Pitch.Range
    , voiceThree : Pitch.Range
    , voiceFour : Pitch.Range
    , voiceFive : Pitch.Range
    }


allRanges : List (Ranges -> Pitch.Range)
allRanges =
    [ .voiceOne
    , .voiceTwo
    , .voiceThree
    , .voiceFour
    , .voiceFive
    ]


allIntervals : VoicingClass -> IntervalList
allIntervals vc =
    { fiveToOne =
        Interval.subtract vc.voiceFive vc.voiceOne
    , fiveToTwo =
        Interval.subtract vc.voiceFive vc.voiceTwo
    , fourToOne =
        Interval.subtract vc.voiceFour vc.voiceOne
    , fiveToThree =
        Interval.subtract vc.voiceFive vc.voiceThree
    , fourToTwo =
        Interval.subtract vc.voiceFour vc.voiceTwo
    , threeToOne =
        Interval.subtract vc.voiceThree vc.voiceOne
    , fiveToFour =
        Interval.subtract vc.voiceFive vc.voiceFour
    , fourToThree =
        Interval.subtract vc.voiceFour vc.voiceThree
    , threeToTwo =
        Interval.subtract vc.voiceThree vc.voiceTwo
    , twoToOne =
        Interval.subtract vc.voiceTwo vc.voiceOne
    }


type alias IntervalList =
    { fiveToOne : Interval.Interval
    , fiveToTwo : Interval.Interval
    , fourToOne : Interval.Interval
    , fiveToThree : Interval.Interval
    , fourToTwo : Interval.Interval
    , threeToOne : Interval.Interval
    , fiveToFour : Interval.Interval
    , fourToThree : Interval.Interval
    , threeToTwo : Interval.Interval
    , twoToOne : Interval.Interval
    }


violatesLowIntervalLimits : Voicing -> Bool
violatesLowIntervalLimits theVoicing =
    allIntervals (Voicing.voicingClass theVoicing)
        |> (\{ fiveToOne, fiveToTwo, fiveToThree, fiveToFour, fourToOne, fourToTwo, fourToThree, threeToOne, threeToTwo, twoToOne } ->
                [ ( getVoiceFive theVoicing, fiveToOne )
                , ( getVoiceFive theVoicing, fiveToTwo )
                , ( getVoiceFive theVoicing, fiveToThree )
                , ( getVoiceFive theVoicing, fiveToFour )
                , ( getVoiceFour theVoicing, fourToOne )
                , ( getVoiceFour theVoicing, fourToTwo )
                , ( getVoiceFour theVoicing, fourToThree )
                , ( getVoiceThree theVoicing, threeToOne )
                , ( getVoiceThree theVoicing, threeToTwo )
                , ( getVoiceTwo theVoicing, twoToOne )
                ]
           )
        |> List.map
            (\( voice, interval ) ->
                Voicing.violatesLowIntervalLimits interval voice
            )
        |> List.any identity



-- Interval limits


type alias VoiceIntervalLimits =
    { twoToOne : VoicingClass.IntervalRange
    , threeToTwo : VoicingClass.IntervalRange
    , fourToThree : VoicingClass.IntervalRange
    , fiveToFour : VoicingClass.IntervalRange
    }



-- Generating voicing classes


placeFactors : SpacingLimits -> VoicingClass -> List VoicingClass
placeFactors intervalLimits voicingClass =
    let
        voiceFiveInitial =
            Interval.toSimple voicingClass.voiceFive

        voiceFourInitial =
            adjustToBeAboveMinimum
                { minimum = Interval.min intervalLimits.fiveToFour
                , lower = voiceFiveInitial
                , upper = Interval.toSimple voicingClass.voiceFour
                }

        voiceThreeInitial =
            adjustToBeAboveMinimum
                { minimum = Interval.min intervalLimits.fourToThree
                , lower = voiceFourInitial
                , upper = Interval.toSimple voicingClass.voiceThree
                }

        voiceTwoInitial =
            adjustToBeAboveMinimum
                { minimum = Interval.min intervalLimits.threeToTwo
                , lower = voiceThreeInitial
                , upper = Interval.toSimple voicingClass.voiceTwo
                }

        voiceOneInitial =
            adjustToBeAboveMinimum
                { minimum = Interval.min intervalLimits.twoToOne
                , lower = voiceTwoInitial
                , upper = Interval.toSimple voicingClass.voiceOne
                }

        fiveToFourOctaveShifts =
            octaveShiftsAllowedBetween
                { maximum = Interval.max intervalLimits.fiveToFour
                , lower = voiceFourInitial
                , upper = voiceThreeInitial
                }

        fourToThreeOctaveShifts =
            octaveShiftsAllowedBetween
                { maximum = Interval.max intervalLimits.fourToThree
                , lower = voiceFourInitial
                , upper = voiceThreeInitial
                }

        threeToTwoOctaveShifts =
            octaveShiftsAllowedBetween
                { maximum = Interval.max intervalLimits.threeToTwo
                , lower = voiceThreeInitial
                , upper = voiceTwoInitial
                }

        twoToOneOctaveShifts =
            octaveShiftsAllowedBetween
                { maximum = Interval.max intervalLimits.twoToOne
                , lower = voiceTwoInitial
                , upper = voiceOneInitial
                }
    in
    { voiceOne = voiceOneInitial
    , voiceTwo = voiceTwoInitial
    , voiceThree = voiceThreeInitial
    , voiceFour = voiceFourInitial
    , voiceFive = voiceFiveInitial
    }
        |> validateInitialVoicingClass intervalLimits
        |> Maybe.map
            (performAllowedOctaveShifts
                { shiftsForVoiceFour = fiveToFourOctaveShifts
                , shiftsForVoiceThree = fourToThreeOctaveShifts
                , shiftsForVoiceTwo = threeToTwoOctaveShifts
                , shiftsForVoiceOne = twoToOneOctaveShifts
                }
            )
        |> Maybe.withDefault []


validateInitialVoicingClass : SpacingLimits -> VoicingClass -> Maybe VoicingClass
validateInitialVoicingClass intervalLimits voicingClass =
    let
        differenceMeetsMinimum l u minimum =
            Interval.semitones (Interval.subtract l u) >= Interval.semitones minimum

        differenceMeetsMaximum l u maximum =
            Interval.semitones (Interval.subtract l u) <= Interval.semitones maximum
    in
    if
        List.all identity
            [ differenceMeetsMinimum voicingClass.voiceFive voicingClass.voiceFour (Interval.min intervalLimits.fiveToFour)
            , differenceMeetsMaximum voicingClass.voiceFive voicingClass.voiceFour (Interval.max intervalLimits.fiveToFour)
            , differenceMeetsMinimum voicingClass.voiceFour voicingClass.voiceThree (Interval.min intervalLimits.fourToThree)
            , differenceMeetsMaximum voicingClass.voiceFour voicingClass.voiceThree (Interval.max intervalLimits.fourToThree)
            , differenceMeetsMinimum voicingClass.voiceThree voicingClass.voiceTwo (Interval.min intervalLimits.threeToTwo)
            , differenceMeetsMaximum voicingClass.voiceThree voicingClass.voiceTwo (Interval.max intervalLimits.threeToTwo)
            , differenceMeetsMinimum voicingClass.voiceTwo voicingClass.voiceOne (Interval.min intervalLimits.twoToOne)
            , differenceMeetsMaximum voicingClass.voiceTwo voicingClass.voiceOne (Interval.max intervalLimits.twoToOne)
            ]
    then
        Just voicingClass

    else
        Nothing


adjustToBeAboveMinimum :
    { minimum : Interval.Interval
    , lower : Interval.Interval
    , upper : Interval.Interval
    }
    -> Interval.Interval
adjustToBeAboveMinimum { minimum, lower, upper } =
    -- TODO: duplicated with FourPart. extract into another module
    let
        lowerIsAboveUpper l u =
            Interval.semitones (Interval.subtract l u) < 0

        differenceIsBelowMinimum l u =
            Interval.semitones (Interval.subtract l u) < Interval.semitones minimum
    in
    upper
        |> Util.Basic.while
            (\currentUpper ->
                lowerIsAboveUpper lower currentUpper
                    || differenceIsBelowMinimum lower currentUpper
            )
            Interval.addOctave


performAllowedOctaveShifts :
    { shiftsForVoiceFour : Int
    , shiftsForVoiceThree : Int
    , shiftsForVoiceTwo : Int
    , shiftsForVoiceOne : Int
    }
    -> VoicingClass
    -> List VoicingClass
performAllowedOctaveShifts { shiftsForVoiceFour, shiftsForVoiceThree, shiftsForVoiceTwo, shiftsForVoiceOne } initialVoicingClass =
    [ initialVoicingClass ]
        |> shiftFromVoiceFour shiftsForVoiceFour
        |> shiftFromVoiceThree shiftsForVoiceThree
        |> shiftFromVoiceTwo shiftsForVoiceTwo
        |> shiftFromVoiceOne shiftsForVoiceOne


shiftFromVoiceFour : Int -> List VoicingClass -> List VoicingClass
shiftFromVoiceFour shiftCount voicingClasses =
    let
        doShifts : List Interval.Interval -> VoicingClass -> List VoicingClass
        doShifts intervalsToShift vc =
            List.map
                (\shift ->
                    { vc
                        | voiceOne = vc.voiceOne |> Interval.add shift
                        , voiceTwo = vc.voiceTwo |> Interval.add shift
                        , voiceThree = vc.voiceThree |> Interval.add shift
                        , voiceFour = vc.voiceFour |> Interval.add shift
                    }
                )
                intervalsToShift
    in
    voicingClasses
        |> List.concatMap
            (doShifts (shiftCountToIntervalList shiftCount))


shiftFromVoiceThree : Int -> List VoicingClass -> List VoicingClass
shiftFromVoiceThree shiftCount voicingClasses =
    let
        doShifts : List Interval.Interval -> VoicingClass -> List VoicingClass
        doShifts intervalsToShift vc =
            List.map
                (\shift ->
                    { vc
                        | voiceOne = vc.voiceOne |> Interval.add shift
                        , voiceTwo = vc.voiceTwo |> Interval.add shift
                        , voiceThree = vc.voiceThree |> Interval.add shift
                    }
                )
                intervalsToShift
    in
    voicingClasses
        |> List.concatMap
            (doShifts (shiftCountToIntervalList shiftCount))


shiftFromVoiceTwo : Int -> List VoicingClass -> List VoicingClass
shiftFromVoiceTwo shiftCount voicingClasses =
    let
        doShifts : List Interval.Interval -> VoicingClass -> List VoicingClass
        doShifts intervalsToShift vc =
            List.map
                (\shift ->
                    { vc
                        | voiceOne = vc.voiceOne |> Interval.add shift
                        , voiceTwo = vc.voiceTwo |> Interval.add shift
                    }
                )
                intervalsToShift
    in
    voicingClasses
        |> List.concatMap
            (doShifts (shiftCountToIntervalList shiftCount))


shiftFromVoiceOne : Int -> List VoicingClass -> List VoicingClass
shiftFromVoiceOne shiftCount voicingClasses =
    let
        doShifts : List Interval.Interval -> VoicingClass -> List VoicingClass
        doShifts intervalsToShift vc =
            List.map
                (\shift ->
                    { vc
                        | voiceOne = vc.voiceOne |> Interval.add shift
                    }
                )
                intervalsToShift
    in
    voicingClasses
        |> List.concatMap
            (doShifts (shiftCountToIntervalList shiftCount))


shiftCountToIntervalList : Int -> List Interval.Interval
shiftCountToIntervalList count =
    List.map
        (\currentCount ->
            Util.Basic.applyNTimes currentCount Interval.addOctave Interval.perfectUnison
        )
        (List.range 0 count)


octaveShiftsAllowedBetween :
    { maximum : Interval.Interval
    , lower : Interval.Interval
    , upper : Interval.Interval
    }
    -> Int
octaveShiftsAllowedBetween { maximum, lower, upper } =
    let
        difference l u =
            Interval.semitones (Interval.subtract l u)

        differenceIsBelowMaximum l u =
            difference l u < Interval.semitones maximum

        maximumIsAtLeastCurrentDifferencePlusAnOctave l u =
            Interval.semitones maximum >= (Interval.semitones Interval.perfectOctave + difference l u)
    in
    ( 0, upper )
        |> Util.Basic.while
            (\( count, currentUpper ) ->
                differenceIsBelowMaximum lower currentUpper
                    && maximumIsAtLeastCurrentDifferencePlusAnOctave lower currentUpper
            )
            (\( count, shouldBeAbove ) ->
                ( count + 1, Interval.addOctave shouldBeAbove )
            )
        |> (\( count, shouldBeAbove ) -> count)
