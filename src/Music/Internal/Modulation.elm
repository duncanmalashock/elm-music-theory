module Music.Internal.Modulation exposing
    ( Modulation
    , allowTheoreticalKeys
    , apply
    , applyMultiple
    , byInterval
    , downByFifths
    , normalizeToCircleOfFifths
    , simplifyAndPreferFSharp
    , simplifyAndPreferGFlat
    , switchToParallelKey
    , switchToRelativeKey
    , upByFifths
    )

import Music.Internal.Interval as Interval
import Music.Internal.Key as Key
import Music.Internal.PitchClass as PitchClass
import Util.Basic as Util


type Modulation
    = Modulation ModulationDetails


type alias ModulationDetails =
    { interval : Interval.Interval
    , modeChange : Maybe ModeChange
    }


type ModeChange
    = ChangeToRelativeKey
    | ChangeToParallelKey


type NormalizeSettings
    = DontNormalize
    | NormalizeToCircleOfFifths
    | Simplify PreferGFlatOrFSharp


allowTheoreticalKeys : NormalizeSettings
allowTheoreticalKeys =
    DontNormalize


normalizeToCircleOfFifths : NormalizeSettings
normalizeToCircleOfFifths =
    NormalizeToCircleOfFifths


simplifyAndPreferGFlat : NormalizeSettings
simplifyAndPreferGFlat =
    Simplify PreferGFlat


simplifyAndPreferFSharp : NormalizeSettings
simplifyAndPreferFSharp =
    Simplify PreferFSharp


type PreferGFlatOrFSharp
    = PreferGFlat
    | PreferFSharp


upByFifths : Int -> Modulation
upByFifths numFifths =
    Modulation
        { interval =
            Util.applyNTimes
                numFifths
                (Interval.add Interval.perfectFifth)
                Interval.perfectUnison
        , modeChange = Nothing
        }


downByFifths : Int -> Modulation
downByFifths numFifths =
    Modulation
        { interval =
            Util.applyNTimes
                numFifths
                (Interval.subtract Interval.perfectFifth)
                Interval.perfectUnison
        , modeChange = Nothing
        }


byInterval : Interval.Interval -> Modulation
byInterval interval =
    Modulation
        { interval = interval
        , modeChange = Nothing
        }


switchToParallelKey : Modulation
switchToParallelKey =
    Modulation
        { interval = Interval.perfectUnison
        , modeChange = Just ChangeToParallelKey
        }


switchToRelativeKey : Modulation
switchToRelativeKey =
    Modulation
        { interval = Interval.perfectUnison
        , modeChange = Just ChangeToRelativeKey
        }


apply : NormalizeSettings -> Modulation -> Key.Key -> Key.Key
apply normalizeSettings (Modulation details) key =
    key
        |> applyRootTransposition details
        |> applyModeChange details
        |> normalize normalizeSettings


applyMultiple : NormalizeSettings -> List Modulation -> Key.Key -> Key.Key
applyMultiple normalizeSettings modulations key =
    List.foldl (apply normalizeSettings) key modulations


normalize : NormalizeSettings -> Key.Key -> Key.Key
normalize normalizeSettings key =
    case normalizeSettings of
        DontNormalize ->
            key

        NormalizeToCircleOfFifths ->
            if keyNeedsNormalization normalizeSettings key then
                key
                    |> findFirstEnharmonicEquivalent
                        (majorKeysWith7AccidentalsOrFewer
                            ++ minorKeysWith7AccidentalsOrFewer
                        )

            else
                key

        Simplify preferGFlatOrFSharp ->
            if keyNeedsNormalization normalizeSettings key then
                key
                    |> findFirstEnharmonicEquivalent
                        (majorKeysWith5AccidentalsOrFewer
                            ++ minorKeysWith5AccidentalsOrFewer
                            ++ [ keyForGFlatOrFSharpPreference preferGFlatOrFSharp ]
                        )

            else
                key


keyNeedsNormalization : NormalizeSettings -> Key.Key -> Bool
keyNeedsNormalization normalizeSettings key =
    case normalizeSettings of
        DontNormalize ->
            False

        NormalizeToCircleOfFifths ->
            ((Key.signature key |> List.length) >= 8)
                || (Key.signature key
                        |> List.any
                            (\pc -> abs (PitchClass.accidentals pc) >= 2)
                   )

        Simplify _ ->
            (Key.signature key |> List.length) >= 6


keyForGFlatOrFSharpPreference : PreferGFlatOrFSharp -> Key.Key
keyForGFlatOrFSharpPreference pref =
    case pref of
        PreferGFlat ->
            Key.gFlat

        PreferFSharp ->
            Key.fSharp


findFirstEnharmonicEquivalent : List Key.Key -> Key.Key -> Key.Key
findFirstEnharmonicEquivalent options keyToReplace =
    options
        |> List.filter (Key.areEnharmonicEquivalents keyToReplace)
        |> List.head
        |> Maybe.withDefault Key.c


majorKeysWith7AccidentalsOrFewer : List Key.Key
majorKeysWith7AccidentalsOrFewer =
    [ Key.c
    , Key.g
    , Key.d
    , Key.a
    , Key.e
    , Key.b
    , Key.cFlat
    , Key.fSharp
    , Key.gFlat
    , Key.dFlat
    , Key.cSharp
    , Key.aFlat
    , Key.eFlat
    , Key.bFlat
    , Key.f
    ]


minorKeysWith7AccidentalsOrFewer : List Key.Key
minorKeysWith7AccidentalsOrFewer =
    majorKeysWith7AccidentalsOrFewer
        |> List.map Key.relative


majorKeysWith5AccidentalsOrFewer : List Key.Key
majorKeysWith5AccidentalsOrFewer =
    [ Key.c
    , Key.g
    , Key.d
    , Key.a
    , Key.e
    , Key.b
    , Key.dFlat
    , Key.aFlat
    , Key.eFlat
    , Key.bFlat
    , Key.f
    ]


minorKeysWith5AccidentalsOrFewer : List Key.Key
minorKeysWith5AccidentalsOrFewer =
    majorKeysWith5AccidentalsOrFewer
        |> List.map Key.relative


applyModeChange : ModulationDetails -> Key.Key -> Key.Key
applyModeChange details key =
    case details.modeChange of
        Just ChangeToRelativeKey ->
            Key.relative key

        Just ChangeToParallelKey ->
            Key.parallel key

        Nothing ->
            key


applyRootTransposition : ModulationDetails -> Key.Key -> Key.Key
applyRootTransposition details key =
    Key.setTonic
        (Key.tonic key
            |> PitchClass.transpose details.interval
        )
        key
