module Test.Util exposing (..)

import Expect
import MusicTheory.Pitch as Pitch
import MusicTheory.Voicing.FourPart as FourPart


expectAllInList : (a -> Maybe String) -> List a -> Expect.Expectation
expectAllInList maybeErrorDescription list =
    List.map maybeErrorDescription list
        |> List.filterMap identity
        |> (\errorDescriptions ->
                case errorDescriptions of
                    [] ->
                        Expect.pass

                    nonEmptyList ->
                        nonEmptyList
                            |> String.join ", "
                            |> (\str ->
                                    "[ " ++ str ++ "]"
                               )
                            |> Expect.fail
           )


voicingToString : FourPart.Voicing -> String
voicingToString theVoicing =
    theVoicing
        |> FourPart.toPitches
        |> (\{ voiceOne, voiceTwo, voiceThree, voiceFour } ->
                String.join " "
                    [ Pitch.toString voiceFour
                    , Pitch.toString voiceThree
                    , Pitch.toString voiceTwo
                    , Pitch.toString voiceOne
                    ]
           )
