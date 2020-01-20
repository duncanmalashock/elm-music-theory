module Pitch.SpellingTests exposing (all)

import Expect
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Octave as Octave exposing (OctaveError(..))
import MusicTheory.Pitch as Pitch
import MusicTheory.Pitch.Enharmonic exposing (EnharmonicTransformationError(..))
import MusicTheory.Pitch.Spelling as Spelling
import MusicTheory.PitchClass.Spelling exposing (Accidental(..))
import Test exposing (..)


all : Test
all =
    describe "Spelling Tests"
        [ test "C###4 should be spelled as D#4" <|
            \_ ->
                Pitch.pitch C Pitch.tripleSharp Octave.four
                    |> Spelling.simple
                    |> Expect.equal (Ok <| { letter = D, accidental = Sharp, octave = Octave.four })
        , test "B#5 should be spelled as C6" <|
            \_ ->
                Pitch.pitch B Pitch.sharp Octave.five
                    |> Spelling.simple
                    |> Expect.equal (Ok <| { letter = C, accidental = Natural, octave = Octave.six })
        , test "C5 as natural or sharp should be C5" <|
            \_ ->
                Pitch.pitch C Pitch.natural Octave.five
                    |> Spelling.naturalOrElseSharp
                    |> Expect.equal (Ok <| { letter = C, accidental = Natural, octave = Octave.five })
        , test "Db5 as natural or sharp should be C#5" <|
            \_ ->
                Pitch.pitch D Pitch.flat Octave.five
                    |> Spelling.naturalOrElseSharp
                    |> Expect.equal (Ok <| { letter = C, accidental = Sharp, octave = Octave.five })
        , test "C5 as natural or flat should be C5" <|
            \_ ->
                Pitch.pitch C Pitch.natural Octave.five
                    |> Spelling.naturalOrElseFlat
                    |> Expect.equal (Ok <| { letter = C, accidental = Natural, octave = Octave.five })
        , test "C#5 as natural or sharp should be Db5" <|
            \_ ->
                Pitch.pitch C Pitch.sharp Octave.five
                    |> Spelling.naturalOrElseFlat
                    |> Expect.equal (Ok <| { letter = D, accidental = Flat, octave = Octave.five })
        , test "B#3 as natural or sharp should be C4" <|
            \_ ->
                Pitch.pitch B Pitch.sharp Octave.three
                    |> Spelling.naturalOrElseSharp
                    |> Expect.equal (Ok <| { letter = C, accidental = Natural, octave = Octave.four })
        ]
