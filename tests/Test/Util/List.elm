module Test.Util.List exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Util.List


all : Test
all =
    describe "all"
        [ test "rotateLeft" <|
            \_ ->
                let
                    expected =
                        [ 2, 3, 1 ]

                    result =
                        [ 1, 2, 3 ]
                            |> Util.List.rotateLeft 1
                in
                Expect.equal expected result
        ]
