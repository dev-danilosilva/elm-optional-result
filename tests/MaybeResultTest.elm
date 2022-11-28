module MaybeResultTest exposing (..)

import Expect exposing (Expectation)
import MaybeResult exposing (MaybeResult)
import Test exposing (..)


suite : Test
suite =
    describe "MaybeResult use cases"
        [ describe "isError behaviour"
            [ test "isError testing a error value" <|
                \_ ->
                    MaybeResult.isError (MaybeResult.error "placeholder")
                        |> Expect.equal True
            , test "isError testing a success value" <|
                \_ ->
                    MaybeResult.isError (MaybeResult.success "placeholder")
                        |> Expect.equal False
            , test "isError testing an empty value" <|
                \_ ->
                    MaybeResult.isError MaybeResult.nothing
                        |> Expect.equal False
            ]
        , describe "isSucces behavior"
            [ test "isSuccess testing a error value" <|
                \_ ->
                    MaybeResult.isSuccess (MaybeResult.error "placeholder")
                        |> Expect.equal False
            , test "isSuccess testing a success value" <|
                \_ ->
                    MaybeResult.isSuccess (MaybeResult.success "placeholder")
                        |> Expect.equal True
            , test "isSuccess testing an empty value" <|
                \_ ->
                    MaybeResult.isSuccess MaybeResult.nothing
                        |> Expect.equal False
            ]
        , describe "map function behaviour"
            [ test "when mapping a success to the same type value" <|
                \_ ->
                    MaybeResult.map sqrt (MaybeResult.success 9.0)
                        |> Expect.equal (MaybeResult.success 3.0)
            , test "when mapping a success to another type" <|
                \_ ->
                    MaybeResult.map String.fromInt (MaybeResult.success 4)
                        |> Expect.equal (MaybeResult.success "4")
            , test "when mapping error" <|
                \_ ->
                    MaybeResult.map String.fromInt (MaybeResult.error "Error Message")
                        |> Expect.equal (MaybeResult.error "Error Message")
            , test "when mapping nothing" <|
                \_ ->
                    MaybeResult.map String.fromInt MaybeResult.nothing
                        |> Expect.equal MaybeResult.nothing
            ]
        ]
