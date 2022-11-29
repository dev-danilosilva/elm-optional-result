module OptionalResultTest exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (..)
import OptionalResult
import Test exposing (..)


suite : Test
suite =
    describe "Maybe Result use cases"
        [ describe "mapping success"
            [ fuzz int "map identity should return the same OptionalResult value" <|
                \x ->
                    OptionalResult.map identity (OptionalResult.Success x)
                        |> Expect.equal (OptionalResult.Success x)
            , fuzz int "map to different types should work as expected" <|
                \x ->
                    OptionalResult.map String.fromInt (OptionalResult.Success x)
                        |> Expect.equal (OptionalResult.Success (String.fromInt x))
            , fuzz int "mapping a value with oposite operations should result the original value" <|
                \x ->
                    OptionalResult.Success x
                        |> OptionalResult.map (\value -> value + 1)
                        |> OptionalResult.map (\value -> value - 1)
                        |> Expect.equal (OptionalResult.Success x)
            , fuzz2 int int "Given a comutative combine function, the map2 argument order does not change the result" <|
                \x1 x2 ->
                    OptionalResult.map2 (+) (OptionalResult.Success x1) (OptionalResult.Success x2)
                        |> Expect.equal (OptionalResult.Success (x2 + x1))
            , fuzz3 int int int "Given a comutative combine function, the map3 argument order does not change the result" <|
                \t u v ->
                    OptionalResult.map3 (\x y z -> x + y + z)
                        (OptionalResult.Success t)
                        (OptionalResult.Success u)
                        (OptionalResult.Success v)
                        |> Expect.all
                            [ Expect.equal (OptionalResult.Success <| t + u + v)
                            , Expect.equal (OptionalResult.Success <| t + v + u)
                            , Expect.equal (OptionalResult.Success <| u + t + v)
                            , Expect.equal (OptionalResult.Success <| u + v + t)
                            , Expect.equal (OptionalResult.Success <| v + t + u)
                            , Expect.equal (OptionalResult.Success <| v + u + t)
                            ]
            , fuzz map4
                (\t u v w -> Dict.fromList [ ( "t", t ), ( "u", u ), ( "w", w ), ( "v", v ) ])
                int
                int
                int
                int
                "Given a comutative combine function, the map4 argument order does not change the result"
              <|
                \intDict ->
                    let
                        t =
                            intDict |> Dict.get "t" |> Maybe.withDefault 1

                        u =
                            intDict |> Dict.get "u" |> Maybe.withDefault 2

                        v =
                            intDict |> Dict.get "v" |> Maybe.withDefault 3

                        w =
                            intDict |> Dict.get "w" |> Maybe.withDefault 4
                    in
                    OptionalResult.map4 (\a b c d -> a + b + c + d)
                        (OptionalResult.Success t)
                        (OptionalResult.Success u)
                        (OptionalResult.Success v)
                        (OptionalResult.Success w)
                        |> Expect.all
                            [ Expect.equal (OptionalResult.Success <| t + u + v + w)
                            , Expect.equal (OptionalResult.Success <| t + u + w + v)
                            , Expect.equal (OptionalResult.Success <| t + v + w + u)
                            , Expect.equal (OptionalResult.Success <| t + v + u + w)
                            , Expect.equal (OptionalResult.Success <| t + w + u + v)
                            , Expect.equal (OptionalResult.Success <| t + w + v + u)
                            , Expect.equal (OptionalResult.Success <| u + t + w + v)
                            , Expect.equal (OptionalResult.Success <| u + t + v + w)
                            , Expect.equal (OptionalResult.Success <| u + w + v + t)
                            , Expect.equal (OptionalResult.Success <| u + w + t + v)
                            , Expect.equal (OptionalResult.Success <| u + v + w + t)
                            , Expect.equal (OptionalResult.Success <| u + v + t + w)
                            , Expect.equal (OptionalResult.Success <| v + t + u + w)
                            , Expect.equal (OptionalResult.Success <| v + t + w + u)
                            , Expect.equal (OptionalResult.Success <| v + w + u + t)
                            , Expect.equal (OptionalResult.Success <| v + w + t + u)
                            , Expect.equal (OptionalResult.Success <| v + u + w + t)
                            , Expect.equal (OptionalResult.Success <| v + u + t + w)
                            , Expect.equal (OptionalResult.Success <| w + u + t + v)
                            , Expect.equal (OptionalResult.Success <| w + u + v + t)
                            , Expect.equal (OptionalResult.Success <| w + t + u + v)
                            , Expect.equal (OptionalResult.Success <| w + t + v + u)
                            , Expect.equal (OptionalResult.Success <| w + v + t + u)
                            , Expect.equal (OptionalResult.Success <| w + v + u + t)
                            ]
            , test "mapping4 four equal type values" <|
                \_ ->
                    OptionalResult.map4 (\x y z w -> x + y + z + w)
                        (OptionalResult.Success 1)
                        (OptionalResult.Success 2)
                        (OptionalResult.Success 3)
                        (OptionalResult.Success 4)
                        |> Expect.equal (OptionalResult.Success 10)
            , test "mapping5 five equal type values" <|
                \_ ->
                    OptionalResult.map5 (\s t u v w -> s + t + u + v + w)
                        (OptionalResult.Success 1)
                        (OptionalResult.Success 2)
                        (OptionalResult.Success 3)
                        (OptionalResult.Success 4)
                        (OptionalResult.Success 5)
                        |> Expect.equal (OptionalResult.Success 15)
            , test "mapping6 six equal type values" <|
                \_ ->
                    OptionalResult.map6 (\s t u v w x -> s + t + u + v + w + x)
                        (OptionalResult.Success 1)
                        (OptionalResult.Success 2)
                        (OptionalResult.Success 3)
                        (OptionalResult.Success 4)
                        (OptionalResult.Success 5)
                        (OptionalResult.Success 6)
                        |> Expect.equal (OptionalResult.Success 21)
            ]
        , describe "mapping error"
            [ fuzz int "map OptionalResult.Error" <|
                \x ->
                    OptionalResult.map ((+) x)
                        (OptionalResult.Error "Error Message")
                        |> Expect.equal (OptionalResult.Error "Error Message")
            , fuzz string "mapping error returns the same mapping error result" <|
                \x ->
                    OptionalResult.map ((+) 1)
                        (OptionalResult.Error x)
                        |> Expect.equal (OptionalResult.Error x)
            , test "mapping2 an OptionalResult.Error should return first error message" <|
                \_ ->
                    OptionalResult.map2 (+)
                        (OptionalResult.Error "Error Message 1")
                        (OptionalResult.Error "Error Message 2")
                        |> Expect.equal (OptionalResult.Error "Error Message 1")
            , test "map2 first argument OptionalResult.Error and second OptionalResult.Empty" <|
                \_ ->
                    OptionalResult.map2 (+)
                        (OptionalResult.Error "Error Message")
                        OptionalResult.Empty
                        |> Expect.equal (OptionalResult.Error "Error Message")
            , test "map2 first argument OptionalResult.Empty and second OptionalResult.Error" <|
                \_ ->
                    OptionalResult.map2 (+)
                        OptionalResult.Empty
                        (OptionalResult.Error "Error Message")
                        |> Expect.equal (OptionalResult.Error "Error Message")
            , test "map3 first argument as OptionalResult.Error" <|
                \_ ->
                    OptionalResult.map3 (\x y z -> x + y + z)
                        (OptionalResult.Error "Error Message")
                        (OptionalResult.Success 2)
                        (OptionalResult.Success 3)
                        |> Expect.equal (OptionalResult.Error "Error Message")
            , test "map3 second argument as OptionalResult.Error" <|
                \_ ->
                    OptionalResult.map3 (\x y z -> x + y + z)
                        (OptionalResult.Success 1)
                        (OptionalResult.Error "Error Message")
                        (OptionalResult.Success 3)
                        |> Expect.equal (OptionalResult.Error "Error Message")
            , test "map3 third argument as OptionalResult.Error" <|
                \_ ->
                    OptionalResult.map3 (\x y z -> x + y + z)
                        (OptionalResult.Success 1)
                        (OptionalResult.Success 2)
                        (OptionalResult.Error "Error Message")
                        |> Expect.equal (OptionalResult.Error "Error Message")
            , test "map3 all arguments as OptionalResult.Error" <|
                \_ ->
                    OptionalResult.map3 (\x y z -> x + y + z)
                        (OptionalResult.Error "Error Message")
                        (OptionalResult.Error "Error Message")
                        (OptionalResult.Error "Error Message")
                        |> Expect.equal (OptionalResult.Error "Error Message")
            , test "map4 first argument as OptionalResult.Error" <|
                \_ ->
                    OptionalResult.map4 (\w x y z -> w + x + y + z)
                        (OptionalResult.Error "Error Message")
                        (OptionalResult.Success 2)
                        (OptionalResult.Success 3)
                        (OptionalResult.Success 4)
                        |> Expect.equal (OptionalResult.Error "Error Message")
            , test "map4 second argument as OptionalResult.Error" <|
                \_ ->
                    OptionalResult.map4 (\w x y z -> w + x + y + z)
                        (OptionalResult.Success 1)
                        (OptionalResult.Error "Error Message")
                        (OptionalResult.Success 3)
                        (OptionalResult.Success 4)
                        |> Expect.equal (OptionalResult.Error "Error Message")
            , test "map4 third argument as OptionalResult.Error" <|
                \_ ->
                    OptionalResult.map4 (\w x y z -> w + x + y + z)
                        (OptionalResult.Success 1)
                        (OptionalResult.Success 2)
                        (OptionalResult.Error "Error Message")
                        (OptionalResult.Success 4)
                        |> Expect.equal (OptionalResult.Error "Error Message")
            , test "map4 fourth argument as OptionalResult.Error" <|
                \_ ->
                    OptionalResult.map4 (\w x y z -> w + x + y + z)
                        (OptionalResult.Success 1)
                        (OptionalResult.Success 2)
                        (OptionalResult.Success 3)
                        (OptionalResult.Error "Error Message")
                        |> Expect.equal (OptionalResult.Error "Error Message")
            , test "map4 all arguments as OptionalResult.Error" <|
                \_ ->
                    OptionalResult.map4 (\w x y z -> w + x + y + z)
                        (OptionalResult.Error "Error Message")
                        (OptionalResult.Error "Error Message")
                        (OptionalResult.Error "Error Message")
                        (OptionalResult.Error "Error Message")
                        |> Expect.equal (OptionalResult.Error "Error Message")
            ]
        , describe "mapping empty"
            [ test "map OptionalResult.Empty" <|
                \_ ->
                    OptionalResult.map ((+) 5)
                        OptionalResult.Empty
                        |> Expect.equal OptionalResult.Empty
            , test "map2 first argument as OptionalResult.Empty" <|
                \_ ->
                    OptionalResult.map2 (+)
                        OptionalResult.Empty
                        (OptionalResult.Success 3)
                        |> Expect.equal OptionalResult.Empty
            , test "map2 second argument as OptionalResult.Empty" <|
                \_ ->
                    OptionalResult.map2 (+)
                        (OptionalResult.Success 3)
                        OptionalResult.Empty
                        |> Expect.equal OptionalResult.Empty
            , test "map3 with all arguments as OptionalResult.Empty" <|
                \_ ->
                    OptionalResult.map3 (\x y z -> x + y + z)
                        OptionalResult.Empty
                        OptionalResult.Empty
                        OptionalResult.Empty
                        |> Expect.equal OptionalResult.Empty
            , test "map3 with first argument as OptionalResult.Empty" <|
                \_ ->
                    OptionalResult.map3 (\x y z -> x + y + z)
                        OptionalResult.Empty
                        (OptionalResult.Success 2)
                        (OptionalResult.Success 3)
                        |> Expect.equal OptionalResult.Empty
            , test "map3 with second argument as OptionalResult.Empty" <|
                \_ ->
                    OptionalResult.map3 (\x y z -> x + y + z)
                        (OptionalResult.Success 1)
                        OptionalResult.Empty
                        (OptionalResult.Success 3)
                        |> Expect.equal OptionalResult.Empty
            , test "map3 with third argument as OptionalResult.Empty" <|
                \_ ->
                    OptionalResult.map3 (\x y z -> x + y + z)
                        (OptionalResult.Success 1)
                        (OptionalResult.Success 2)
                        OptionalResult.Empty
                        |> Expect.equal OptionalResult.Empty
            , test "map4 with all arguments as OptionalResult.Empty" <|
                \_ ->
                    OptionalResult.map4 (\w x y z -> w + x + y + z)
                        OptionalResult.Empty
                        OptionalResult.Empty
                        OptionalResult.Empty
                        OptionalResult.Empty
                        |> Expect.equal OptionalResult.Empty
            , test "map4 with first argument as OptionalResult.Empty" <|
                \_ ->
                    OptionalResult.map4 (\w x y z -> w + x + y + z)
                        OptionalResult.Empty
                        (OptionalResult.Success 2)
                        (OptionalResult.Success 3)
                        (OptionalResult.Success 4)
                        |> Expect.equal OptionalResult.Empty
            , test "map4 with second argument as OptionalResult.Empty" <|
                \_ ->
                    OptionalResult.map4 (\w x y z -> w + x + y + z)
                        (OptionalResult.Success 1)
                        OptionalResult.Empty
                        (OptionalResult.Success 3)
                        (OptionalResult.Success 4)
                        |> Expect.equal OptionalResult.Empty
            , test "map4 with third argument as OptionalResult.Empty" <|
                \_ ->
                    OptionalResult.map4 (\w x y z -> w + x + y + z)
                        (OptionalResult.Success 1)
                        (OptionalResult.Success 2)
                        OptionalResult.Empty
                        (OptionalResult.Success 4)
                        |> Expect.equal OptionalResult.Empty
            , test "map4 with fourth argument as OptionalResult.Empty" <|
                \_ ->
                    OptionalResult.map4 (\w x y z -> w + x + y + z)
                        (OptionalResult.Success 1)
                        (OptionalResult.Success 2)
                        (OptionalResult.Success 3)
                        OptionalResult.Empty
                        |> Expect.equal OptionalResult.Empty
            ]
        ]
