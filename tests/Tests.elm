module Tests exposing (..)

import Test exposing (..)
import Expect
import String


{- internal -}

import Agenda exposing (..)


all : Test
all =
    describe "Agenda Test Suite"
        [ test "<+> two times with result" <|
            \() ->
                Expect.all
                    (addTwoToSucceed
                        [ [ 'a', 'b' ]
                        , [ 'b', 'a' ]
                        ]
                    )
                    (Just "ab")
        , test "<+> two times with error" <|
            \() ->
                Expect.equal
                    (error (runs addTwo [ 'a', 'a' ]))
                    True
        , test "<+> three times with result" <|
            \() ->
                Expect.all
                    (addThreeToSucceed
                        [ [ 'a', 'b', 'c' ]
                        , [ 'a', 'c', 'b' ]
                        , [ 'b', 'a', 'c' ]
                        , [ 'b', 'c', 'a' ]
                        , [ 'c', 'a', 'b' ]
                        , [ 'c', 'b', 'a' ]
                        ]
                    )
                    (Just "abc")
        , test "<+> three times with error" <|
            \() ->
                Expect.all
                    (addThreeToFail
                        [ [ 'a', 'b', 'b' ]
                        , [ 'a', 'a' ]
                        ]
                    )
                    True
        ]


addTwoToSucceed : List (List Char) -> List (Maybe String -> Expect.Expectation)
addTwoToSucceed =
    List.map (\chars -> Expect.equal (result (runs addTwo chars)))


addTwo : Agenda () Char String
addTwo =
    let
        concat a b =
            a ++ b
    in
        succeed2 concat
            |= (charA <+> charB)


addThreeToSucceed : List (List Char) -> List (Maybe String -> Expect.Expectation)
addThreeToSucceed =
    List.map (\chars -> Expect.equal (result (runs addThree chars)))


addThreeToFail : List (List Char) -> List (Bool -> Expect.Expectation)
addThreeToFail =
    List.map (\chars -> Expect.equal (error (runs addThree chars)))


addThree : Agenda () Char String
addThree =
    let
        concat a b c =
            a ++ b ++ c
    in
        succeed3 concat
            |= (charA <+> charB <+> charC)



{- setup -}


charA : Agenda () Char String
charA =
    tryChar 'a'


charB : Agenda () Char String
charB =
    tryChar 'b'


charC : Agenda () Char String
charC =
    tryChar 'c'


tryChar : Char -> Agenda () Char String
tryChar c =
    try <|
        \msg ->
            if msg == c then
                succeed (String.fromChar c)
            else
                fail
