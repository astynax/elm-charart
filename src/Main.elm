module Main exposing (main)

import Html
import Lang exposing (..)


main =
    Html.ul []
        [ item img1
        , item img2
        ]


item i =
    Html.li [] [ Html.pre [] [ Html.text <| display i ] ]


img1 =
    let
        tp =
            flip << mirror

        sq =
            square identity tp
    in
    sq <|
        grow <|
            sq <|
                sq <|
                    growH
                        [ " .#"
                        , "*  "
                        ]


img2 =
    let
        clear =
            \i ->
                fill i
                    [ ".  "
                    , " * "
                    , "  ."
                    ]

        cbrd =
            square identity clear

        step =
            mirror << cbrd << mirror << cbrd
    in
    growH <|
        step <|
            step
                [ "/\\"
                , "\\/"
                ]


square f1 f2 =
    fork atop (fork aside f1 f2) (fork aside f2 f1)
