module Main exposing (main)

import Color as C
import Html
import Lang exposing (..)


main =
    Html.div []
        [ display img1
        , display img2
        , display img3
        ]


img1 =
    let
        tp =
            flip << mirror

        sq =
            square identity tp

        colorize x y ( c, _, _ ) =
            let
                fg =
                    case c of
                        '#' ->
                            if modBy 3 (x + y) == 0 then
                                C.red

                            else
                                C.blue

                        '.' ->
                            C.yellow

                        '*' ->
                            C.green

                        _ ->
                            C.white
            in
            ( c, fg, C.black )

        build =
            sq << grow << sq << sq << growH
    in
    map colorize <|
        build <|
            fromStrings
                [ " .#"
                , "*  "
                ]


img2 =
    let
        navy =
            C.rgb255 0 0 80

        ch f c =
            ( c, f, navy )

        sp =
            ch C.white ' '

        clear =
            \i ->
                fill i
                    [ [ ch C.white '.', sp, sp ]
                    , [ sp, ch C.yellow '*', sp ]
                    , [ sp, sp, ch (C.rgb 0 80 80) '.' ]
                    ]

        cbrd =
            square identity clear

        step =
            mirror << cbrd << mirror << cbrd
    in
    growH <|
        step <|
            step <|
                uniform C.green navy <|
                    fromStrings
                        [ "/\\"
                        , "\\/"
                        ]


img3 =
    let
        fromPalette i =
            case modBy 4 i of
                0 ->
                    C.black

                1 ->
                    C.red

                2 ->
                    C.yellow

                _ ->
                    C.white

        blank =
            grow <| grow <| grow <| grow <| fromStrings [ " " ]

        paint x y _ =
            ( '*'
            , fromPalette (modBy 2 x)
            , fromPalette (y + x)
            )
    in
    square identity mirror <| map paint <| blank


square f1 f2 =
    fork atop (fork aside f1 f2) (fork aside f2 f1)
