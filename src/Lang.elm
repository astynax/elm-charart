module Lang exposing (..)

import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes as Attributes exposing (style)
import String


type alias Cell = ( Char, Color, Color )


type alias Line =
    List Cell


type alias Image =
    List Line


type alias Op =
    Image -> Image


type alias Combiner =
    Image -> Image -> Image


fromStrings : List String -> Image
fromStrings =
    List.map <|
        List.map (\c -> ( c, Color.black, Color.white ))
            << String.toList


display : Image -> Html a
display =
    Html.pre [] << List.intersperse (Html.br [] []) << List.map displayRow


displayRow : Line -> Html a
displayRow =
    Html.span [] << List.map displayCell


displayCell : Cell -> Html a
displayCell ( ch, fg, bg ) =
    Html.span
        [ style "background-color" (Color.toCssString bg)
        , style "color" (Color.toCssString fg)
        ]
        [ Html.text <| String.fromChar ch
        ]


flip : Op
flip =
    List.reverse


mirror : Op
mirror =
    List.map List.reverse


aside : Combiner
aside =
    List.map2 List.append


atop : Combiner
atop =
    List.append


fork : Combiner -> Op -> Op -> Op
fork c o1 o2 i =
    c (o1 i) (o2 i)


growH : Op
growH =
    List.map dup


growV : Op
growV =
    dup


grow : Op
grow =
    growV << growH


fill : Combiner
fill x =
    fillUsing x << List.map2 fillUsing x


map : (Int -> Int -> Cell -> Cell) -> Op
map f =
    List.indexedMap <|
        \y ->
            List.indexedMap <|
                \x -> f x y


uniform : Color -> Color -> Op
uniform fg bg =
    map <| \_ _ (c, _, _) -> ( c, fg, bg )



-- helpers


dup : List a -> List a
dup =
    List.concatMap (\x -> [ x, x ])


fillUsing : List a -> List a -> List a
fillUsing xs ys =
    let
        lx =
            List.length xs

        ly =
            List.length ys

        n =
            lx // ly
    in
    if ly == 0 then
        xs

    else
        List.take lx <| List.concatMap (always ys) <| List.range 0 n
