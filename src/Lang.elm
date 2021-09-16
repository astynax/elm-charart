module Lang exposing (..)

import String


type alias Image =
    List String


type alias Op =
    Image -> Image


type alias Combiner =
    Image -> Image -> Image


display : Image -> String
display =
    String.join "\n"


flip : Op
flip =
    List.reverse


mirror : Op
mirror =
    List.map String.reverse


aside : Combiner
aside =
    List.map2 String.append


atop : Combiner
atop =
    List.append


fork : Combiner -> Op -> Op -> Op
fork c o1 o2 i =
    c (o1 i) (o2 i)


growH : Op
growH =
    List.map (String.fromList << dup << String.toList)


growV : Op
growV =
    dup


grow : Op
grow =
    growV << growH


fill : Combiner
fill x y =
    let
        z =
            List.map2
                (\s1 s2 ->
                    String.fromList <|
                        fillUsing (String.toList s1) (String.toList s2)
                )
                x
                y
    in
    fillUsing x z



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
