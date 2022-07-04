module Main exposing (main)

import Canvas exposing (Renderable, rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Html exposing (Html)


width =
    800


height =
    200



-- VIEW


renderBackground : Renderable
renderBackground =
    shapes [ fill Color.gray ] [ rect ( 0, 0 ) width height ]


renderDino : ( Float, Float ) -> Renderable
renderDino ( x, y ) =
    shapes
        [ fill (Color.rgba 0 0 0 1) ]
        [ rect ( x, y ) 50 50 ]


main : Html msg
main =
    Canvas.toHtml ( width, height )
        []
        [ renderBackground
        , renderDino ( 20, 150 )
        ]
