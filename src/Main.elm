module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Canvas exposing (Renderable, rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Html exposing (Html)
import Json.Decode as Decode


main : Program () Dino Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = renderWorld }



-- MODEL

canvasWidth =
    800


canvasHeight =
    200


dinoSize =
    50


type alias Box =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }

type alias Dino =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    }


init : () -> ( Dino, Cmd msg )
init _ =
    ( Dino 20 0 0 0
    , Cmd.none
    )



-- UPDATE


type Msg
    = AnimationFrame Float
    | KeyDown Direction


gravity : Float -> Dino -> Dino
gravity delta dino =
    { dino | vy = dino.vy - 350 / delta }

friction : Float -> Dino -> Dino
friction delta dino =
    applyIfOnGround dino { dino | vx = dino.vx / (delta / 15.5) }


physics : Float -> Dino -> Dino
physics delta dino =
    { x = clamp dinoSize (canvasWidth - dinoSize * 2) dino.x + dino.vx / delta
    , y = clamp 0 (canvasHeight - dinoSize) (dino.y + dino.vy / delta)
    , vx = dino.vx
    , vy = dino.vy
    }


step : Float -> Dino -> Dino
step delta dino =
    dino
        |> gravity delta
        |> friction delta
        |> physics delta


applyIfOnGround : Dino -> Dino -> Dino
applyIfOnGround old new =
    if old.y == 0 then
        new

    else
        old


update : Msg -> Dino -> ( Dino, Cmd msg )
update msg dino =
    case msg of
        AnimationFrame delta ->
            ( step delta dino
            , Cmd.none
            )

        KeyDown direction ->
            case direction of
                Left ->
                    ( applyIfOnGround dino { dino | vx = -100 }
                    , Cmd.none
                    )

                Right ->
                    ( applyIfOnGround dino { dino | vx = 100 }
                    , Cmd.none
                    )

                Up ->
                    ( applyIfOnGround dino { dino | vy = 300 }
                    , Cmd.none
                    )

                _ ->
                    ( dino, Cmd.none )



-- SUBSCRIPTIONS


type Direction
    = Left
    | Right
    | Up
    | Down
    | Other


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    case string of
        "ArrowLeft" ->
            KeyDown Left

        "ArrowRight" ->
            KeyDown Right

        "ArrowUp" ->
            KeyDown Up

        "ArrowDown" ->
            KeyDown Down

        " " ->
            KeyDown Up

        _ ->
            KeyDown Other


subscriptions : Dino -> Sub Msg
subscriptions _ =
    Sub.batch <|
        [ onKeyDown keyDecoder
        , onAnimationFrameDelta AnimationFrame
        ]



-- VIEW


translateY : Float -> Float
translateY y =
    -y + canvasHeight - dinoSize


renderBackground : Dino -> Renderable
renderBackground _ =
    shapes [ fill Color.gray ] [ rect ( 0, 0 ) canvasWidth canvasHeight ]


renderDino : Dino -> Renderable
renderDino dino =
    shapes
        [ fill Color.black ]
        [ rect
            ( dino.x, translateY dino.y )
            dinoSize
            dinoSize
        ]


renderWorld : Dino -> Html msg
renderWorld dino =
    Canvas.toHtml ( round canvasWidth, round canvasHeight )
        []
        [ renderBackground dino
        , renderDino dino
        ]
