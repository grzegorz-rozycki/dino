module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Canvas exposing (Renderable, rect, shapes, text, texture)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Text exposing (TextAlign(..), align, font, maxWidth)
import Canvas.Texture as Texture exposing (Texture)
import Color
import Html exposing (Html)
import Json.Decode as Decode


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = renderWorld }



-- MODEL


canvasWidth =
    800


canvasHeight =
    300


dinoSize =
    96


scrollSpeed =
    100


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


type alias Sprite =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


spriteToTexture : Sprite -> Texture -> Texture
spriteToTexture sprite texture =
    Texture.sprite
        { x = sprite.x
        , y = sprite.y
        , width = sprite.w
        , height = sprite.h
        }
        texture


spriteSheetDefinition =
    { file = "./img/spritesheet.png"
    , dino = Sprite 1678 0 (1766 - 1678) 96
    , ground = Sprite 0 103 2400 24
    }


type alias SpriteSheet =
    { ground : Texture
    , dino : Texture
    }


type Sprites
    = Loading
    | Loaded SpriteSheet
    | Failed


type alias Model =
    { dino : Dino
    , distance : Float
    , speed : Float
    , sprites : Sprites
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( Model
        (Dino 20 0 0 0)
        0
        0
        Loading
    , Cmd.none
    )


type Msg
    = AnimationFrame Float
    | KeyDown Direction
    | TextureLoaded (Maybe Texture)


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


step : Float -> Model -> Model
step delta model =
    let
        dino =
            model.dino
                |> gravity delta
                |> friction delta
                |> physics delta
    in
    { model | dino = dino }


applyIfOnGround : Dino -> Dino -> Dino
applyIfOnGround old new =
    if old.y == 0 then
        new

    else
        old


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        AnimationFrame delta ->
            ( step delta model
            , Cmd.none
            )

        KeyDown direction ->
            let
                dino =
                    model.dino
            in
            case direction of
                Left ->
                    ( { model | dino = applyIfOnGround dino { dino | vx = -100 } }
                    , Cmd.none
                    )

                Right ->
                    ( { model | dino = applyIfOnGround dino { dino | vx = 100 } }
                    , Cmd.none
                    )

                Up ->
                    ( { model | dino = applyIfOnGround dino { dino | vy = 300 } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TextureLoaded texture ->
            case texture of
                Nothing ->
                    ( { model | sprites = Failed }
                    , Cmd.none
                    )

                Just t ->
                    ( { model
                        | sprites =
                            Loaded
                                (SpriteSheet
                                    (spriteToTexture spriteSheetDefinition.ground t)
                                    (spriteToTexture spriteSheetDefinition.dino t)
                                )
                      }
                    , Cmd.none
                    )



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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch <|
        [ onKeyDown keyDecoder
        , onAnimationFrameDelta AnimationFrame
        ]



-- VIEW


textures : List (Texture.Source Msg)
textures =
    [ Texture.loadFromImageUrl spriteSheetDefinition.file TextureLoaded
    ]


translateY : Float -> Float
translateY y =
    -y + canvasHeight - dinoSize


renderBackground : SpriteSheet -> List Renderable
renderBackground sprites =
    [ shapes [ fill Color.white ] [ rect ( 0, 0 ) canvasWidth canvasHeight ]
    , texture [] ( toFloat 0, translateY -(dinoSize / 2) ) sprites.ground
    ]


renderDino : SpriteSheet -> Dino -> List Renderable
renderDino sprites dino =
    [ shapes
        [ fill (Color.rgba 0 0 0 0.5) ]
        [ rect
            ( dino.x, translateY dino.y )
            dinoSize
            dinoSize
        ]
    , texture [] ( dino.x, translateY dino.y ) sprites.dino
    ]


renderText : String -> List Renderable
renderText txt =
    [ text
        [ font { size = 48, family = "sans-serif" }
        , align Center
        , maxWidth canvasWidth
        ]
        ( canvasWidth / 2, canvasHeight / 2 - 24 )
        txt
    ]


renderWorld : Model -> Html Msg
renderWorld model =
    Canvas.toHtmlWith
        { width = round canvasWidth
        , height = round canvasHeight
        , textures = textures
        }
        []
        (case model.sprites of
            Loading ->
                renderText "Loading"

            Loaded sprites ->
                renderBackground sprites ++ renderDino sprites model.dino

            Failed ->
                renderText "failed"
        )
