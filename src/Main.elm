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
    , cactusSmall1 = Sprite 447 0 33 71
    , cactusSmall2 = Sprite 481 0 33 71
    , dino = Sprite 1678 0 88 96
    , ground = Sprite 0 103 2400 24
    }


type alias SpriteSheet =
    { ground : Texture
    , dino : Texture
    , cactusSmall1 : Texture
    , cactusSmall2 : Texture
    }


type Sprites
    = Loading
    | Loaded SpriteSheet
    | Failed


type alias Model =
    { backgroundOffset : Float
    , dino : Dino
    , speed : Float
    , sprites : Sprites
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( Model
        0
        (Dino 20 0 0 0)
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
        backgroundOffset =
            model.backgroundOffset + scrollSpeed / delta

        dino =
            model.dino
                |> gravity delta
                |> friction delta
                |> physics delta
    in
    { model
        | backgroundOffset =
            if backgroundOffset >= spriteSheetDefinition.ground.w then
                0

            else
                backgroundOffset
        , dino = dino
    }


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
                                    (spriteToTexture spriteSheetDefinition.cactusSmall1 t)
                                    (spriteToTexture spriteSheetDefinition.cactusSmall2 t)
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


renderBackground : Float -> SpriteSheet -> List Renderable
renderBackground bgOffset sprites =
    [ shapes [ fill Color.white ] [ rect ( 0, 0 ) canvasWidth canvasHeight ]
    , texture [] ( -bgOffset, translateY -(dinoSize / 2) ) sprites.ground
    , texture [] ( -bgOffset + spriteSheetDefinition.ground.w, translateY -(dinoSize / 2) ) sprites.ground
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


renderEnemies : SpriteSheet -> List Renderable
renderEnemies sprites =
    [ texture [] ( 400, translateY 0 ) sprites.cactusSmall1
    , texture [] ( 500, translateY 0 ) sprites.cactusSmall2
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
                renderBackground model.backgroundOffset sprites ++ renderEnemies sprites ++ renderDino sprites model.dino

            Failed ->
                renderText "failed"
        )
