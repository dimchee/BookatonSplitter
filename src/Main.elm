module Main exposing (..)

import Browser
import Dict exposing (Dict)
import File exposing (File)
import File.Select
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Task


type alias Percentage =
    Int


type alias Img =
    { name : String
    , mime : String
    , url : Maybe String
    }


type alias Model =
    { percentage : Percentage
    , images : Dict String Img
    }


type Msg
    = ChangedPos Percentage
    | Split
    | ImagesRequested
    | ImagesLoaded File (List File)
    | GotUrl Img String


requestImages : Cmd Msg
requestImages =
    File.Select.files [ "image/png", "image/jpeg" ] ImagesLoaded


gradient : Percentage -> String
gradient p =
    "linear-gradient(rgba(0, 0, 0, 0.5)" ++ String.fromInt p ++ "%, black " ++ String.fromInt p ++ "%)"


viewImages : Dict String Img -> Html.Html Msg
viewImages =
    Dict.toList
        >> List.filterMap (Tuple.second >> .url)
        >> List.map
            (\url ->
                Html.img
                    [ Html.Attributes.src url
                    , Html.Attributes.height 100
                    ]
                    []
            )
        >> Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "justify-content" "center"
            , Html.Attributes.style "gap" "10px"
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { percentage = 0, images = Dict.empty }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html.Html Msg
view { percentage, images } =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        ]
        [ Html.img
            [ Html.Attributes.src "https://raw.githubusercontent.com/makccr/wallpapers/master/wallpapers/art/ARTWORK-bridal-procession-on-the-hardangerfjord.jpg"
            , Html.Attributes.height 500
            , Html.Attributes.style "-webkit-mask-image" <| gradient percentage
            , Html.Attributes.style "mask-image" <| gradient percentage
            , Html.Events.on "mousemove" mouseMoveDecoder
            , Html.Events.onClick Split
            , Html.Events.onMouseOut <| ChangedPos 0
            ]
            []
        , Html.button [ Html.Events.onClick ImagesRequested ] [ Html.text "Choose Files" ]
        , viewImages images
        ]


fileToImg : File -> Img
fileToImg file =
    { name = File.name file, mime = File.mime file, url = Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedPos x ->
            ( { model | percentage = x }, Cmd.none )

        Split ->
            ( model, Cmd.none )

        ImagesRequested ->
            ( model, requestImages )

        ImagesLoaded img imgs ->
            ( { model
                | images =
                    img
                        :: imgs
                        |> List.map (\x -> ( File.name x, fileToImg x ))
                        |> Dict.fromList
              }
            -- TODO Should be lazy (Don't load all images at once)
            , List.map (\file -> File.toUrl file |> Task.perform (GotUrl <| fileToImg file)) (img :: imgs) |> Cmd.batch
            )

        GotUrl img url ->
            ( { model | images = Dict.insert img.name { img | url = Just url } model.images }
            , Cmd.none
            )


mouseMoveDecoder : Json.Decode.Decoder Msg
mouseMoveDecoder =
    Json.Decode.field "offsetY" Json.Decode.int
        |> Json.Decode.map (\x -> ChangedPos <| round <| toFloat x * 100 / 500)
