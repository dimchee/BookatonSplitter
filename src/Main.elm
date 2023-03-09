module Main exposing (..)

import Browser
import Html
import Html.Attributes
import Html.Events
import Images
import Json.Decode


type alias Percentage =
    Int


type alias Model =
    { percentage : Percentage
    , images : Images.Model
    }


type Msg
    = ChangedPos Percentage
    | Split
    | Images Images.Msg


gradient : Percentage -> String
gradient p =
    "linear-gradient(rgba(0, 0, 0, 0.5)" ++ String.fromInt p ++ "%, black " ++ String.fromInt p ++ "%)"


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                let
                    ( images, cmd ) =
                        Images.init "makccr/wallpapers"
                in
                ( { percentage = 0
                  , images = images
                  }
                , Cmd.map Images cmd
                )
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
        [ case Images.getImage 0 images of
            Just url ->
                Html.img
                [ Html.Attributes.src url
                , Html.Attributes.height 500
                , Html.Attributes.style "-webkit-mask-image" <| gradient percentage
                , Html.Attributes.style "mask-image" <| gradient percentage
                , Html.Events.on "mousemove" mouseMoveDecoder
                , Html.Events.onClick Split
                , Html.Events.onMouseOut <| ChangedPos 0
                ]
                []
            Nothing -> Html.div [] []
        -- , Html.button [ Html.Events.onClick ImagesRequested ] [ Html.text "Choose Files" ]
        -- , Html.map Images <| Images.viewImages images.images
        , Html.map Images <| Images.view images
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedPos x ->
            ( { model | percentage = x }, Cmd.none )

        Split ->
            ( model, Cmd.none )

        Images childMsg ->
            let
                ( newImages, cmd ) =
                    Images.update childMsg model.images
            in
            ( { model | images = newImages }, Cmd.map Images cmd )


mouseMoveDecoder : Json.Decode.Decoder Msg
mouseMoveDecoder =
    Json.Decode.field "offsetY" Json.Decode.int
        |> Json.Decode.map (\x -> ChangedPos <| round <| toFloat x * 100 / 500)
