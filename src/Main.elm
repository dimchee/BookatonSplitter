module Main exposing (..)

import Browser
import Html
import Html.Attributes
import Html.Events
import Images
import Json.Decode
import List.Extra


type alias Model =
    { percentage : Images.Percentage
    , page : Int
    , pageIncStep : Int
    , images : Images.Model
    }


type Msg
    = ChangedPos Images.Percentage
    | Images Images.Msg
    | ChangedPageIncStep (Maybe Int)
    | VisitLastModification
    | VisitNextModification
    | Modify Images.Percentage


gradient : Images.Percentage -> String
gradient p =
    "linear-gradient(rgba(0, 0, 0, 0.5)" ++ String.fromInt p ++ "%, black " ++ String.fromInt p ++ "%)"


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                let
                    ( images, cmd ) =
                        -- Images.init "makccr/wallpapers"
                        Images.initJson
                in
                ( { percentage = 0
                  , page = 0
                  , pageIncStep = 5
                  , images = images
                  }
                , Cmd.map Images cmd
                )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


viewImage : Model -> Int -> Html.Html Msg
viewImage { images, percentage } i =
    case Images.getImageUrl i images of
        Just url ->
            Html.img
                [ Html.Attributes.src url
                , Html.Attributes.height 500
                , Html.Attributes.style "-webkit-mask-image" <| gradient percentage
                , Html.Attributes.style "mask-image" <| gradient percentage
                , Html.Events.on "mousemove" mouseMoveDecoder
                , Html.Events.onClick <| Modify percentage
                , Html.Events.onMouseOut <| ChangedPos 0
                ]
                []

        Nothing ->
            Html.div [] []


view : Model -> Html.Html Msg
view ({ images, page } as model) =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        ]
        [ Html.div
            []
            [ Html.button [ Html.Events.onClick VisitLastModification ] [ Html.text "Back" ]
            , Html.button [ Html.Events.onClick VisitNextModification ] [ Html.text "Step" ]
            , Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "1"
                , Html.Attributes.max "20"
                , Html.Attributes.value <| String.fromInt model.pageIncStep
                , Html.Events.onInput (String.toInt >> ChangedPageIncStep)
                ]
                []
            , Html.text <| String.fromInt model.pageIncStep
            ]
        , viewImage model page

        -- , Html.button [ Html.Events.onClick ImagesRequested ] [ Html.text "Choose Files" ]
        -- , Html.map Images <| Images.viewImages images.images
        , Html.map Images <| Images.view images
        ]


closestModFiltered : (Int -> Bool) -> Model -> Int
closestModFiltered isGood { page, pageIncStep, images } =
    Images.getModifications images
        |> List.map Tuple.first
        |> (\l ->
                List.Extra.last l
                    |> Maybe.map (\x -> (x + pageIncStep |> Images.clampIndex images) :: l)
                    |> Maybe.withDefault l
           )
        |> List.filter isGood
        |> List.Extra.minimumBy (\ind -> abs <| ind - page)
        |> Maybe.withDefault page


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedPos x ->
            ( { model | percentage = x }, Cmd.none )

        Images childMsg ->
            let
                ( newImages, cmd ) =
                    Images.update childMsg model.images
            in
            ( { model | images = newImages }, Cmd.map Images cmd )

        ChangedPageIncStep m ->
            ( Maybe.map (\x -> { model | pageIncStep = x }) m |> Maybe.withDefault model, Cmd.none )

        VisitNextModification ->
            ( { model | page = closestModFiltered ((<) model.page) model }, Cmd.none )

        VisitLastModification ->
            ( { model | page = closestModFiltered ((>) model.page) model }, Cmd.none )

        Modify percentage ->
            { model
                | images = Images.modify ( model.page, percentage ) model.images
            }
                |> update VisitNextModification


mouseMoveDecoder : Json.Decode.Decoder Msg
mouseMoveDecoder =
    Json.Decode.field "offsetY" Json.Decode.int
        |> Json.Decode.map (\x -> ChangedPos <| round <| toFloat x * 100 / 500)
