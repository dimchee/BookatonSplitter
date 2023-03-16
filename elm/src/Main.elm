module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import List.Extra


type alias Percentage =
    Float


type alias Img =
    Int


type alias Model =
    { percentage : Percentage
    , page : Int
    , pageIncStep : Int
    , modifications : Dict Img Percentage
    }


type Msg
    = ChangedPos Percentage
    | ChangedPageIncStep (Maybe Int)
    | VisitLastModification
    | VisitNextModification
    | Modify Percentage


gradient : Percentage -> String
gradient p =
    "linear-gradient(rgba(0, 0, 0, 0.5)" ++ String.fromFloat p ++ "%, black " ++ String.fromFloat p ++ "%)"


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ -> ( Model 0 0 5 Dict.empty, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


viewImage : Percentage -> Int -> Html.Html Msg
viewImage percentage ind =
    Html.img
        [ Html.Attributes.src <| "images/" ++ String.fromInt ind
        , Html.Attributes.height 1000
        , Html.Attributes.style "-webkit-mask-image" <| gradient percentage
        , Html.Attributes.style "mask-image" <| gradient percentage
        , Html.Events.on "mousemove" mouseMoveDecoder
        , Html.Events.onClick <| Modify percentage
        , Html.Events.onMouseOut <| ChangedPos 0
        , Html.Attributes.style "object-fit" "contain"
        ]
        []


view : Model -> Html.Html Msg
view { percentage, page, pageIncStep } =
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
                , Html.Attributes.value <| String.fromInt pageIncStep
                , Html.Events.onInput (String.toInt >> ChangedPageIncStep)
                ]
                []
            , Html.text <| String.fromInt pageIncStep
            ]
        , viewImage percentage page
        ]


closestModFiltered : (Int -> Bool) -> Model -> Int
closestModFiltered isGood { page, pageIncStep, modifications } =
    -- TODO clamp index (total images not known for now)
    modifications
        |> Dict.keys
        |> (\l ->
                List.Extra.last l
                    |> Maybe.map (\x -> (x + pageIncStep) :: l)
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

        ChangedPageIncStep m ->
            ( Maybe.map (\x -> { model | pageIncStep = x }) m |> Maybe.withDefault model, Cmd.none )

        VisitNextModification ->
            ( { model | page = closestModFiltered ((<) model.page) model }, Cmd.none )

        VisitLastModification ->
            ( { model | page = closestModFiltered ((>) model.page) model }, Cmd.none )

        Modify percentage ->
            { model
                | modifications = Dict.update model.page (\_ -> Just percentage) model.modifications
            }
                |> update VisitNextModification


mouseMoveDecoder : Json.Decode.Decoder Msg
mouseMoveDecoder =
    Json.Decode.field "offsetY" Json.Decode.int
        |> Json.Decode.map (\x -> ChangedPos <| toFloat x * 100 / 1000)
