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


type Split
    = Before
    | After
    | Middle Percentage


type alias Model =
    { percentage : Percentage
    , page : Int
    , pageIncStep : Int
    , modifications : Dict Img Split
    }


type Msg
    = ChangedPos Percentage
    | ChangedPageIncStep (Maybe Int)
    | VisitLastModification
    | VisitNextModification
    | VisitNextPage
    | VisitLastPage
    | Modify Split


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


splitButton : Maybe Split -> String -> Split -> Html.Html Msg
splitButton last text split =
    Html.button
        [ Html.Attributes.style "width" "100%"
        , Html.Attributes.style "border" "none"
        , Html.Attributes.style "font-size" "2em"
        , Html.Events.onClick <| Modify split
        , Maybe.andThen 
            (\x -> if x == split then Just <| Html.Attributes.style "background-color" "red" else Nothing)
            last
            |> Maybe.withDefault (Html.Attributes.style "background-color" "#f0f0f0")
        ]
        [ Html.text text ]


viewImage : Model -> Html.Html Msg
viewImage { percentage, page, modifications } =
    Html.div
        [ Html.Attributes.style "width" "min-content"
        ]
        [ splitButton (Dict.get page modifications) "Split Before" Before
        , Html.div
            [ Html.Attributes.style "position" "relative"
            , Html.Attributes.style "width" "min-content"
            ]
            [ case Dict.get page modifications of 
                Just (Middle p) ->
                    Html.span
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "width" "3px"
                    , Html.Attributes.style "top" <| String.fromFloat p ++ "%"
                    , Html.Attributes.style "bottom" "0"
                    , Html.Attributes.style "left" "50%"
                    -- , Html.Attributes.style "background-color" "red"
                    , Html.Attributes.style "transform" "translate(-50%, -50%) rotate(90deg)"
                    , Html.Attributes.style "z-index" "1"
                    , Html.Attributes.style "height" "700px"
                    , Html.Attributes.style "background"
                        "repeating-linear-gradient(0deg,red 0 3px,#0000 0 7px)"
                    ]
                    []
                _ -> Html.span [] []
            , Html.img
                [ Html.Attributes.src <| "images/" ++ String.fromInt page
                , Html.Attributes.height 900
                , Html.Attributes.style "-webkit-mask-image" <| gradient percentage
                , Html.Attributes.style "mask-image" <| gradient percentage
                , Html.Events.on "mousemove" mouseMoveDecoder
                , Html.Events.onClick <| Modify <| Middle percentage
                , Html.Events.onMouseOut <| ChangedPos 0
                , Html.Attributes.style "object-fit" "contain"
                ]
                []
            ]
        , splitButton (Dict.get page modifications) "Split After" After
        ]


view : Model -> Html.Html Msg
view ({ pageIncStep } as model) =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        ]
        [ Html.div
            []
            [ Html.button [ Html.Events.onClick VisitLastModification ] [ Html.text "Last Change" ]
            , Html.button [ Html.Events.onClick VisitNextModification ] [ Html.text "Next Change" ]
            , Html.button [ Html.Events.onClick VisitLastPage ] [ Html.text "Last Page" ]
            , Html.button [ Html.Events.onClick VisitNextPage ] [ Html.text "Next Page" ]
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
        , viewImage model
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

        VisitNextPage ->
            -- TODO clip index
            ( { model | page = model.page + 1 }, Cmd.none )

        VisitLastPage ->
            ( { model | page = max 0 <| model.page - 1 }, Cmd.none )

        Modify percentage ->
            { model
                | modifications = Dict.update model.page (\_ -> Just percentage) model.modifications
            }
                |> update VisitNextModification


mouseMoveDecoder : Json.Decode.Decoder Msg
mouseMoveDecoder =
    Json.Decode.field "offsetY" Json.Decode.int
        |> Json.Decode.map (\x -> ChangedPos <| toFloat x * 100 / 900)
