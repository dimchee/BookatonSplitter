module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Encode
import List.Extra


type alias Percentage =
    Float


type alias Img =
    Int


type Split
    = Before
    | After
    | Middle Percentage


type alias Splits =
    Dict Img Split


type alias Model =
    { percentage : Percentage
    , page : Int
    , pageIncStep : Int
    , splits : Splits
    , serverMsg : Maybe String
    }


type Direction
    = Last
    | Next


type Msg
    = ChangedPos Percentage
    | ChangedPageIncStep (Maybe Int)
    | VisitSplit Direction
    | VisitPage Direction
    | Modify Split
    | RemoveSplit
    | Step
    | Save
    | Saved (Result Http.Error String)
    | Load
    | Loaded (Result Http.Error (Dict Img Split))
    | ToPDF


gradient : Percentage -> String
gradient p =
    "linear-gradient(rgba(0, 0, 0, 0.5)" ++ String.fromFloat p ++ "%, black " ++ String.fromFloat p ++ "%)"


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ -> ( Model 0 5 5 Dict.empty Nothing, Cmd.none )
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
            (\x ->
                if x == split then
                    Just <| Html.Attributes.style "background-color" "red"

                else
                    Nothing
            )
            last
            |> Maybe.withDefault (Html.Attributes.style "background-color" "#f0f0f0")
        ]
        [ Html.text text ]


splitLine : Model -> Html.Html msg -> Html.Html msg
splitLine { page, splits } img =
    Html.div
        [ Html.Attributes.style "position" "relative"
        , Html.Attributes.style "width" "min-content"
        ]
        [ case Dict.get page splits of
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

            _ ->
                Html.span [] []
        , img
        ]


viewImage : Model -> Html.Html Msg
viewImage ({ percentage, page, splits } as model) =
    Html.div
        [ Html.Attributes.style "width" "min-content"
        , Html.Attributes.style "height" "100%"
        ]
        [ splitButton (Dict.get page splits) "Split Before" Before
        , splitLine model <|
            Html.img
                [ Html.Attributes.src <| "images/" ++ String.fromInt page
                , Html.Attributes.style "-webkit-mask-image" <| gradient percentage
                , Html.Attributes.style "mask-image" <| gradient percentage
                , Html.Events.on "mousemove" mouseMoveDecoder
                , Html.Events.onClick <| Modify <| Middle percentage
                , Html.Events.onMouseOut <| ChangedPos 0
                , Html.Attributes.style "object-fit" "contain"
                , Html.Attributes.height imageHeight
                ]
                []
        , splitButton (Dict.get page splits) "Split After" After
        ]


imageHeight : Int
imageHeight =
    1000


numOfPages : Int
numOfPages =
    -- TODO make this vvalue part of model (idealy through init)
    1128


navButton : String -> Msg -> Html.Html Msg
navButton text msg =
    Html.button [ Html.Attributes.style "font-size" "2em", Html.Events.onClick msg ] [ Html.text text ]


viewPageNum : Model -> Html.Html Msg
viewPageNum { page } =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        ]
        [ Html.div [ Html.Attributes.style "font-size" "2em" ]
            [ Html.text "Page ", numSpan (page + 1), Html.text " of ", numSpan numOfPages ]
        , Html.div []
            [ navButton "Last" <| VisitPage Last
            , navButton "Next" <| VisitPage Next
            , navButton "Step" <| Step
            ]
        ]


viewSplitNum : Model -> Html.Html Msg
viewSplitNum { splits, page } =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        ]
        [ Html.div [ Html.Attributes.style "font-size" "2em" ]
            [ Html.text "Split "
            , Dict.keys splits
                |> List.Extra.findIndex ((<) page)
                |> Maybe.withDefault (Dict.size splits)
                |> numSpan
            , Html.text " of "
            , numSpan <| Dict.size splits
            ]
        , Html.div []
            [ navButton "Last" <| VisitSplit Last
            , navButton "Next" <| VisitSplit Next
            , navButton "Del" RemoveSplit
            ]
        ]


numSpan : Int -> Html.Html Msg
numSpan num =
    Html.span
        [ Html.Attributes.style "width" "2em"
        , Html.Attributes.style "display" "inline-block"
        ]
        [ Html.text <| String.fromInt num ]


view : Model -> Html.Html Msg
view ({ pageIncStep, serverMsg } as model) =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "row"
        , Html.Attributes.style "justify-content" "center"
        ]
        [ viewImage model
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "justify-content" "center"
            , Html.Attributes.style "text-align" "center"
            ]
            [ viewSplitNum model
            , viewPageNum model
            , Html.div []
                [ Html.text "Step size "
                , numSpan pageIncStep
                , Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.min "1"
                    , Html.Attributes.max "30"
                    , Html.Attributes.value <| String.fromInt pageIncStep
                    , Html.Events.onInput (String.toInt >> ChangedPageIncStep)
                    ]
                    []
                ]
            , Html.div []
                [ navButton "Save" Save
                , navButton "Load" Load
                , navButton "PDF" ToPDF
                ]
            , Html.text <| Maybe.withDefault "No messages from server" serverMsg
            ]
        ]


closestSplitFiltered : (Int -> Bool) -> Model -> Int
closestSplitFiltered isGood { page, splits } =
    splits
        |> Dict.keys
        -- |> (\l -> (clamp 0 (numOfPages - 1) <| page + pageIncStep) :: l)
        |> List.filter isGood
        |> List.Extra.minimumBy (\ind -> abs <| ind - page)
        |> Maybe.withDefault page


chooseLastNext : Direction -> a -> a -> a
chooseLastNext dir last next =
    case dir of
        Last ->
            last

        Next ->
            next


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedPos x ->
            ( { model | percentage = x }, Cmd.none )

        ChangedPageIncStep m ->
            ( Maybe.map (\x -> { model | pageIncStep = x }) m |> Maybe.withDefault model, Cmd.none )

        Step ->
            ( { model | page = clamp 0 (numOfPages - 1) <| model.page + model.pageIncStep }, Cmd.none )

        VisitPage dir ->
            ( { model | page = clamp 0 (numOfPages - 1) <| model.page + chooseLastNext dir -1 1 }, Cmd.none )

        VisitSplit dir ->
            ( { model | page = closestSplitFiltered (chooseLastNext dir (>) (<) model.page) model }, Cmd.none )

        Modify percentage ->
            { model
                | splits = Dict.update model.page (\_ -> Just percentage) model.splits
            }
                |> update Step

        RemoveSplit ->
            ( { model
                | splits = Dict.remove model.page model.splits
              }
            , Cmd.none
            )

        Save ->
            ( model, save model.splits )

        Saved result ->
            ( { model | serverMsg = Result.toMaybe result }, Cmd.none )

        Load ->
            ( model, load )

        Loaded result ->
            Result.toMaybe result
                |> Maybe.map
                    (\m ->
                        ( { model
                            | splits = m
                            , serverMsg = Just <| "Loaded " ++ String.fromInt (Dict.size m) ++ " splits"
                          }
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault ( { model | serverMsg = Just "Could not load :P" }, Cmd.none )

        ToPDF ->
            ( model
            , Http.get
                { url = "topdf"
                , expect = Http.expectString Saved
                }
            )


toMods : Dict String Split -> Dict Img Split
toMods =
    Dict.toList >> List.filterMap (\( x, y ) -> String.toInt x |> Maybe.map (\a -> ( a, y ))) >> Dict.fromList


load : Cmd Msg
load =
    Http.get
        { url = "load"
        , expect =
            Http.expectJson Loaded (Json.Decode.dict splitDecoder |> Json.Decode.map toMods)
        }


splitDecoder : Json.Decode.Decoder Split
splitDecoder =
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "Before" ->
                            Json.Decode.succeed Before

                        "After" ->
                            Json.Decode.succeed After

                        _ ->
                            Json.Decode.fail "Not Split"
                )
        , Json.Decode.field "Middle" Json.Decode.float |> Json.Decode.map Middle
        ]


splitEncoder : Split -> Json.Encode.Value
splitEncoder split =
    case split of
        Before ->
            Json.Encode.string "Before"

        After ->
            Json.Encode.string "After"

        Middle p ->
            Json.Encode.object [ ( "Middle", Json.Encode.float p ) ]


save : Dict Img Split -> Cmd Msg
save dict =
    Http.post
        { url = "save"
        , body = Http.jsonBody <| Json.Encode.dict String.fromInt splitEncoder dict
        , expect = Http.expectString Saved
        }


mouseMoveDecoder : Json.Decode.Decoder Msg
mouseMoveDecoder =
    Json.Decode.field "offsetY" Json.Decode.int
        |> Json.Decode.map (\x -> ChangedPos <| toFloat x * 100 / toFloat imageHeight)
