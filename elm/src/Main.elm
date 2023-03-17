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


type alias Model =
    { percentage : Percentage
    , page : Int
    , pageIncStep : Int
    , modifications : Dict Img Split
    , serverMsg : Maybe String
    }


type Direction
    = Last
    | Next


type Msg
    = ChangedPos Percentage
    | ChangedPageIncStep (Maybe Int)
    | VisitModification Direction
    | VisitPage Direction
    | Modify Split
    | RemoveModification
    | Step
    | Save
    | Saved (Result Http.Error String)
    | Load
    | Loaded (Result Http.Error (Dict Img Split))


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


modificationLine : Model -> Html.Html msg -> Html.Html msg
modificationLine { page, modifications } img =
    Html.div
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

            _ ->
                Html.span [] []
        , img
        ]


viewImage : Model -> Html.Html Msg
viewImage ({ percentage, page, modifications } as model) =
    Html.div
        [ Html.Attributes.style "width" "min-content"
        , Html.Attributes.style "height" "100%"
        ]
        [ splitButton (Dict.get page modifications) "Split Before" Before
        , modificationLine model <|
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
        , splitButton (Dict.get page modifications) "Split After" After
        ]


imageHeight : Int
imageHeight =
    1000


navButton : String -> Msg -> Html.Html Msg
navButton text msg =
    Html.button [ Html.Attributes.style "font-size" "2em", Html.Events.onClick msg ] [ Html.text text ]


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
            [ navButton "Remove Change" RemoveModification
            , navButton "Last Change" <| VisitModification Last
            , navButton "Next Change" <| VisitModification Next
            , navButton "Last Page" <| VisitPage Last
            , navButton "Next Page" <| VisitPage Next
            , navButton "Step" <| Step
            , Html.div []
                [ Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.min "1"
                    , Html.Attributes.max "20"
                    , Html.Attributes.value <| String.fromInt pageIncStep
                    , Html.Events.onInput (String.toInt >> ChangedPageIncStep)
                    ]
                    []
                , Html.span
                    [ Html.Attributes.style "width" "4em"
                    , Html.Attributes.style "font-size" "2em"
                    ]
                    [ Html.text <| String.fromInt pageIncStep ]
                ]
            , Html.div []
                [ navButton "Save" Save
                , navButton "Load" Load
                ]
            , Html.text <| Maybe.withDefault "No messages from server" serverMsg
            ]
        ]


closestModFiltered : (Int -> Bool) -> Model -> Int
closestModFiltered isGood { page, modifications } =
    modifications
        |> Dict.keys
        -- TODO clamp index (total images not known for now)
        -- |> (\l -> (page + pageIncStep) :: l)
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
            -- TODO clip index from above
            ( { model | page = model.page + model.pageIncStep }, Cmd.none )

        VisitPage dir ->
            -- TODO clip index from above
            ( { model | page = max 0 <| model.page + chooseLastNext dir 1 -1 }, Cmd.none )

        VisitModification dir ->
            ( { model | page = closestModFiltered (chooseLastNext dir (>) (<) model.page) model }, Cmd.none )

        Modify percentage ->
            { model
                | modifications = Dict.update model.page (\_ -> Just percentage) model.modifications
            }
                |> update Step

        RemoveModification ->
            ( { model
                | modifications = Dict.remove model.page model.modifications
              }
            , Cmd.none
            )

        Save ->
            ( model, save model.modifications )

        Saved result ->
            ( { model | serverMsg = Result.toMaybe result }, Cmd.none )

        Load ->
            ( model, load )

        Loaded result ->
            Result.toMaybe result
                |> Maybe.map
                    (\m ->
                        ( { model
                            | modifications = m
                            , serverMsg = Just <| "Loaded " ++ String.fromInt (Dict.size m) ++ " modifications"
                          }
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault ( { model | serverMsg = Just "Could not load :P" }, Cmd.none )


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
