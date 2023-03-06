module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode


type alias Percentage =
    Int


type alias Img =
    { name : String
    , mime : String
    , url : Maybe String
    }


type alias File =
    { name : String
    , url : String
    }


type alias Path =
    List File


type alias Dir =
    { path : Path
    , files : Maybe (Dict String File)
    }


type alias Repo =
    String


rootFile : Repo -> File
rootFile repo =
    { name = "/"
    , url = "https://api.github.com/repos/" ++ repo ++ "/git/trees/master"
    }


type alias Model =
    { percentage : Percentage
    , images : Dict String Img
    , repo : Repo
    , curDir : Dir
    }


type Msg
    = ChangedPos Percentage
    | Split
    | GotUrl Img String
    | RepoChanged String
    | GotDirListing (Maybe (Dict String File))
    | Selected String


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { percentage = 0
      , images = Dict.empty
      , repo = "makccr/wallpapers"
      , curDir = { path = [], files = Nothing }
      }
    , listDir <| rootFile "makccr/wallpapers"
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html.Html Msg
view { percentage, images, curDir } =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        ]
        [ Html.img
            [ Html.Attributes.src <| "https://raw.githubusercontent.com/makccr/wallpapers/master/wallpapers/art/ARTWORK-bridal-procession-on-the-hardangerfjord.jpg"
            , Html.Attributes.height 500
            , Html.Attributes.style "-webkit-mask-image" <| gradient percentage
            , Html.Attributes.style "mask-image" <| gradient percentage
            , Html.Events.on "mousemove" mouseMoveDecoder
            , Html.Events.onClick Split
            , Html.Events.onMouseOut <| ChangedPos 0
            ]
            []

        -- , Html.button [ Html.Events.onClick ImagesRequested ] [ Html.text "Choose Files" ]
        , Html.input [ Html.Events.onInput RepoChanged, Html.Attributes.placeholder "Github Repository" ] []
        , viewImages images
        , Html.div [] [ Html.text <| "Selected Path: " ++ (curDir.path |> List.map .name |> List.reverse |> String.join "/") ]
        , Html.div []
            [ List.map
                (Html.option [] << List.singleton << Html.text)
                (curDir.files |> Maybe.map Dict.keys |> Maybe.withDefault [])
                |> List.append [ Html.option [] [ Html.text "." ], Html.option [] [ Html.text ".." ] ]
                |> Html.select [ Html.Events.onInput Selected ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedPos x ->
            ( { model | percentage = x }, Cmd.none )

        Split ->
            ( model, Cmd.none )

        GotUrl img url ->
            ( { model | images = Dict.insert img.name { img | url = Just url } model.images }
            , Cmd.none
            )

        RepoChanged repo ->
            ( { model | repo = repo }
            , listDir <| rootFile repo
            )

        GotDirListing files ->
            ( { model | curDir = { path = model.curDir.path, files = files } }
            , Cmd.none
            )

        Selected name ->
            if name == ".." then
                let
                    newPath =
                        List.tail model.curDir.path |> Maybe.withDefault [ rootFile model.repo ]
                in
                ( { model
                    | curDir = { path = newPath, files = Nothing }
                  }
                , newPath |> List.head |> Maybe.withDefault (rootFile model.repo) |> listDir
                )

            else
                case Maybe.andThen (Dict.get name) model.curDir.files of
                    Just file ->
                        ( { model
                            | curDir = { path = file :: model.curDir.path, files = Nothing }
                          }
                        , listDir <| file
                        )

                    Nothing ->
                        ( model, Cmd.none )


listDir : File -> Cmd Msg
listDir { url } =
    Http.get
        { url = url
        , expect =
            Json.Decode.field "tree"
                (Json.Decode.list <|
                    Json.Decode.map2 File
                        (Json.Decode.field "path" Json.Decode.string)
                        (Json.Decode.field "url" Json.Decode.string)
                )
                |> Json.Decode.map (List.map (\({ name } as file) -> ( name, file )) >> Dict.fromList)
                |> Http.expectJson
                    (Result.toMaybe >> GotDirListing)
        }


mouseMoveDecoder : Json.Decode.Decoder Msg
mouseMoveDecoder =
    Json.Decode.field "offsetY" Json.Decode.int
        |> Json.Decode.map (\x -> ChangedPos <| round <| toFloat x * 100 / 500)
