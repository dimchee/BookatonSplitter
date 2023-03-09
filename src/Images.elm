module Images exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import List.Extra


type FileType
    = Dir (Maybe (Dict String File))
    | Plain


type alias File =
    { name : String
    , url : String
    , ft : FileType
    }


type alias Repo =
    String


type alias Path =
    -- TODO Should be unempty list (File, List File)
    List File


formatPath : Path -> String
formatPath =
    List.map .name >> List.reverse >> String.join "/"


listDir : File -> Maybe (Dict String File)
listDir file =
    case file.ft of
        Dir x ->
            x

        Plain ->
            Nothing


listPath : Path -> Maybe (Dict String File)
listPath =
    List.head >> Maybe.andThen listDir


type alias Img =
    { name : String
    , mime : String
    , url : Maybe String
    }


type alias Model =
    { repo : Repo
    , curPath : Path
    , selecting : Bool
    }


getImage : Int -> Model -> Maybe String
getImage ind { repo, curPath } =
    Maybe.andThen (Dict.toList >> List.drop ind >> List.head) (listPath curPath)
        |> Maybe.map (Tuple.second >> .name)
        |> Maybe.map (\x -> formatPath curPath ++ "/" ++ x)
        |> Maybe.map (\x -> "https://raw.githubusercontent.com/" ++ repo ++ "/master/" ++ x)


type Msg
    = GotDirListing (Maybe (Dict String File))
    | PathChanged String
    | RepoChanged Repo
    | New
    | Submit


rootFile : Repo -> File
rootFile repo =
    { name = ""
    , url = "https://api.github.com/repos/" ++ repo ++ "/git/trees/master"
    , ft = Dir Nothing
    }


requestDirListing : Path -> Cmd Msg
requestDirListing path =
    case path of
        { url } :: _ ->
            Http.get
                { url = url
                , expect =
                    Json.Decode.field "tree"
                        (Json.Decode.list <|
                            Json.Decode.map3
                                (\name link t ->
                                    File name link <|
                                        if t == "tree" then
                                            Dir Nothing

                                        else
                                            Plain
                                )
                                (Json.Decode.field "path" Json.Decode.string)
                                (Json.Decode.field "url" Json.Decode.string)
                                (Json.Decode.field "type" Json.Decode.string)
                        )
                        |> Json.Decode.map (List.map (\({ name } as file) -> ( name, file )) >> Dict.fromList)
                        |> Http.expectJson
                            (Result.toMaybe >> GotDirListing)
                }

        [] ->
            Cmd.none


addCmd : (Model -> Cmd Msg) -> Model -> ( Model, Cmd Msg )
addCmd f model =
    ( model, f model )


init : Repo -> ( Model, Cmd Msg )
init repo =
    { repo = repo
    , curPath = [ rootFile repo ]
    , selecting = False
    }
        |> (\model -> ( model, requestDirListing model.curPath ))


addFiles : Maybe (Dict String File) -> Path -> Path
addFiles files path =
    List.Extra.uncons path
        |> Maybe.map (Tuple.mapFirst (\file -> { file | ft = Dir files }))
        |> Maybe.map (\( x, y ) -> x :: y)
        |> Maybe.withDefault path


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RepoChanged repo ->
            { model | repo = repo }
                |> addCmd (\{ curPath } -> requestDirListing curPath)

        GotDirListing files ->
            ( { model | curPath = addFiles files model.curPath }
            , Cmd.none
            )

        PathChanged name ->
            if name == ".." then
                { model
                    | curPath =
                        case model.curPath of
                            _ :: ((_ :: _) as xs) ->
                                xs

                            _ ->
                                model.curPath
                }
                    |> addCmd (\{ curPath } -> requestDirListing curPath)

            else
                case Maybe.andThen (Dict.get name) <| listPath model.curPath of
                    Just file ->
                        { model | curPath = file :: model.curPath }
                            |> addCmd (\{ curPath } -> requestDirListing curPath)

                    Nothing ->
                        ( model, Cmd.none )

        New ->
            ( { model | selecting = True }, Cmd.none )

        Submit ->
            ( { model | selecting = False }, Cmd.none )


viewImages : Dict String Img -> Html Msg
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


view : Model -> Html Msg
view ({ selecting } as model) =
    if selecting then
        viewSelectDir model

    else
        Html.div
            []
            [ Html.button [ Html.Events.onClick New ] [ Html.text "New" ]

            -- , Html.button [ Html.Events.onClick Load ] [ Html.text "Load" ]
            -- , Html.button [ Html.Events.onClick Save ] [ Html.text "Save" ]
            ]


viewSelectDir : Model -> Html Msg
viewSelectDir { repo, curPath } =
    Html.div
        []
        [ Html.input [ Html.Events.onInput RepoChanged, Html.Attributes.placeholder "Github Repository" ] []
        , Html.div []
            [ List.map
                (Html.option [] << List.singleton << Html.text)
                (listPath curPath
                    |> Maybe.map Dict.toList
                    |> Maybe.withDefault []
                    |> List.filterMap
                        (\( _, { ft, name } ) ->
                            if ft == Plain then
                                Nothing

                            else
                                Just name
                        )
                )
                |> List.append [ Html.option [] [ Html.text "." ], Html.option [] [ Html.text ".." ] ]
                |> Html.select [ Html.Events.onInput PathChanged ]
            ]
        , Html.div []
            [ Html.text <|
                "Selected Directory: "
                    ++ repo
                    ++ (curPath |> List.map .name |> List.reverse |> String.join "/")
            ]
        , Html.button [ Html.Events.onClick Submit ] [ Html.text "Submit" ]
        ]
