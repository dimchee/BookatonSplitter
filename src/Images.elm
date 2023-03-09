module Images exposing (..)

import Dict exposing (Dict)
import File
import File.Download
import File.Select
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Encode
import List.Extra
import Task


type FileType
    = Dir { parent : File, files : Maybe (List File) }
    | Plain { parent : File }
    | Root { files : Maybe (List File) }


type alias File =
    -- TODO Add sha (changes init, so it will need to parse resulting url and sha)
    -- TODO Maybe dont save url at all (and generate it on fly from sha) so it is less error prone
    { name : String
    , url : String
    , ft : FileType
    }


pathToFile : File -> String
pathToFile { name, ft } =
    case ft of
        Dir { parent } ->
            pathToFile parent ++ "/" ++ name

        Plain { parent } ->
            pathToFile parent ++ "/" ++ name

        Root _ ->
            ""


fileRepo : File -> String
fileRepo { url } =
    String.split "/" url |> List.drop 4 |> List.take 2 |> String.join "/"


listDir : File -> Maybe (List File)
listDir file =
    case file.ft of
        Dir { files } ->
            files

        Root { files } ->
            files

        Plain _ ->
            Nothing


type alias Model =
    { selected : File
    , selecting : Bool
    }


getImageUrl : Int -> Model -> Maybe String
getImageUrl ind { selected } =
    listDir selected
        |> Maybe.map (List.filter (\{ name } -> String.endsWith ".jpg" name || String.endsWith ".png" name))
        |> Maybe.map (List.sortBy .name)
        |> Maybe.andThen (List.Extra.getAt ind)
        |> Maybe.map (\x -> pathToFile selected ++ "/" ++ x.name)
        |> Maybe.map (\x -> "https://raw.githubusercontent.com/" ++ fileRepo selected ++ "/609906c5de7c5c201f8263f620487e4330a5d188/" ++ x)


type Msg
    = GotDirListing (Maybe (List File))
    | PathChanged String
    | RepoChanged String
    | New
    | Submit
    | Load
    | Loaded File.File
    | Save
    | GotFile String


rootFile : String -> File
rootFile repo =
    { name = ""
    , url = "https://api.github.com/repos/" ++ repo ++ "/git/trees/master"
    , ft = Root { files = Nothing }
    }


requestDirListing : File -> Cmd Msg
requestDirListing ({ url } as file) =
    Http.get
        { url = url
        , expect =
            Json.Decode.field "url" Json.Decode.string
                |> Json.Decode.andThen
                    (\newUrl ->
                        Json.Decode.field "tree"
                            (Json.Decode.list <|
                                Json.Decode.map3
                                    (\name link t ->
                                        File name link <|
                                            if t == "tree" then
                                                Dir { parent = { file | url = newUrl }, files = Nothing }

                                            else
                                                Plain { parent = { file | url = newUrl } }
                                    )
                                    (Json.Decode.field "path" Json.Decode.string)
                                    (Json.Decode.field "url" Json.Decode.string)
                                    (Json.Decode.field "type" Json.Decode.string)
                            )
                    )
                |> Http.expectJson
                    (Result.toMaybe >> GotDirListing)
        }


addCmd : (Model -> Cmd Msg) -> Model -> ( Model, Cmd Msg )
addCmd f model =
    ( model, f model )


init : String -> ( Model, Cmd Msg )
init repo =
    { selected = rootFile repo
    , selecting = False
    }
        |> addCmd (\{ selected } -> requestDirListing selected)


addFiles : Maybe (List File) -> File -> File
addFiles files file =
    case file.ft of
        Dir x ->
            { file | ft = Dir { x | files = files } }

        Root x ->
            { file | ft = Root { x | files = files } }

        Plain _ ->
            file


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RepoChanged repo ->
            { model | selected = rootFile repo }
                |> addCmd (\{ selected } -> requestDirListing selected)

        GotDirListing files ->
            ( { model | selected = addFiles files model.selected }
            , Cmd.none
            )

        PathChanged name ->
            if name == ".." then
                { model
                    | selected =
                        case model.selected.ft of
                            Plain { parent } ->
                                parent

                            Dir { parent } ->
                                parent

                            Root _ ->
                                model.selected
                }
                    |> addCmd (\{ selected } -> requestDirListing selected)

            else
                case Maybe.andThen (List.Extra.find (\file -> name == file.name)) <| listDir model.selected of
                    Just file ->
                        { model | selected = file }
                            |> addCmd (\{ selected } -> requestDirListing selected)

                    Nothing ->
                        ( model, Cmd.none )

        New ->
            ( { model | selecting = True }, Cmd.none )

        Submit ->
            ( { model | selecting = False }, Cmd.none )

        Load ->
            ( model, File.Select.file [ "application/json" ] Loaded )

        Loaded file ->
            ( model, Task.perform GotFile (File.toString file) )

        Save ->
            ( model, File.Download.string "parts.json" "application/json" <| Json.Encode.encode 4 <| fileEncoder model.selected )

        GotFile content ->
            -- TODO Make decoding error visible in UI
            -- ( Json.Decode.decodeString modelDecoder content |> Result.toMaybe |> Maybe.withDefault model
            ( { model
                | selected =
                    Json.Decode.decodeString fileDecoder content
                        |> Result.toMaybe
                        |> Maybe.withDefault model.selected
              }
            , Cmd.none
            )


fileEncoder : File -> Json.Encode.Value
fileEncoder file =
    Json.Encode.object
        [ ( "name", Json.Encode.string file.name )
        , ( "url", Json.Encode.string file.url )
        , ( "ft"
          , Json.Encode.object <|
                case file.ft of
                    Dir { parent, files } ->
                        [ ( "tag", Json.Encode.string "dir" )
                        , ( "parent", fileEncoder parent )
                        , ( "files"
                          , files
                                |> Maybe.map (Json.Encode.list fileEncoder)
                                |> Maybe.withDefault Json.Encode.null
                          )
                        ]

                    Root { files } ->
                        [ ( "tag", Json.Encode.string "root" )
                        , ( "files"
                          , Maybe.map (Json.Encode.list fileEncoder) files
                                |> Maybe.withDefault Json.Encode.null
                          )
                        ]

                    Plain { parent } ->
                        [ ( "tag", Json.Encode.string "plain" )
                        , ( "parent", fileEncoder parent )
                        ]
          )
        ]


fileDecoder : Json.Decode.Decoder File
fileDecoder =
    Json.Decode.map3 File
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "url" Json.Decode.string)
        (Json.Decode.field "tag" Json.Decode.string
            |> Json.Decode.andThen
                (\tag ->
                    case tag of
                        "dir" ->
                            Json.Decode.map2 (\parent files -> Dir { parent = parent, files = files })
                                (Json.Decode.field "parent" <| Json.Decode.lazy (\_ -> fileDecoder))
                                (Json.Decode.field "files" <|
                                    Json.Decode.lazy
                                        (\_ ->
                                            Json.Decode.nullable <| Json.Decode.list fileDecoder
                                        )
                                )

                        "root" ->
                            Json.Decode.map (\files -> Root { files = files })
                                (Json.Decode.field "files" <|
                                    Json.Decode.lazy
                                        (\_ ->
                                            Json.Decode.nullable <| Json.Decode.list fileDecoder
                                        )
                                )

                        "plain" ->
                            Json.Decode.map (\parent -> Plain { parent = parent })
                                (Json.Decode.field "parent" <| Json.Decode.lazy (\_ -> fileDecoder))

                        _ ->
                            Json.Decode.fail "invalid `ft` tag"
                )
            |> Json.Decode.field "ft"
        )


view : Model -> Html Msg
view ({ selecting } as model) =
    if selecting then
        viewSelectDir model

    else
        Html.div
            []
            [ Html.button [ Html.Events.onClick New ] [ Html.text "New" ]
            , Html.button [ Html.Events.onClick Load ] [ Html.text "Load" ]
            , Html.button [ Html.Events.onClick Save ] [ Html.text "Save" ]
            ]


viewSelectDir : Model -> Html Msg
viewSelectDir { selected } =
    Html.div
        []
        [ Html.input [ Html.Events.onInput RepoChanged, Html.Attributes.placeholder "Github Repository" ] []
        , Html.div []
            [ List.map
                (Html.option [] << List.singleton << Html.text)
                (listDir selected
                    |> Maybe.withDefault []
                    |> List.filterMap
                        (\{ ft, name } ->
                            case ft of
                                Plain _ ->
                                    Nothing

                                _ ->
                                    Just name
                        )
                )
                |> List.append [ Html.option [] [ Html.text "." ], Html.option [] [ Html.text ".." ] ]
                |> Html.select [ Html.Events.onInput PathChanged ]
            ]
        , Html.div []
            [ Html.text <|
                "Selected Directory: "
                    ++ fileRepo selected
                    ++ pathToFile selected
            ]
        , Html.button [ Html.Events.onClick Submit ] [ Html.text "Submit" ]
        ]
