module Images exposing (..)

import File
import File.Download
import File.Select
import Git
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import List.Extra
import PartsJson
import Task


type alias Percentage =
    Int


type alias Image =
    { split : Maybe Percentage, url : String }


type alias Loadable =
    { folder : Git.Object
    , images : List Image -- sorted list of images
    }


type alias Model =
    { selected : Maybe Loadable
    , selecting : Bool
    }


getImageUrl : Int -> Model -> Maybe String
getImageUrl ind { selected } =
    selected |> Maybe.andThen (.images >> List.Extra.getAt ind) >> Maybe.map .url


clampIndex : Model -> Int -> Int
clampIndex { selected } ind =
    selected
        |> Maybe.map (.images >> List.length)
        |> Maybe.withDefault 0
        |> (\len -> clamp 0 (len-1) ind)


{-| Returns in increasing order of Index
-}
getModifications : Model -> List ( Int, Percentage )
getModifications { selected } =
    selected
        |> Maybe.map .images
        |> Maybe.withDefault []
        |> List.indexedMap (\ind { split } -> ( ind, split ))
        |> List.filterMap (\( ind, split ) -> Maybe.map (\x -> ( ind, x )) split)


modify : ( Int, Percentage ) -> Model -> Model
modify ( ind, split ) model =
    { model
        | selected =
            Maybe.map
                (\selected ->
                    { selected
                        | images =
                            List.Extra.updateAt ind
                                (\img -> { img | split = Just split })
                                selected.images
                    }
                )
                model.selected
    }


type Msg
    = GotDirListing (Maybe (List Git.Object))
    | PathChanged String
    | RepoChanged String
    | New
    | Submit
    | Load
    | Loaded File.File
    | Save
    | GotFile String


rootFile : String -> Loadable
rootFile repo =
    { folder =
        { name = ""
        , url = "https://api.github.com/repos/" ++ repo ++ "/git/trees/master"
        , ft = Git.Root { files = Nothing }
        }
    , images = []
    }


requestDirListing : Model -> ( Model, Cmd Msg )
requestDirListing ({ selected } as model) =
    ( model
    , Maybe.map (.folder >> Git.requestDirListing GotDirListing) selected
        |> Maybe.withDefault Cmd.none
    )


notListed : Git.Object -> Loadable
notListed folder =
    { folder = folder, images = [] }


initJson : ( Model, Cmd Msg )
initJson =
    ( { selected =
            Json.Decode.decodeString loadableDecoder PartsJson.json
                |> Result.toMaybe
      , selecting = False
      }
    , Cmd.none
    )


init : String -> ( Model, Cmd Msg )
init repo =
    { selected = Just <| rootFile repo
    , selecting = False
    }
        |> requestDirListing


refreshLoadable : Maybe (List Git.Object) -> Loadable -> Loadable
refreshLoadable files { folder } =
    { folder = Git.refreshFiles files folder
    , images =
        files
            |> Maybe.withDefault []
            |> List.filter
                (\{ name } ->
                    String.endsWith ".jpg" name || String.endsWith ".png" name
                )
            |> List.sortBy .name
            |> List.map Git.toRawLink
            |> List.map (Image Nothing)
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RepoChanged repo ->
            { model | selected = Just <| rootFile repo }
                |> requestDirListing

        GotDirListing files ->
            ( { model
                | selected = Maybe.map (refreshLoadable files) model.selected
              }
            , Cmd.none
            )

        PathChanged name ->
            if name == ".." then
                model.selected
                    |> Maybe.map .folder
                    |> Maybe.map (\x -> Git.getParent x |> Maybe.withDefault x)
                    |> Maybe.map notListed
                    |> Maybe.map (\l -> { model | selected = Just l } |> requestDirListing)
                    -- Don't refresh without a reason
                    |> Maybe.withDefault ( model, Cmd.none )

            else
                model.selected
                    |> Maybe.map .folder
                    |> Maybe.andThen Git.listDir
                    |> Maybe.andThen (List.Extra.find (\file -> name == file.name))
                    |> Maybe.map notListed
                    |> Maybe.map (\l -> { model | selected = Just l } |> requestDirListing)
                    -- Don't refresh without a reason
                    |> Maybe.withDefault ( model, Cmd.none )

        New ->
            ( { model | selecting = True }, Cmd.none )

        Submit ->
            ( { model | selecting = False }, Cmd.none )

        Load ->
            ( model, File.Select.file [ "application/json" ] Loaded )

        Loaded file ->
            ( model, Task.perform GotFile (File.toString file) )

        Save ->
            ( model
            , case model.selected of
                Just selected ->
                    loadableEncoder selected
                        |> Json.Encode.encode 4
                        |> File.Download.string "parts.json" "application/json"

                Nothing ->
                    Cmd.none
            )

        GotFile content ->
            -- TODO Make decoding error visible in UI
            -- ( Json.Decode.decodeString modelDecoder content |> Result.toMaybe |> Maybe.withDefault model
            ( { model
                | selected =
                    Json.Decode.decodeString loadableDecoder content
                        |> Result.toMaybe
              }
            , Cmd.none
            )


loadableEncoder : Loadable -> Json.Encode.Value
loadableEncoder { folder, images } =
    Json.Encode.object
        [ ( "folder", Git.objectEncoder folder )
        , ( "images", Json.Encode.list imageEncoder images )
        ]


imageEncoder : Image -> Json.Encode.Value
imageEncoder { split, url } =
    Json.Encode.object
        [ ( "split", split |> Maybe.map Json.Encode.int |> Maybe.withDefault Json.Encode.null )
        , ( "url", Json.Encode.string url )
        ]


loadableDecoder : Json.Decode.Decoder Loadable
loadableDecoder =
    Json.Decode.map2 Loadable
        (Json.Decode.field "folder" Git.objectDecoder)
        (Json.Decode.field "images" <| Json.Decode.list imageDecoder)


imageDecoder : Json.Decode.Decoder Image
imageDecoder =
    Json.Decode.map2 Image
        (Json.Decode.field "split" <| Json.Decode.nullable Json.Decode.int)
        (Json.Decode.field "url" Json.Decode.string)


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
                (selected |> Maybe.map (.folder >> Git.subDirs) |> Maybe.withDefault [] |> List.map .name)
                |> List.append [ Html.option [] [ Html.text "." ], Html.option [] [ Html.text ".." ] ]
                |> Html.select [ Html.Events.onInput PathChanged ]
            ]
        , Html.div []
            [ Html.text <|
                "Selected Directory: "
                    ++ (Maybe.map (.folder >> Git.fileRepo) selected |> Maybe.withDefault "noRepo")
                    ++ (Maybe.map (.folder >> Git.pathToFile) selected |> Maybe.withDefault "noPath")
            ]
        , Html.button [ Html.Events.onClick Submit ] [ Html.text "Submit" ]
        ]
