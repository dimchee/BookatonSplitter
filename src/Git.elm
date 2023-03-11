module Git exposing (..)

import Http
import Json.Decode
import Json.Encode


type ObjType
    = Tree { parent : Object, files : Maybe (List Object) }
    | Blob { parent : Object }
    | Root { files : Maybe (List Object) }


type alias Object =
    -- TODO Add sha (changes init, so it will need to parse resulting url and sha)
    -- TODO Maybe dont save url at all (and generate it on fly from sha) so it is less error prone
    { name : String
    , url : String
    , ft : ObjType
    }


isBlob : Object -> Bool
isBlob { ft } =
    case ft of
        Blob _ ->
            True

        _ ->
            False

isTree : Object -> Bool
isTree = isBlob >> not

subDirs : Object -> List Object
subDirs = listDir >> Maybe.withDefault [] >> List.filter isTree

pathToFile : Object -> String
pathToFile { name, ft } =
    case ft of
        Tree { parent } ->
            pathToFile parent ++ "/" ++ name

        Blob { parent } ->
            pathToFile parent ++ "/" ++ name

        Root _ ->
            ""


fileRepo : Object -> String
fileRepo { url } =
    String.split "/" url |> List.drop 4 |> List.take 2 |> String.join "/"


listDir : Object -> Maybe (List Object)
listDir file =
    case file.ft of
        Tree { files } ->
            files

        Root { files } ->
            files

        Blob _ ->
            Nothing


getParent : Object -> Maybe Object
getParent file =
    case file.ft of
        Blob { parent } ->
            Just <| parent

        Tree { parent } ->
            Just <| parent

        Root _ ->
            Nothing

toRawLink : Object -> String
toRawLink file =
    "https://raw.githubusercontent.com/"
        ++ fileRepo file
        ++ "/609906c5de7c5c201f8263f620487e4330a5d188/"
        ++ pathToFile file

-- TODO Remote data should be requested at most once
requestDirListing : (Maybe (List Object) -> msg) -> Object -> Cmd msg
requestDirListing toMsg ({ url } as file) =
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
                                        Object name link <|
                                            if t == "tree" then
                                                Tree { parent = { file | url = newUrl }, files = Nothing }

                                            else
                                                Blob { parent = { file | url = newUrl } }
                                    )
                                    (Json.Decode.field "path" Json.Decode.string)
                                    (Json.Decode.field "url" Json.Decode.string)
                                    (Json.Decode.field "type" Json.Decode.string)
                            )
                    )
                |> Http.expectJson
                    (Result.toMaybe >> toMsg)
        }


refreshFiles : Maybe (List Object) -> Object -> Object
refreshFiles files file =
    case file.ft of
        Tree x ->
            { file | ft = Tree { x | files = files } }

        Root x ->
            { file | ft = Root { x | files = files } }

        Blob _ ->
            file


objectEncoder : Object -> Json.Encode.Value
objectEncoder file =
    Json.Encode.object
        [ ( "name", Json.Encode.string file.name )
        , ( "url", Json.Encode.string file.url )
        , ( "ft"
          , Json.Encode.object <|
                case file.ft of
                    Tree { parent, files } ->
                        [ ( "tag", Json.Encode.string "dir" )
                        , ( "parent", objectEncoder parent )
                        , ( "files"
                          , files
                                |> Maybe.map (Json.Encode.list objectEncoder)
                                |> Maybe.withDefault Json.Encode.null
                          )
                        ]

                    Root { files } ->
                        [ ( "tag", Json.Encode.string "root" )
                        , ( "files"
                          , Maybe.map (Json.Encode.list objectEncoder) files
                                |> Maybe.withDefault Json.Encode.null
                          )
                        ]

                    Blob { parent } ->
                        [ ( "tag", Json.Encode.string "plain" )
                        , ( "parent", objectEncoder parent )
                        ]
          )
        ]


objectDecoder : Json.Decode.Decoder Object
objectDecoder =
    Json.Decode.map3 Object
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "url" Json.Decode.string)
        (Json.Decode.field "tag" Json.Decode.string
            |> Json.Decode.andThen
                (\tag ->
                    case tag of
                        "dir" ->
                            Json.Decode.map2 (\parent files -> Tree { parent = parent, files = files })
                                (Json.Decode.field "parent" <| Json.Decode.lazy (\_ -> objectDecoder))
                                (Json.Decode.field "files" <|
                                    Json.Decode.lazy
                                        (\_ ->
                                            Json.Decode.nullable <| Json.Decode.list objectDecoder
                                        )
                                )

                        "root" ->
                            Json.Decode.map (\files -> Root { files = files })
                                (Json.Decode.field "files" <|
                                    Json.Decode.lazy
                                        (\_ ->
                                            Json.Decode.nullable <| Json.Decode.list objectDecoder
                                        )
                                )

                        "plain" ->
                            Json.Decode.map (\parent -> Blob { parent = parent })
                                (Json.Decode.field "parent" <| Json.Decode.lazy (\_ -> objectDecoder))

                        _ ->
                            Json.Decode.fail "invalid `ft` tag"
                )
            |> Json.Decode.field "ft"
        )
