module Elmstatic exposing
    ( Content
    , Format(..)
    , Layout
    , Page
    , Post
    , PostList
    , decodePage
    , decodePost
    , decodePostList
    , htmlTemplate
    , inlineScript
    , layout
    , script
    , stylesheet
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json
import Json.Decode.Extra exposing (andMap)
import Sidebar
import Styles


type Format
    = Markdown
    | ElmMarkup


type alias Post =
    { content : String
    , date : String
    , format : Format
    , link : String
    , section : String
    , tags : List String
    , title : String
    , copyright : String
    , description : String
    , siteUrl : String
    , siteTitle : String
    , siteName : String
    }


type alias Page =
    { content : String
    , format : Format
    , title : String
    , copyright : String
    , description : String
    , siteUrl : String
    , siteTitle : String
    , siteName : String
    }


type alias PostList =
    { posts : List Post
    , section : String
    , title : String
    , copyright : String
    , description : String
    , siteUrl : String
    , siteTitle : String
    , siteName : String
    }


type alias Content a =
    { a
        | copyright : String
        , description : String
        , siteUrl : String
        , siteTitle : String
        , siteName : String
    }


type alias Layout =
    Program Json.Value Json.Value Never


version : String
version =
    "1.0.0"


pageLinks : List ( String, String )
pageLinks =
    [ ( "About", "/about" ), ( "Contact", "/contact" ) ]


{-| For backward compatibility, look for the content either in `markdown` or `content` fields
-}
decodeContent : Json.Decoder String
decodeContent =
    Json.oneOf [ Json.field "markdown" Json.string, Json.field "content" Json.string ]


decodeFormat : Json.Decoder Format
decodeFormat =
    Json.oneOf
        [ Json.map
            (\format ->
                if format == "emu" then
                    ElmMarkup

                else
                    Markdown
            )
          <|
            Json.field "format" Json.string
        , Json.succeed Markdown
        ]


decodePage : Json.Decoder Page
decodePage =
    Json.map8 Page
        decodeContent
        decodeFormat
        (Json.field "title" Json.string)
        (Json.field "copyright" Json.string)
        (Json.field "description" Json.string)
        (Json.field "siteUrl" Json.string)
        (Json.field "siteTitle" Json.string)
        (Json.field "siteName" Json.string)


decodePost : Json.Decoder Post
decodePost =
    Json.succeed Post
        |> andMap decodeContent
        |> andMap (Json.field "date" Json.string)
        |> andMap decodeFormat
        |> andMap (Json.field "link" Json.string)
        |> andMap (Json.field "section" Json.string)
        |> andMap (Json.field "tags" <| Json.list Json.string)
        |> andMap (Json.field "title" Json.string)
        |> andMap (Json.field "copyright" Json.string)
        |> andMap (Json.field "description" Json.string)
        |> andMap (Json.field "siteUrl" Json.string)
        |> andMap (Json.field "siteTitle" Json.string)
        |> andMap (Json.field "siteName" Json.string)


decodePostList : Json.Decoder PostList
decodePostList =
    Json.succeed PostList
        |> andMap (Json.field "posts" <| Json.list decodePost)
        |> andMap (Json.field "section" Json.string)
        |> andMap (Json.field "title" Json.string)
        |> andMap (Json.field "copyright" Json.string)
        |> andMap (Json.field "description" Json.string)
        |> andMap (Json.field "siteUrl" Json.string)
        |> andMap (Json.field "siteTitle" Json.string)
        |> andMap (Json.field "siteName" Json.string)


script : String -> Html Never
script src =
    node "citatsmle-script" [ attribute "src" src ] []


inlineScript : String -> Html Never
inlineScript js =
    node "citatsmle-script" [] [ text js ]


stylesheet : String -> Html Never
stylesheet href =
    node "link" [ attribute "href" href, attribute "rel" "stylesheet", attribute "type" "text/css" ] []


htmlTemplate : Content contents -> List (Html Never) -> Html Never
htmlTemplate content contentNodes =
    node "html"
        []
        [ node "head"
            []
            [ node "title" [] [ text content.siteTitle ]
            , node "meta" [ attribute "http-equiv" "content-type", attribute "content" "text/html; charset=utf-8" ] []
            , node "meta" [ attribute "viewport" "width=device-width, initial-scale=1.0, maximum-scale=1" ] []
            , node "link" [ attribute "href" "http://gmpg.org/xfn/11", attribute "rel" "profile" ] []
            , stylesheet "/css/poole.css"
            , stylesheet "/css/syntax.css"
            , stylesheet "/css/hyde.css"
            , stylesheet "http://fonts.googleapis.com/css?family=PT+Sans:400,400italic,700|Abril+Fatface"
            , node "link" [ attribute "rel" "apple-touch-icon-precomposed", attribute "sizes" "144x144", attribute "href" "/apple-touch-icon-144-precomposed.png" ] []
            , node "link" [ attribute "rel" "shortcut icon", attribute "href" "/favicon.ico" ] []
            , node "link" [ attribute "rel" "alternate", attribute "type" "application/rss+xml", attribute "title" "RSS", attribute "href" "/atom.xml" ] []
            , Styles.styles
            ]
        , node "body"
            []
            [ Sidebar.html content.siteName content.siteUrl content.description content.copyright version pageLinks
            , div [ class "content container" ] contentNodes
            ]
        ]


layout : Json.Decoder (Content content) -> (Content content -> Result String (List (Html Never))) -> Layout
layout decoder view =
    Browser.document
        { init = \contentJson -> ( contentJson, Cmd.none )
        , view =
            \contentJson ->
                case Json.decodeValue decoder contentJson of
                    Err error ->
                        { title = "error"
                        , body = [ Html.div [ attribute "error" <| Json.errorToString error ] [] ]
                        }

                    Ok content ->
                        case view content of
                            Err viewError ->
                                { title = "error"
                                , body = [ Html.div [ attribute "error" viewError ] [] ]
                                }

                            Ok viewHtml ->
                                { title = ""
                                , body = [ htmlTemplate content <| viewHtml ]
                                }
        , update = \msg contentJson -> ( contentJson, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
