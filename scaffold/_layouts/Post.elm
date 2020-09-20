module Post exposing (main, metadataHtml)

import Elmstatic exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (alt, attribute, class, href, src)
import Page


tagsToHtml : List String -> List (Html Never)
tagsToHtml tags =
    let
        tagLink tag =
            "/tags/" ++ String.toLower tag

        linkify tag =
            a [ href <| tagLink tag ] [ text tag ]
    in
    List.map linkify tags


metadataHtml : Elmstatic.Post -> Html Never
metadataHtml post =
    span [ class "post-date" ] [ text post.date ]



-- implement actual feature, shouldn't be hard


related : Html Never
related =
    div [ class "related" ]
        [ h2 []
            [ text "Related Posts" ]
        , ul [ class "related-posts" ]
            [ li []
                [ h3 []
                    [ a [ href "{{ post.url }}" ]
                        [ text "{{ post.title }}"
                        , small [] [ text "{{ post.date | date_to_string }}" ]
                        ]
                    ]
                ]
            ]
        ]


layout : String -> List (Html Never) -> List (Html Never)
layout title contentItems =
    [ div [ class "post" ]
        ([ h1 [ class "post-title" ] [ text title ] ] ++ contentItems ++ [ ])
    ]


main : Elmstatic.Layout
main =
    Elmstatic.layout Elmstatic.decodePost <|
        \content ->
            Ok <|
                layout
                    content.title
                    [ metadataHtml content, Page.markdown content.content ]
