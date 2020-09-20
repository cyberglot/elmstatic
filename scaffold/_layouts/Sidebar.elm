module Sidebar exposing (html)

import Html exposing (..)
import Html.Attributes as Attr exposing (alt, attribute, class, href, src)
import List


html : String -> String -> String -> String -> String -> List ( String, String ) -> Html Never
html titl link desc copyright version lnks =
    div [ class "sidebar" ]
        [ div [ class "container sidebar-sticky" ]
            [ div [ class "sidebar-about" ]
                [ h1 []
                    [ a [ href link ]
                        [ text titl ]
                    ]
                , p [ class "lead" ]
                    [ text desc ]
                ]
            , nav [ class "sidebar-nav" ]
                ([ a [ class "sidebar-nav-item", href link ]
                    [ text "Home" ]
                 ]
                    ++ List.map links lnks
                    ++ [ span [ class "sidebar-nav-item" ]
                            [ text <| "Currently v" ++ version ]
                       ]
                )
            , p []
                [ text <| copyright ++ ". All rights reserved." ]
            ]
        ]


links : ( String, String ) -> Html Never
links ( titl, url ) =
    a [ class "sidebar-nav-item", href url ]
        [ text titl ]
