module Embeds exposing (..)

import Html exposing (Html, Attribute, iframe, div, a, text)
import Html.Attributes exposing (attribute, class, href, width, height, src, title, target, style)
import Json.Encode as Encode

scStyle : String
scStyle = 
  "font-size: 10px; color: #cccccc;line-break: anywhere;word-break: normal;overflow: hidden;white-space: nowrap;text-overflow: ellipsis; font-family: Interstate,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Garuda,Verdana,Tahoma,sans-serif;font-weight: 100;"

scrolling : String -> Attribute msg 
scrolling val =
  attribute "scrolling" val

frameborder : String -> Attribute msg
frameborder val =
  attribute "frameborder" val

allow : String -> Attribute msg
allow val =
  attribute "allow" val


widthCustom : String -> Attribute msg
widthCustom val =
  attribute "width" val

styleCss : String -> Attribute msg
styleCss val =
  attribute "style" val

scEmbedHeight : Int
scEmbedHeight = 
  166

soundcloud : Html msg
soundcloud =
 let
  s = "https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/tracks/1208128471&color=%23ef292a&auto_play=false&hide_related=false&show_comments=true&show_user=true&show_reposts=false&show_teaser=true"
  sample = "https://soundcloud.com/nil-arity/synthuary-1-minimal"
 in 
  div [class "sc" ] 
    [ iframe [ widthCustom "100%", height scEmbedHeight, scrolling "no", frameborder "no", allow "autoplay", src s] []
    , div [ styleCss scStyle ] 
        [ a [ href "https://soundcloud.com/synthonysound", title "Synthony Sound", target "_blank", styleCss "color: #cccccc: text-decoration: none;"]
          [ text "Synthony Sound", text " ."]
        , a [ href sample, title "synthuary 1 - minimal", target "_blank", styleCss "color: #cccccc; text-decoration: none;" ] 
          [ text "Sample - Kick Drum" ]
        ]
    ]


main = Html.text ""
