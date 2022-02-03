module Embeds exposing (..)

import Html exposing (Html, Attribute, iframe, div, a, text)
import Types exposing (SynthRole(..))
import Html.Attributes exposing (attribute, class, href, width, height, src, title, target, style)
import Json.Encode as Encode


sampleLink : SynthRole -> String
sampleLink role =
  let 
    base = "https://soundcloud.com/synthonysound/sample-"
    end = case role of 
      Bass -> "bassline"
      Chords -> "chords"
      Melody -> "melody"
      Hat -> "hats"
      Kick -> "kick"
      Perc -> "percussion"
  in 
  base ++ end


sampleId : SynthRole -> String
sampleId role =
  String.fromInt <| case role of 
      Bass -> 1209348034
      Chords -> 1209348019
      Melody -> 1209347977
      Hat -> 1209348001
      Kick -> 1209347995
      Perc -> 1209347965

sampleEmbedUrl : SynthRole -> String
sampleEmbedUrl role =
  "https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/tracks/" 
  ++ sampleId role
  ++ "&color=%23ef292a&auto_play=false&hide_related=false&show_comments=true&show_user=true&show_reposts=false&show_teaser=true"


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


soundcloud : SynthRole -> Html msg
soundcloud role =
  div [class "sc" ] 
    [ iframe [ widthCustom "100%", height scEmbedHeight, scrolling "no", frameborder "no", allow "autoplay", src (sampleEmbedUrl role)] []
    , div [ styleCss scStyle ] 
        [ a [ href "https://soundcloud.com/synthonysound", title "Synthony Sound", target "_blank", styleCss "color: #cccccc: text-decoration: none;"]
          [ text "Synthony Sound", text " ."]
        , a [ href (sampleLink role), title "synthuary 1 - minimal", target "_blank", styleCss "color: #cccccc; text-decoration: none;" ] 
          [ text "Sample - Kick Drum" ]
        ]
    ]


main = Html.text ""
