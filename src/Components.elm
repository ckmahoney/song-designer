module Components exposing (card)

import Browser
import Html exposing (Html, div, figure, img, p, a, br, time, text)
import Html.Attributes exposing (class, src, href, alt, datetime)

svg : String -> Html a
svg name =
  img [src "/svg/bell.svg"] []

cardImage : Html a
cardImage = 
  div [class "card-image"]
    [ figure [class "image is-4by3"]
       [ img [src "https://bulma.io/images/placeholders/1280x960.png", alt "Placeholder Image"] [] ] ]


cardContent : String -> String-> Html a
cardContent title content = 
  div [class "card-content"]
    [ div [class "media"]
      [ div [class "media-left"]
         [ figure [class "image is-48x48"]
           [ svg "bell" ] ]
         , div [class "media-content"]
             [ p [class "title is-4"] [text title]
             , p [class "subtitle is-6"] [text title] ] ]
      , div [class "content"] [text content] ]


card : String -> Html a
card title = 
  div [class "card"]
    [ cardImage
    , cardContent title "the content"]


main =
  Html.text ""
