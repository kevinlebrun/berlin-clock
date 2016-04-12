module Main (..) where

import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Time


type alias Seconds =
  Int


type State
  = On
  | Off


type alias Model =
  { top : State
  , line1 : List State
  , line2 : List State
  , line3 : List State
  , line4 : List State
  }


model : Seconds -> Model
model seconds =
  let
    date =
      Date.fromTime (toFloat <| seconds * 1000)

    hours =
      Date.hour date

    minutes =
      Date.minute date
  in
    { top =
        if (seconds // 2) % 2 == 0 then
          On
        else
          Off
    , line1 = List.take 4 <| (List.repeat (hours // 5) On) ++ (List.repeat 4 Off)
    , line2 = List.take 4 <| (List.repeat (hours % 5) On) ++ (List.repeat 4 Off)
    , line3 = List.take 11 <| (List.repeat (minutes // 5) On) ++ (List.repeat 11 Off)
    , line4 = List.take 4 <| (List.repeat (minutes % 5) On) ++ (List.repeat 4 Off)
    }


view : Model -> Html
view model =
  div
    [ class "container" ]
    [ styles
    , div
        [ class "clock" ]
        [ div [ class "top" ] [ light model.top ]
        , div [ class "line1" ] (List.map light model.line1)
        , div [ class "line2" ] (List.map light model.line2)
        , div [ class "line3" ] (List.map light model.line3)
        , div [ class "line4" ] (List.map light model.line4)
        ]
    ]


light : State -> Html
light state =
  div [ classList [ ( "light", True ), ( "light--on", state == On ) ] ] [ text (String.fromChar '\xA0') ]


main : Signal Html
main =
  Signal.map (view << model) tick


tick : Signal Seconds
tick =
  Signal.map (round << Time.inSeconds) <| Time.every Time.second



-- STYLES


styles =
  let
    css =
      """
.container {
    display: flex;
    justify-content: center;
    position: fixed;
    width: 100%;
    height: 100%;
}

.clock {
    display: flex;
    justify-content: center;
    width: 200px;
    flex-direction: column;
}

.top, .line1, .line2, .line3, .line4 {
    display: flex;
    justify-content: center;
}

.line1, .line2, .line3, .line4 {
    border-radius: 5px;
    border: 5px solid #f5f5f5;
    margin-top: 5px;
}

.light {
    flex: 1 0 auto;
    background-color: #f5c579;
    border-left: 5px solid #f8f8ef;
}

.light:first-child {
    border: 0;
    border-radius: 3px 0 0 3px;
}

.light:last-child {
    border-radius: 0 3px 3px 0;
}

.light--on {
    background-color: #fffb3b;
    background: -webkit-radial-gradient(circle cover, #fffb3b 0%, #f5c579 100%)
}

.top .light {
    flex: 0 0 auto;
    height: 40px;
    width: 40px;
    border-radius: 50%;
    border: 5px solid #f5f5f5;
}

.top .light--on {
    background: -webkit-radial-gradient(circle cover, #fffb3b 0%, #f5c579 80%)
}

.line1 .light, .line2 .light, .line3 .light:nth-child(3n+0) {
    background-color: #b8566a;
}

.line1 .light--on, .line2 .light--on, .line3 .light--on:nth-child(3n+0) {
    background-color: #fb5d76;
    background: -webkit-radial-gradient(circle cover, #fb5d76 0%, #98626f 150%)
}
"""
  in
    node "style" [] [ text css ]
