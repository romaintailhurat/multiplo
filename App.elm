import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Tuple exposing (first, second)

-- CSS

stylesheet link =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      link
            ]
        children = []
    in node tag attrs children

-- Number

randomPair : Random.Generator (Int, Int)
randomPair =
    Random.pair (Random.int 1 9) (Random.int 1 9)

multiplication : (Int, Int) -> Int
multiplication pair = first pair *  second pair

-- Display

displayMult : Model -> String
displayMult model =
  (toString model.x)
  ++ "x"
  ++ (toString model.y)
  ++ "="
  ++ if model.visibility == True then (toString model.res) else "?"

-- Model

type alias Model =
  { x : Int
  , y : Int
  , res : Int
  , visibility : Bool}

init : (Model, Cmd Msg)
init =
  (Model 1 1 1 False, Cmd.none)

-- Update

type Msg =
  Gen
  | New (Int, Int)
  | ShowRes

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Gen ->
      (model, Random.generate New randomPair)
    New pair ->
      (Model (first pair) (second pair) (multiplication pair) False, Cmd.none)
    ShowRes ->
      ({ model | visibility = True }, Cmd.none)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- Views

view : Model -> Html Msg
view model =
  div []
    [ stylesheet "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.2.3/css/bulma.min.css"
    , div [ class "container" ]
      [ div [ style [("font-size", "20em")] ]
        [ text (displayMult model) ]
      , div []
        [ button [ class "button is-primary", onClick Gen ]
          [ text "Nouvelle multiplication" ]
        , button [ class "button is-primary", onClick ShowRes ]
          [ text "Montrer résultat"]]
      , div []
        [ a [ href "https://github.com/romaintailhurat/multiplo" ]
          [ text "[ source code ]"] ]
      ]
    ]

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }
