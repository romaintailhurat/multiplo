import Html exposing (..)
import Html.Events exposing (..)
import Random
import Tuple exposing (first, second)

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
    [ div []
      [ text ( "DEBUG" ++ (toString model) ) ]
    , div []
      [ button [ onClick Gen ] [ text "Nouvelle multiplication" ] ]
    , div []
      [ text (displayMult model) ]
    , div []
      [ button [ onClick ShowRes ] [ text "Montrer résultat"]]
    ]

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }
