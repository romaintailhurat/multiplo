import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Tuple exposing (first, second)

-- CSS
stylesheet : String -> Html msg
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

type Page =
  HomePage | MultPage

type alias Model =
  { x : Int
  , y : Int
  , res : Int
  , visibility : Bool
  , page : Page}

init : (Model, Cmd Msg)
init =
  (Model 1 1 1 False HomePage, Cmd.none)

-- Update

type Msg =
  Gen
  | New (Int, Int)
  | ShowRes
  | Home
  | ShowMult

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Gen ->
      (model, Random.generate New randomPair)
    New pair ->
      (Model (first pair) (second pair) (multiplication pair) False MultPage, Cmd.none)
    ShowRes ->
      ({ model | visibility = True }, Cmd.none)
    Home ->
      ({ model | page = HomePage }, Cmd.none)
    ShowMult ->
      ({ model | page = MultPage }, Cmd.none)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- Views

homeView : Model -> Html Msg
homeView model =
  div []
    [ stylesheet "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.2.3/css/bulma.min.css"
    , div [ class "container" ]
      [ text "Hello", button [ class "button is-primary", onClick ShowMult ] [ text "Mult ?"]]
    ]

multView : Model -> Html Msg
multView model =
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

view : Model -> Html Msg
view model =
  case model.page of
    HomePage ->
      homeView model
    MultPage ->
      multView model

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }
