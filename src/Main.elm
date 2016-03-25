import Effects exposing (Effects)
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes
import Http
import Signal
import Task

import DataLoader
import Respell
import StartApp

app =
  StartApp.start { init = init, update = update, view = view, inputs = [] }

main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks

type alias Model =
  { dataLoader : DataLoader.Model
  , userText : String
  , genText : String
  , cache : Respell.Cache
  }

init : (Model, Effects Action)
init =
  ( { dataLoader = fst DataLoader.init
    , userText = ""
    , genText = ""
    , cache = Respell.emptyCache
    }
  , Effects.map DataLoaded <| snd DataLoader.init
  )

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div []
    [ Html.div []
        [ Html.a
            [ Attributes.href
                "https://github.com/evanshort73/homophone/blob/master/LICENSE"
            ]
            [ Html.text "License" ]
        ]
    , Html.div []
        [ Html.textarea
            [ Events.on "input" Events.targetValue <|
                \x -> Signal.message address (EditText x)
            , Attributes.style
                [ ("width", "400px"), ("height", "200px") ]
            ]
            []
        ]
    , Html.div []
        [ Html.button
            [ Events.onClick address RespellText
            , Attributes.style
                [ ("padding", "10px 20px 10px 20px") ]
            ]
            [ Html.text "->" ]
        ]
    , Html.div []
        [ Html.text model.genText ]
    , Html.div []
        [ DataLoader.view model.dataLoader ]
    ]

type Action
  = EditText String
  | RespellText
  | DataLoaded DataLoader.Action
update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    DataLoaded subAction ->
      let subUpdate = DataLoader.update subAction model.dataLoader in
        ( { model | dataLoader = fst subUpdate }
        , Effects.map DataLoaded <| snd subUpdate
        )
    EditText newUserText ->
      ( case model.dataLoader of
          DataLoader.NotLoaded _ ->
            { model | userText = newUserText, genText = "not loaded" }
          DataLoader.Loaded data ->
            let
              result =
                Respell.respell data model.cache newUserText
            in
              { model
              | userText = newUserText
              , genText =
                  Maybe.withDefault
                    "no solution" <|
                    Maybe.map fst result.respelling
              , cache = result.cache
              }
      , Effects.none
      )
    RespellText ->
      ( case model.dataLoader of
          DataLoader.NotLoaded _ -> { model | genText = "not loaded" }
          DataLoader.Loaded data ->
            let
              result =
                Respell.respell data Respell.emptyCache model.userText
            in
              { model
              | genText =
                  Maybe.withDefault
                    "no solution" <|
                    Maybe.map fst result.respelling
              , cache = result.cache
              }
      , Effects.none
      )
