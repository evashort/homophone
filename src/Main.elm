import Effects exposing (Effects)
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes
import Http
import Random
import Signal
import Task

import DataLoader
import Repronounce exposing (Respelling(..))
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
  , modified : Bool
  }

init : (Model, Effects Action)
init =
  ( { dataLoader = fst DataLoader.init
    , userText = ""
    , genText = ""
    , cache = Respell.emptyCache
    , modified = False
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
            [ Events.onClick address RefreshText
            , Attributes.style
                [ ("padding", "10px 20px 10px 20px") ]
            ]
            [ Html.text "->" ]
        ]
    , Html.div []
        [ Html.text <| model.genText ++ if model.modified then "..." else "" ]
    , Html.div []
        [ DataLoader.view model.dataLoader ]
    ]

type Action
  = EditText String
  | RespellText
  | RefreshText
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
      ( { model | userText = newUserText, modified = True }
      , if model.modified then Effects.none
        else Effects.task <| Task.succeed RespellText
      )
    RespellText ->
      case model.dataLoader of
        DataLoader.NotLoaded _ ->
          ( { model | genText = "not loaded" }
          , Effects.none
          )
        DataLoader.Loaded data ->
          let
            result = Respell.respell data model.cache model.userText 1
          in
            ( { model
              | genText =
                  case result.respelling of
                    InProgress -> model.genText
                    Done (text, _) -> text
                    NoSolution -> "no solution"
              , cache = result.cache
              , modified = result.respelling == InProgress
              }
            , case result.respelling of
                InProgress -> Effects.task <| Task.succeed RespellText
                _ -> Effects.none
            )
    RefreshText ->
      ( case model.dataLoader of
          DataLoader.NotLoaded _ -> { model | genText = "not loaded" }
          DataLoader.Loaded data ->
            let
              result =
                Respell.respell
                  data Respell.emptyCache model.userText Random.maxInt
            in
              { model
              | genText =
                  case result.respelling of
                    InProgress ->
                      Debug.crash "still in progress after maxInt iterations"
                    Done (text, _) -> text
                    NoSolution -> "no solution"
              , cache = result.cache
              , modified = False
              }
      , Effects.none
      )
