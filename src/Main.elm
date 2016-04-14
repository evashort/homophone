import Effects exposing (Effects)
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes
import Http
import Json.Decode as Json
import Random
import Signal
import String
import Task

import DataLoader
import Repronounce exposing (Respelling(..))
import Respell
import StartApp

app =
  StartApp.start { init = init, update = update, view = view, inputs = [] }

main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks

port title : String
port title = "Homophone Generator"

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
    [ Html.div
        [ Attributes.style
            [ ("margin", "10pt")
            , ("margin-bottom", "0pt")
            , ("font-size", "20pt")
            , ("line-height", "1.25em")
            ]
        ]
        [ Html.text "Homophone Generator" ]
    , Html.div
        [ Attributes.style
            [ ("display", "table")
            , ("width", "1%")
            ]
        ]
        [ Html.div
            [ Events.on "input" targetInnerText <|
                Signal.message address << EditText
            , Attributes.style
                [ ("font-size", "20pt")
                , ("border", "1pt solid")
                , ("padding", "10pt")
                , ("margin", "10pt")
                , ("line-height", "1.25em")
                , ("min-height", "1.25em")
                , ("overflow", "auto")
                , ("width", "20em")
                , ("resize", "horizontal")
                ]
            , Attributes.placeholder "Type some words..."
            , Attributes.contenteditable True
            ]
            []
        , Html.div [ Attributes.hidden True ]
            [ Html.button
                [ Events.onClick address RefreshText
                , Attributes.style
                    [ ("padding", "10px 20px 10px 20px") ]
                ]
                [ Html.text "->" ]
            ]
        , Html.div
            [ Attributes.style
                [ ("font-size", "20pt")
                , ("border", "1pt solid")
                , ("padding", "10pt")
                , ("margin", "10pt")
                , ("line-height", "1.25em")
                , ("min-height", "1.25em")
                , ("overflow", "auto")
                ]
            ]
            [ Html.text model.genText ]
        ]
     , Html.a
         [ Attributes.href
             "https://github.com/evanshort73/homophone/blob/master/LICENSE"
         , Attributes.style [ ("margin", "10pt") ]
         ]
         [ Html.text "License" ]
    , DataLoader.view model.dataLoader
    ]

targetInnerText : Json.Decoder String
targetInnerText =
  Json.at ["target", "innerText"] Json.string

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
      ( { model
        | userText = newUserText
        , genText = model.genText ++ if model.modified then "..." else ""
        , modified = True
        }
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
                    InProgress (text, remainingPhonemes) ->
                      text ++ String.repeat (dotCount remainingPhonemes) "​."
                    Done (text, _) -> text
                    NoSolution -> "no solution"
              , cache = result.cache
              , modified =
                  case result.respelling of
                    InProgress _ -> True
                    _ -> False
              }
            , case result.respelling of
                InProgress _ -> Effects.task <| Task.succeed RespellText
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
                    InProgress _ ->
                      Debug.crash "still in progress after maxInt iterations"
                    Done (text, _) -> text
                    NoSolution -> "no solution"
              , cache = result.cache
              , modified = False
              }
      , Effects.none
      )

dotCount : Int -> Int
dotCount remainingPhonemes =
  max 3 <| round <| 2.33 * toFloat remainingPhonemes
