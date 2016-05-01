import Char
import Effects exposing (Effects)
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes
import Http
import Random
import Signal
import String
import Task

import DataLoader
import Respell exposing (TextUnit, Status(..))
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
  , userText : List TextUnit
  , genText : String
  , cache : Respell.Cache
  , modified : Bool
  , hidden : Bool
  }

init : (Model, Effects Action)
init =
  ( { dataLoader = fst DataLoader.init
    , userText = []
    , genText = ""
    , cache = Respell.emptyCache
    , modified = False
    , hidden = False
    }
  , Effects.map DataLoaded <| snd DataLoader.init
  )

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div []
    [ Html.div
        [ Attributes.style
            [ ("font-size", "20pt")
            , ("line-height", "1.25em")
            , ("margin", "10pt")
            ]
        ]
        [ Html.text "Homophone Generator" ]
     , Html.a
         [ Attributes.href "#"
         , Events.onClick
             address <|
             if model.hidden then ShowInput else HideInput
         , Attributes.style [ ("margin", "10pt") ]
         ]
         [ Html.text <| if model.hidden then "Show input" else "Hide input" ]
    , Html.div
        [ Attributes.style
            [ ("display", "table")
            , ("width", "1%")
            ]
        ]
        [ Html.div
          [ Attributes.style
              [ ("font-size", "20pt")
              , ("line-height", "1.25em")
              , ("min-height", "20pt")
              , ("border", "1pt solid")
              , ("border-radius", "3pt")
              , ("margin", "10pt")
              , ("position", "relative")
              ]
          ]
          [ Html.textarea
              [ Events.on "input" Events.targetValue <|
                  Signal.message address << EditText
              , Attributes.style <|
                  [ ("font-size", "inherit")
                  , ("font-family", "inherit")
                  , ("line-height", "inherit")
                  , ("width", "100%")
                  , ("height", "100%")
                  , ("padding", "10pt")
                  , ("border", "none")
                  , ("margin", "0")
                  , ("position", "absolute")
                  , ("resize", "none")
                  , ("overflow", "hidden")
                  , ("-webkit-box-sizing", "border-box") -- Safari/Chrome, other WebKit
                  , ("-moz-box-sizing", "border-box")    -- Firefox, other Gecko
                  , ("box-sizing", "border-box")         -- Opera/IE 8+
                  , ("background-color", "transparent")
                  ] ++
                    if model.hidden then [("display", "none")] else []
              , Attributes.placeholder "Type some words..."
              , Attributes.autofocus True
              ]
              []
          , Html.div
              [ Attributes.style <|
                  [ ("min-height", "1.25em")
                  , ("padding", "10pt")
                  , ("white-space", "pre-wrap")
                  , ("word-wrap", "break-word")
                  , ("color", "transparent")
                  ] ++
                    if model.hidden then [("display", "none")] else []
              ] <|
              List.map viewTextUnit model.userText ++ [ Html.text "\n" ]
          ]
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
                , ("line-height", "1.25em")
                , ("min-height", "1.25em")
                , ("width", "20em")
                , ("padding", "10pt")
                , ("border", "1pt solid")
                , ("border-radius", "3pt")
                , ("margin", "10pt")
                , ("resize", "horizontal")
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

viewTextUnit : TextUnit -> Html
viewTextUnit textUnit =
  if List.isEmpty textUnit.pathLists &&
    isPronounced (Respell.firstChar textUnit.spelling) then
    Html.mark
      [ Attributes.style
          [ ("border-radius", "3pt")
          , ("color", "transparent")
          , ("background-color", "#ffdddd")
          ]
      ]
      [ Html.text textUnit.spelling ]
    else Html.text textUnit.spelling

isPronounced : Char -> Bool
isPronounced c = Char.isLower c || Char.isUpper c || Char.isDigit c

type Action
  = EditText String
  | RespellText
  | RefreshText
  | DataLoaded DataLoader.Action
  | HideInput
  | ShowInput

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
        | userText =
            case model.dataLoader of
              DataLoader.NotLoaded _ -> []
              DataLoader.Loaded data -> Respell.getTextUnits data newUserText
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
                  case result.status of
                    InProgress (text, remainingPhonemes) ->
                      text ++ String.repeat (dotCount remainingPhonemes) "​."
                    Done (text, _) -> text
                    NoSolution -> "no solution"
              , cache = result.cache
              , modified =
                  case result.status of
                    InProgress _ -> True
                    _ -> False
              }
            , case result.status of
                InProgress _ -> Effects.task <| Task.succeed RespellText
                _ -> Effects.none
            )
    HideInput -> ({ model | hidden = True }, Effects.none)
    ShowInput -> ({ model | hidden = False }, Effects.none)
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
                  case result.status of
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
