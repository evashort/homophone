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
import Respell exposing (TextUnit, Cache)
import StartApp

app =
  StartApp.start { init = init, update = update, view = view, inputs = [] }

main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks

port title : String
port title = "Homophone Generator"

type UserText = RawText String | Respelled Cache

type alias Model =
  { dataLoader : DataLoader.Model
  , userText : UserText
  , hidden : Bool
  }

init : (Model, Effects Action)
init =
  ( { dataLoader = fst DataLoader.init
    , userText = RawText ""
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
              case model.userText of
                RawText text -> [ Html.text <| text ++ "\n" ]
                Respelled cache ->
                  List.map viewTextUnit cache.textUnits ++ [ Html.text "\n" ]
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
            [ Attributes.style <|
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
                ] ++
                  case model.userText of
                    RawText _ -> [ ("color", "#767676") ]
                    Respelled _ -> []
            ] <|
            case model.userText of
              RawText _ -> [ Html.text "Loading data..." ]
              Respelled cache ->
                [ Html.text <|
                    Respell.spelling cache ++
                      if Respell.done cache then ""
                      else
                        String.repeat
                          (dotCount <| Respell.remainingPhonemes cache)
                          "​."
                ]
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
      case DataLoader.update subAction model.dataLoader of
        ( newDataLoader, subEffect ) ->
          let mappedSubEffect = Effects.map DataLoaded subEffect in
            case (DataLoader.data newDataLoader, model.userText) of
              ( Just (pronouncer, speller, dCosts, sCosts, wCosts)
              , RawText text
              ) ->
                let
                  cache =
                    Respell.setGoal
                      text <|
                      Respell.init pronouncer speller dCosts sCosts wCosts
                in
                  ( { model
                    | dataLoader = newDataLoader
                    , userText = Respelled cache
                    }
                  , if Respell.done cache then mappedSubEffect
                    else
                      Effects.batch
                        [ mappedSubEffect
                        , Effects.task <| Task.succeed RespellText
                        ]
                  )
              _ -> ({ model | dataLoader = newDataLoader }, mappedSubEffect)
    EditText newUserText ->
      case model.userText of
        RawText _ ->
          ( { model | userText = RawText newUserText }, Effects.none )
        Respelled cache ->
          ( { model
            | userText = Respelled <| Respell.setGoal newUserText cache
            }
          , if Respell.done cache then
              Effects.task <| Task.succeed RespellText
            else Effects.none
          )
    RespellText ->
      case model.userText of
        Respelled cache ->
          let newCache = Respell.update 1 cache in
            ( { model | userText = Respelled newCache }
            , if Respell.done newCache then
                if Respell.complete newCache then Effects.none
                else
                  Debug.crash <|
                    "no solution for \"" ++ Respell.goal newCache ++ "\""
              else Effects.task <| Task.succeed RespellText
            )
        RawText _ -> Debug.crash "RespellText action before data loaded"
    HideInput -> ({ model | hidden = True }, Effects.none)
    ShowInput -> ({ model | hidden = False }, Effects.none)
    RefreshText ->
      case (DataLoader.data model.dataLoader, model.userText) of
        ( Just (pronouncer, speller, dCosts, sCosts, wCosts)
        , Respelled cache
        ) ->
          let
            newCache =
              Respell.update Random.maxInt <|
                Respell.setGoal
                  (Respell.goal cache) <|
                  Respell.init pronouncer speller dCosts sCosts wCosts
          in
            if Respell.done newCache then
              if Respell.complete newCache then
                ( { model | userText = Respelled newCache }, Effects.none )
              else
                Debug.crash <|
                  "no solution for \"" ++ Respell.goal newCache ++ "\""
            else Debug.crash "still in progress after maxInt iterations"
        _ -> Debug.crash "RefreshText action before data loaded"

dotCount : Int -> Int
dotCount remainingPhonemes =
  max 3 <| round <| 2.33 * toFloat remainingPhonemes
