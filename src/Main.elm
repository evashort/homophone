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
import Rack exposing (Rack)
import StartApp

app =
  StartApp.start { init = init, update = update, view = view, inputs = [] }

main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks

port title : String
port title = "Homophone Generator"

type UserText = RawText String | Respelled Rack

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
            , ("margin", "12px")
            ]
        ]
        [ Html.text "Homophone Generator" ]
     , Html.a
         [ Attributes.href "#"
         , Events.onClick
             address <|
             if model.hidden then ShowInput else HideInput
         , Attributes.style [ ("margin", "12px") ]
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
              , ("min-height", "24px")
              , ("border", "1px solid")
              , ("border-radius", "3px")
              , ("margin", "12px")
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
                  , ("padding", "12px")
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
                  , ("padding", "12px")
                  , ("white-space", "pre-wrap")
                  , ("word-wrap", "break-word")
                  , ("color", "transparent")
                  ] ++
                    if model.hidden then [("display", "none")] else []
              ] <|
              case model.userText of
                RawText text -> [ Html.text <| text ++ "\n" ]
                Respelled rack -> Rack.view rack
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
                , ("width", "450px")
                , ("padding", "12px")
                , ("border", "1px solid black")
                , ("border-radius", "3px")
                , ("margin", "12px")
                , ("resize", "horizontal")
                , ("overflow", "auto")
                , ("white-space", "pre-wrap")
                , ("word-wrap", "break-word")
                ] ++
                  case model.userText of
                    RawText _ -> [ ("color", "darkgray") ]
                    Respelled _ -> []
            ]
            [ Html.text <|
                case model.userText of
                  RawText _ ->  "Loading data..."
                  Respelled rack -> Rack.spelling rack
            ]
        ]
     , Html.a
         [ Attributes.href
             "https://github.com/evanshort73/homophone/blob/master/LICENSE"
         , Attributes.style [ ("margin", "12px") ]
         ]
         [ Html.text "License" ]
    , DataLoader.view model.dataLoader
    ]

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
                  rack =
                    Rack.setGoal
                      text <|
                      Rack.init pronouncer speller dCosts sCosts wCosts
                in
                  ( { model
                    | dataLoader = newDataLoader
                    , userText = Respelled rack
                    }
                  , if Rack.done rack then mappedSubEffect
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
        Respelled rack ->
          ( { model
            | userText = Respelled <| Rack.setGoal newUserText rack
            }
          , if Rack.done rack then Effects.task <| Task.succeed RespellText
            else Effects.none
          )
    RespellText ->
      case model.userText of
        Respelled rack ->
          let (newRack, _) = Rack.update 1 rack in
            ( { model | userText = Respelled newRack }
            , if Rack.done newRack then
                if Rack.complete newRack then Effects.none
                else
                  Debug.crash <|
                    "no solution for \"" ++ Rack.goal newRack ++ "\""
              else Effects.task <| Task.succeed RespellText
            )
        RawText _ -> Debug.crash "RespellText action before data loaded"
    HideInput -> ({ model | hidden = True }, Effects.none)
    ShowInput -> ({ model | hidden = False }, Effects.none)
    RefreshText ->
      case (DataLoader.data model.dataLoader, model.userText) of
        ( Just (pronouncer, speller, dCosts, sCosts, wCosts)
        , Respelled rack
        ) ->
          let
            (newRack, _) =
              Rack.update Random.maxInt <|
                Rack.setGoal
                  (Rack.goal rack) <|
                  Rack.init pronouncer speller dCosts sCosts wCosts
          in
            if Rack.done newRack then
              if Rack.complete newRack then
                ( { model | userText = Respelled newRack }, Effects.none )
              else
                Debug.crash <|
                  "no solution for \"" ++ Rack.goal newRack ++ "\""
            else Debug.crash "still in progress after maxInt iterations"
        _ -> Debug.crash "RefreshText action before data loaded"
