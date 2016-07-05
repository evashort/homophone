import Html exposing (Html)
import Html.App
import Html.Events as Events
import Html.Attributes as Attributes
import Json.Decode
import Process
import Random
import Task

import DataLoader
import Rack exposing (Rack)

main : Program Never
main =
  Html.App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }

type UserText = RawText String | Respelled Rack

type alias Model =
  { dataLoader : DataLoader.Model
  , userText : UserText
  , hidden : Bool
  }

init : (Model, Cmd Msg)
init =
  ( { dataLoader = fst DataLoader.init
    , userText = RawText ""
    , hidden = False
    }
  , Cmd.map DataLoaded <| snd DataLoader.init
  )

view : Model -> Html Msg
view model =
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
         , Events.onClick <| if model.hidden then ShowInput else HideInput
         , Attributes.style [ ("margin", "12px") ]
         ]
         [ Html.text <| if model.hidden then "Show input" else "Hide input" ]
    , Html.div
        [ Attributes.style
            [ ("display", "table")
            , ("font-size", "20pt")
            , ("line-height", "1.25em")
            , ("width", "1%")
            ]
        ]
        [ Html.div
          [ Attributes.style
              [ ("min-height", "24px")
              , ("border", "1px solid")
              , ("border-radius", "3px")
              , ("margin", "12px")
              , ("position", "relative")
              ]
          ]
          [ Html.textarea
              [ Events.on
                  "input" <|
                  Json.Decode.map EditText Events.targetValue
              , Attributes.style <|
                  [ ("font", "inherit")
                  , ("width", "100%")
                  , ("height", "100%")
                  , ("padding", "12px")
                  , ("border", "none")
                  , ("margin", "0px")
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
          , Html.pre
              [ Attributes.style <|
                  [ ("font", "inherit")
                  , ("min-height", "1.25em")
                  , ("padding", "12px")
                  , ("margin", "0px")
                  , ("white-space", "pre-wrap")
                  , ("word-wrap", "break-word")
                  , ("color", "transparent")
                  ] ++
                    if model.hidden then [("display", "none")] else []
              ] <|
              viewGoal model.userText
          ]
        , Html.div [ Attributes.hidden True ] <|
            [ Html.button
                [ Events.onClick RefreshText
                , Attributes.style
                    [ ("padding", "10px 20px 10px 20px") ]
                ]
                [ Html.text "->" ]
            ] ++
              case model.userText of
                RawText _ -> []
                Respelled rack ->
                  List.map
                    (Html.div [] << List.repeat 1 << Html.text << toString) <|
                    Rack.costs rack
        , Html.pre
            [ Attributes.style
                [ ("font", "inherit")
                , ("width", "450px")
                , ("padding", "12px")
                , ("border", "1px solid black")
                , ("border-radius", "3px")
                , ("margin", "12px")
                , ("resize", "horizontal")
                , ("overflow", "auto")
                , ("white-space", "pre-wrap")
                , ("word-wrap", "break-word")
                ]
            ] <|
            viewSolution model.userText
        ]
     , Html.a
         [ Attributes.href "https://github.com/evanshort73/homophone"
         , Attributes.style [ ("margin", "12px") ]
         ]
         [ Html.text "GitHub" ]
    , DataLoader.view model.dataLoader
    ]

viewGoal : UserText -> List (Html msg)
viewGoal userText =
  case userText of
    RawText text -> [ Html.text <| text ++ "\n" ]
    Respelled rack -> Rack.viewGoal rack

viewSolution : UserText -> List (Html msg)
viewSolution userText =
  case userText of
    RawText _ ->
      [ Html.mark
          [ Attributes.style
              [ ("color", "darkgray")
              , ("background-color", "inherit")
              ]
          ]
          [ Html.text "Loading data...\n" ]
      ]
    Respelled rack -> Rack.view rack

type Msg
  = EditText String
  | RespellText
  | RefreshText
  | DataLoaded DataLoader.Msg
  | HideInput
  | ShowInput

update: Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    DataLoaded subMsg ->
      case DataLoader.update subMsg model.dataLoader of
        ( newDataLoader, subEffect ) ->
          let mappedSubEffect = Cmd.map DataLoaded subEffect in
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
                      Cmd.batch [ mappedSubEffect, yieldAndThen RespellText ]
                  )
              _ -> ({ model | dataLoader = newDataLoader }, mappedSubEffect)
    EditText newUserText ->
      case model.userText of
        RawText _ ->
          ( { model | userText = RawText newUserText }, Cmd.none )
        Respelled rack ->
          ( { model
            | userText = Respelled <| Rack.setGoal newUserText rack
            }
          , if Rack.done rack then yieldAndThen RespellText else Cmd.none
          )
    RespellText ->
      case model.userText of
        Respelled rack ->
          let (newRack, _) = Rack.update 1 rack in
            ( { model | userText = Respelled newRack }
            , if Rack.done newRack then Cmd.none
              else yieldAndThen RespellText
            )
        RawText _ -> Debug.crash "RespellText action before data loaded"
    HideInput -> ({ model | hidden = True }, Cmd.none)
    ShowInput -> ({ model | hidden = False }, Cmd.none)
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
              ( { model | userText = Respelled newRack }, Cmd.none )
            else Debug.crash "still in progress after maxInt iterations"
        _ -> Debug.crash "RefreshText action before data loaded"

yieldAndThen : msg -> Cmd msg
yieldAndThen =
  Task.perform identity identity <<
    Task.andThen (Process.sleep 0.0) << always << Task.succeed
