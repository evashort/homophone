module DataLoader where

import Effects exposing (Effects)
import Html exposing (Html)
import Http
import Task

import DeletionCosts exposing (DeletionCosts)
import Pronouncer exposing (Pronouncer)
import SubCosts exposing (SubCosts)
import WordCosts exposing (Speller, WordCosts)

dURL = "data/deletions.txt"
sURL = "data/substitutions.txt"
pURL = "data/pronouncer.txt"
wURL = "data/speller.txt"

type alias Model =
  { dState : Maybe (Result DError DeletionCosts)
  , sState : Maybe (Result SError SubCosts)
  , pState : Maybe (Result PError (Pronouncer))
  , wState : Maybe (Result WError (Speller, WordCosts))
  }

init : (Model, Effects Action)
init =
  ( { dState = Nothing, sState = Nothing, pState = Nothing, wState = Nothing }
  , Effects.batch
    [ Effects.task <| Task.map DLoaded <| Task.toResult <| Http.getString dURL
    , Effects.task <| Task.map SLoaded <| Task.toResult <| Http.getString sURL
    , Effects.task <| Task.map PLoaded <| Task.toResult <| Http.getString pURL
    , Effects.task <| Task.map WLoaded <| Task.toResult <| Http.getString wURL
    ]
  )

type DError = DLoadError Http.Error | DParseError DeletionCosts.ParseError

dErrorToString : DError -> String
dErrorToString err =
  case err of
    DLoadError httpErr ->
      "error loading " ++ dURL ++ ": " ++ httpErrorToString httpErr
    DParseError parseErr ->
      "error parsing deletions: " ++ DeletionCosts.parseErrorToString parseErr

type SError = SLoadError Http.Error | SParseError SubCosts.ParseError

sErrorToString : SError -> String
sErrorToString err =
  case err of
    SLoadError httpErr ->
      "error loading " ++ sURL ++ ": " ++ httpErrorToString httpErr
    SParseError parseErr ->
      "error parsing substitutions: " ++ SubCosts.parseErrorToString parseErr

type PError = PLoadError Http.Error | PParseError Pronouncer.ParseError

pErrorToString : PError -> String
pErrorToString err =
  case err of
    PLoadError httpErr ->
      "error loading " ++ pURL ++ ": " ++ httpErrorToString httpErr
    PParseError parseErr ->
      "error parsing pronouncer: " ++ Pronouncer.parseErrorToString parseErr

type WError = WLoadError Http.Error | WParseError WordCosts.ParseError

wErrorToString : WError -> String
wErrorToString err =
  case err of
    WLoadError httpErr ->
      "error loading " ++ wURL ++ ": " ++ httpErrorToString httpErr
    WParseError parseErr ->
      "error parsing speller: " ++ WordCosts.parseErrorToString parseErr

httpErrorToString : Http.Error -> String
httpErrorToString err =
  case err of
    Http.Timeout -> "connection timed out"
    Http.NetworkError ->  "network error"
    Http.UnexpectedPayload _ -> "unexpected payload"
    Http.BadResponse statusCode _ ->
      "bad response code " ++ toString statusCode

type Action
  = DLoaded (Result Http.Error String)
  | SLoaded (Result Http.Error String)
  | PLoaded (Result Http.Error String)
  | WLoaded (Result Http.Error String)

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  ( case action of
      DLoaded result ->
        { model
        | dState =
            Just <|
              Result.andThen
                (Result.formatError DLoadError result) <|
                Result.formatError DParseError << DeletionCosts.parse
        }
      SLoaded result ->
        { model
        | sState =
            Just <|
              Result.andThen
                (Result.formatError SLoadError result) <|
                Result.formatError SParseError << SubCosts.parse
        }
      PLoaded result ->
        { model
        | pState =
            Just <|
              Result.andThen
                (Result.formatError PLoadError result) <|
                Result.formatError PParseError << Pronouncer.parse
        }
      WLoaded result ->
        { model
        | wState =
            Just <|
              Result.andThen
                (Result.formatError WLoadError result) <|
                Result.formatError WParseError << WordCosts.parse
        }
  , Effects.none
  )

data :
  Model -> Maybe (Pronouncer, Speller, DeletionCosts, SubCosts, WordCosts)
data model =
  case (model.dState, model.sState, model.pState, model.wState) of
    ( Just (Ok dCosts)
    , Just (Ok sCosts)
    , Just (Ok pronouncer)
    , Just (Ok (speller, wCosts))
    ) -> Just (pronouncer, speller, dCosts, sCosts, wCosts)
    _ -> Nothing

view : Model -> Html
view model =
  Html.div [] <|
    ( case model.dState of
        Just (Err err) ->
          [ Html.div [] [ Html.text <| dErrorToString err ] ]
        _ -> []
    ) ++
    ( case model.sState of
        Just (Err err) ->
          [ Html.div [] [ Html.text <| sErrorToString err ] ]
        _ -> []
    ) ++
    ( case model.pState of
        Just (Err err) ->
          [ Html.div [] [ Html.text <| pErrorToString err ] ]
        _ -> []
    ) ++
    ( case model.wState of
        Just (Err err) ->
          [ Html.div [] [ Html.text <| wErrorToString err ] ]
        _ -> []
    )
