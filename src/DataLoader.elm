module DataLoader where

import Effects exposing (Effects)
import Html exposing (Html)
import Http
import Task

import DeletionCosts exposing (DeletionCosts)
import Respell
import SubCosts exposing (SubCosts)
import WordCosts exposing (Pronouncer, Speller, WordCosts)

deletionCostsURL = "data/deletions.txt"
subCostsURL = "data/substitutions.txt"
wordCostsURL = "data/entropyAndPronounce.txt"

type Model
  = Loaded Respell.LoadedData
  | NotLoaded LoadingState

type alias LoadingState =
  { deletionCostsState : Maybe (Result DeletionCostsError DeletionCosts)
  , subCostsState : Maybe (Result SubCostsError SubCosts)
  , wordCostsState :
      Maybe (Result WordCostsError (Pronouncer, Speller, WordCosts))
  }

init : (Model, Effects Action)
init =
  ( NotLoaded
      { deletionCostsState = Nothing
      , subCostsState = Nothing
      , wordCostsState = Nothing
      }
  , Effects.batch
    [ Effects.task <|
        Task.map
          DeletionCostsLoaded <|
          Task.toResult <| Http.getString deletionCostsURL
    , Effects.task <|
        Task.map
          SubCostsLoaded <|
          Task.toResult <| Http.getString subCostsURL
    , Effects.task <|
        Task.map
          WordCostsLoaded <|
          Task.toResult <| Http.getString wordCostsURL
    ]
  )

type DeletionCostsError
  = DLoadError Http.Error
  | DParseError DeletionCosts.ParseError

deletionCostsErrorToString : DeletionCostsError -> String
deletionCostsErrorToString err =
  case err of
    DLoadError httpErr ->
      "error loading " ++ deletionCostsURL ++ ": " ++
        httpErrorToString httpErr
    DParseError parseErr ->
      "error parsing deletions: " ++ DeletionCosts.parseErrorToString parseErr

type SubCostsError
  = SLoadError Http.Error
  | SParseError SubCosts.ParseError

subCostsErrorToString : SubCostsError -> String
subCostsErrorToString err =
  case err of
    SLoadError httpErr ->
      "error loading " ++ subCostsURL ++ ": " ++
        httpErrorToString httpErr
    SParseError parseErr ->
      "error parsing substitutions: " ++ SubCosts.parseErrorToString parseErr

type WordCostsError
  = WLoadError Http.Error
  | WParseError WordCosts.ParseError

wordCostsErrorToString : WordCostsError -> String
wordCostsErrorToString err =
  case err of
    WLoadError httpErr ->
      "error loading " ++ wordCostsURL ++ ": " ++
        httpErrorToString httpErr
    WParseError parseErr ->
      "error parsing words: " ++ WordCosts.parseErrorToString parseErr

httpErrorToString : Http.Error -> String
httpErrorToString err =
  case err of
    Http.Timeout -> "connection timed out"
    Http.NetworkError ->  "network error"
    Http.UnexpectedPayload _ -> "unexpected payload"
    Http.BadResponse statusCode _ ->
      "bad response code " ++ toString statusCode

type Action
  = DeletionCostsLoaded (Result Http.Error String)
  | SubCostsLoaded (Result Http.Error String)
  | WordCostsLoaded (Result Http.Error String)

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  ( case model of
      Loaded _ -> model
      NotLoaded loadingState ->
        loadedCheck <|
          case action of
            DeletionCostsLoaded result ->
              { loadingState
              | deletionCostsState =
                  Just <|
                    (Result.formatError DLoadError result)
                    `Result.andThen`
                    (Result.formatError DParseError << DeletionCosts.parse)
              }
            SubCostsLoaded result ->
              { loadingState
              | subCostsState =
                  Just <|
                    (Result.formatError SLoadError result)
                    `Result.andThen`
                    (Result.formatError SParseError << SubCosts.parse)
              }
            WordCostsLoaded result ->
              { loadingState
              | wordCostsState =
                  Just <|
                    (Result.formatError WLoadError result)
                    `Result.andThen`
                    (Result.formatError WParseError << WordCosts.parse)
              }
  , Effects.none
  )

loadedCheck : LoadingState -> Model
loadedCheck s =
  case (s.deletionCostsState, s.subCostsState, s.wordCostsState) of
    ( Just (Ok deletionCosts)
    , Just (Ok subCosts)
    , Just (Ok (pronouncer, speller, wordCosts))
    ) ->
      Loaded
        { deletionCosts = deletionCosts
        , subCosts = subCosts
        , pronouncer = pronouncer
        , speller = speller
        , wordCosts = wordCosts
        }
    _ -> NotLoaded s

view : Model -> Html
view model =
  Html.div [] <|
    case model of
      Loaded _ -> []
      NotLoaded loadingState ->
        ( case loadingState.deletionCostsState of
            Just (Err err) ->
              [ Html.div [] [ Html.text <| deletionCostsErrorToString err ] ]
            _ -> []
        ) ++
        ( case loadingState.subCostsState of
            Just (Err err) ->
              [ Html.div [] [ Html.text <| subCostsErrorToString err ] ]
            _ -> []
        ) ++
        ( case loadingState.wordCostsState of
            Just (Err err) ->
              [ Html.div [] [ Html.text <| wordCostsErrorToString err ] ]
            _ -> []
        )
