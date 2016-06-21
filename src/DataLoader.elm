module DataLoader exposing (Model, init, Msg, update, data, view)

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
  { dState : Maybe (Result String DeletionCosts)
  , sState : Maybe (Result String SubCosts)
  , pState : Maybe (Result String (Pronouncer))
  , wState : Maybe (Result String (Speller, WordCosts))
  }

init : (Model, Cmd Msg)
init =
  ( { dState = Nothing, sState = Nothing, pState = Nothing, wState = Nothing }
  , Cmd.batch
      [ Task.perform DError DLoaded <| Http.getString dURL
      , Task.perform SError SLoaded <| Http.getString sURL
      , Task.perform PError PLoaded <| Http.getString pURL
      , Task.perform WError WLoaded <| Http.getString wURL
      ]
  )

httpErrorToString : String -> Http.Error -> String
httpErrorToString url err =
  "error loading " ++ url ++ ": " ++
    case err of
      Http.Timeout -> "connection timed out"
      Http.NetworkError ->  "network error"
      Http.UnexpectedPayload _ -> "unexpected payload"
      Http.BadResponse statusCode _ ->
        "bad response code " ++ toString statusCode

type Msg
  = DLoaded String
  | SLoaded String
  | PLoaded String
  | WLoaded String
  | DError Http.Error
  | SError Http.Error
  | PError Http.Error
  | WError Http.Error

update : Msg -> Model -> (Model, Cmd.Cmd Msg)
update action model =
  ( case action of
      DLoaded fileContents ->
        { model
        | dState =
            Just <|
              Result.formatError
                DeletionCosts.parseErrorToString <|
                DeletionCosts.parse fileContents
        }
      SLoaded fileContents ->
        { model
        | sState =
            Just <|
              Result.formatError
                SubCosts.parseErrorToString <|
                SubCosts.parse fileContents
        }
      PLoaded fileContents ->
        { model
        | pState =
            Just <|
              Result.formatError
                Pronouncer.parseErrorToString <|
                Pronouncer.parse fileContents
        }
      WLoaded fileContents ->
        { model
        | wState =
            Just <|
              Result.formatError
                WordCosts.parseErrorToString <|
                WordCosts.parse fileContents
        }
      DError httpError ->
        { model | dState = Just <| Err <| httpErrorToString dURL httpError }
      SError httpError ->
        { model | sState = Just <| Err <| httpErrorToString sURL httpError }
      WError httpError ->
        { model | wState = Just <| Err <| httpErrorToString wURL httpError }
      PError httpError ->
        { model | pState = Just <| Err <| httpErrorToString pURL httpError }
  , Cmd.none
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

view : Model -> Html msg
view model =
  Html.div [] <|
    ( case model.dState of
        Just (Err err) -> [ Html.div [] [ Html.text err ] ]
        _ -> []
    ) ++
    ( case model.sState of
        Just (Err err) -> [ Html.div [] [ Html.text err ] ]
        _ -> []
    ) ++
    ( case model.pState of
        Just (Err err) -> [ Html.div [] [ Html.text err ] ]
        _ -> []
    ) ++
    ( case model.wState of
        Just (Err err) -> [ Html.div [] [ Html.text err ] ]
        _ -> []
    )
