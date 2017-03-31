module DataLoader exposing (Model, init, Msg, update, data, view)

import Html exposing (Html)
import Http exposing (Error(..))

import DeletionCosts exposing (DeletionCosts)
import Pronouncer exposing (Pronouncer)
import SubCosts exposing (SubCosts)
import WordCosts exposing (Speller, WordCosts)

dURL : String
dURL = "data/deletions.txt"

sURL : String
sURL = "data/substitutions.txt"

pURL : String
pURL = "data/pronouncer.txt"

wURL : String
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
      [ Http.send
          ( DResult <<
              ( Result.mapError (httpErrorToString dURL) >>
                  Result.andThen
                  ( Result.mapError DeletionCosts.parseErrorToString <<
                      DeletionCosts.parse
                  )
              )
          ) <|
          Http.getString dURL
      , Http.send
          ( SResult <<
              ( Result.mapError (httpErrorToString sURL) >>
                  Result.andThen
                  ( Result.mapError SubCosts.parseErrorToString <<
                      SubCosts.parse
                  )
              )
          ) <|
          Http.getString sURL
      , Http.send
          ( PResult <<
              ( Result.mapError (httpErrorToString pURL) >>
                  Result.andThen
                  ( Result.mapError Pronouncer.parseErrorToString <<
                      Pronouncer.parse
                  )
              )
          ) <|
          Http.getString pURL
      , Http.send
          ( WResult <<
              ( Result.mapError (httpErrorToString wURL) >>
                  Result.andThen
                  ( Result.mapError WordCosts.parseErrorToString <<
                      WordCosts.parse
                  )
              )
          ) <|
          Http.getString wURL
      ]
  )

httpErrorToString : String -> Http.Error -> String
httpErrorToString url err =
  "error loading " ++ url ++ ": " ++
    case err of
      BadUrl _ -> "invalid url"
      Timeout -> "connection timed out"
      NetworkError ->  "network error"
      BadPayload _ _ -> "unexpected payload"
      BadStatus response ->
        "bad response code " ++ toString response.status.code

type Msg
  = DResult (Result String DeletionCosts)
  | SResult (Result String SubCosts)
  | PResult (Result String (Pronouncer))
  | WResult (Result String (Speller, WordCosts))

update : Msg -> Model -> (Model, Cmd.Cmd Msg)
update action model =
  ( case action of
      DResult result -> { model | dState = Just result }
      SResult result -> { model | sState = Just result }
      PResult result -> { model | pState = Just result }
      WResult result -> { model | wState = Just result }
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
