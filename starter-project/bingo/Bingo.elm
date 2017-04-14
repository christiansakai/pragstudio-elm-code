module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import ViewHelpers exposing (..)
import Entry
import Score

-- MODEL

type GameState = EnteringName | Playing

type alias Model =
  { name : String
  , gameNumber : Int
  , entries : List Entry.Entry
  , alertMessage : Maybe String
  , nameInput : String
  , gameState : GameState
  }


initialModel : Model
initialModel =
  Model "Anonymous" 1 [] Nothing "" EnteringName

-- UPDATE

type Msg = NewGame
         | Mark Int
         | NewRandom Int
         | NewEntries (Result Http.Error (List Entry.Entry))
         | CloseAlert
         | ShareScore
         | NewScore (Result Http.Error Score.Score)
         | SetNameInput String
         | SaveName
         | CancelName
         | ChangeGameState GameState

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeGameState state ->
      ({ model | gameState = state }, Cmd.none)

    SaveName ->
      case model.nameInput of
        "" ->
          (model, Cmd.none)

        newName ->
          ({ model | name = newName, 
                     nameInput = "",
                     gameState = Playing }, Cmd.none)

    CancelName ->
      ({ model | nameInput = "",
                 gameState = Playing }, Cmd.none)

    SetNameInput value ->
      ({ model | nameInput = value } , Cmd.none)

    NewRandom randomNumber ->
      ({ model | gameNumber = randomNumber }, Cmd.none)

    ShareScore ->
      (model, Score.postScore model)

    NewScore (Ok score) ->
      let 
          message =
            "Your score of " 
              ++ (toString score.score)
              ++ " was successfully shared!"
      in
        ({ model | alertMessage = Just message }, Cmd.none)

    NewScore (Err error) ->
        ({ model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none)

    NewGame ->
      ({ model | gameNumber = model.gameNumber + 1 }, getEntries)

    NewEntries (Ok randomEntries) ->
      ({ model | entries = randomEntries }, Cmd.none)

    NewEntries (Err error) ->
        ({ model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none)

    CloseAlert ->
      ({ model | alertMessage = Nothing }, Cmd.none)

    Mark id ->
      ({ model | entries = Entry.markEntryWithId model.entries id }, Cmd.none)
    

-- COMMANDS

generateRandomNumber : Cmd Msg
generateRandomNumber =
  Random.generate NewRandom (Random.int 1 100)

entriesUrl : String
entriesUrl = 
  apiUrlPrefix ++ "/random-entries"


-- HELPER
isNothing : Maybe a -> Bool
isNothing entity =
  case entity of
    Just _ -> True
    Nothing -> False


getEntries : Cmd Msg
getEntries =
  Entry.getEntries NewEntries ("http://localhost:3000" ++ "/random-entries")



-- VIEW

apiUrlPrefix : String
apiUrlPrefix =
  "http://localhost:3000"

playerInfo : String -> Int -> String
playerInfo name gameNumber =
  name ++ " - Game #" ++ (toString gameNumber)

viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    h2 [ id "info", class "classy" ] 
        [ a [ href "#", onClick (ChangeGameState EnteringName) ] [ text name ]
        , text (" - Game #" ++ (toString gameNumber)) ]

viewHeader : String -> Html Msg
viewHeader title =
  header []
    [ h1 [] [ text title ] ]

viewFooter : Html Msg
viewFooter =
  footer []
    [ a [ href "http://elm-lang.org" ]
        [ text "Powered by Elm" ]
    ]

view : Model -> Html Msg
view model =
  div [ class "content" ]
      [ viewHeader "BUZZWORD BINGO"
      , alert CloseAlert model.alertMessage
      , viewNameInput model
      , viewPlayer model.name model.gameNumber
      , Entry.viewEntryList Mark model.entries
      , Score.viewScore (Entry.sumMarkedPoints model.entries)
      , div [ class "button-group" ]
            [ primaryButton NewGame "New Game" ] 
            , button [ class "primary", onClick ShareScore 
                     , disabled (allEntriesUnmarked model.entries) ] [ text "Share Score" ] 
      , div [ class "debug" ] [ text (toString model) ]
      , viewFooter
      ]

viewNameInput : Model -> Html Msg
viewNameInput model =
  case model.gameState of
    EnteringName ->
      div [ class "name-input" ]
          [ input 
            [ type_ "text"
            , placeholder "Who's playing?"
            , autofocus True
            , value model.nameInput
            , onInput SetNameInput 
            ]
            []
          , primaryButton SaveName "Save"
          , primaryButton CancelName "Cancel"
          ]
    Playing ->
      text ""

httpErrorToMessage : Http.Error -> String
httpErrorToMessage error =
  case error of
    Http.NetworkError ->
      "Is the server running?"
    Http.BadStatus response ->
      (toString response.status)

    Http.BadPayload message _ ->
      "Decoding failed " ++ message

    _ ->
      (toString error)


-- Other

allEntriesMarked : List Entry.Entry -> Bool
allEntriesMarked entries =
  List.all (\e -> e.marked == True) entries

allEntriesUnmarked : List Entry.Entry -> Bool
allEntriesUnmarked entries =
  List.all (\e -> e.marked == False) entries

main : Program Never Model Msg
main =
  Html.program 
    { init = (initialModel, getEntries)
    , view = view
    , update = update
    , subscriptions = (\model -> Sub.none)
    }
