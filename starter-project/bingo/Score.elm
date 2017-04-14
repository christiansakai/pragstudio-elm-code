module Score exposing 
  ( Score
  , viewScore
  , postScore
  )

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Http
import Entry

type alias Score =
  { id : Int
  , name : String
  , score : Int
  }

viewScore : Int -> Html msg
viewScore sum =
  div [ class "score" ]
      [ span [ class "label" ] [ text "Score" ]
      , span [ class "value" ] [ text (toString sum) ]
      ]

-- postScore : msg -> Model -> Cmd msg
postScore msg model = 
  let 
      url =
        "http://localhost:3000/scores"

      body =
        encodeScore model
          |> Http.jsonBody

      request =
        Http.post url body scoreDecoder
  in
    Http.send msg request


encodeScore : Model -> Encode.Value
encodeScore model =
  Encode.object 
    [ ("name", Encode.string model.name)
    , ("score", Encode.int (Entry.sumMarkedPoints model.entries))
    ]

scoreDecoder : Decoder Score
scoreDecoder =
  Pipeline.decode Score
    |> Pipeline.required "id" Decode.int
    |> Pipeline.required "name" Decode.string
    |> Pipeline.required "score" Decode.int

