module Comm.Post exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Configs as Conf
import Comm.Decoders as JD
import Types exposing (TrackMeta)


bodyFetchSongs : String -> String -> Encode.Value
bodyFetchSongs email uuid = 
  Encode.object
    [ ("action", Encode.string "songs")
    , ("email", Encode.string email)
    , ("uuid", Encode.string uuid)
    ]


fetchSongs : String -> String -> (Result Http.Error (List TrackMeta) -> msg) -> Cmd msg
fetchSongs email uuid cb =
  Http.post
    { url = Conf.selfUrl "user"
    , body = Http.jsonBody <| bodyFetchSongs email uuid
    , expect = Http.expectJson cb (Decode.list JD.decodeTrack)
    }
