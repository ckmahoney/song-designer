module Comm.Decoders exposing (..)

import Json.Decode as Decode
import Defs.Types exposing (..)
import Html exposing (text)

decodeTrack : Decode.Decoder TrackMeta
decodeTrack =
  Decode.map6 TrackMeta
    (Decode.field "id" Decode.int)
    (Decode.field "account_id" Decode.int)
    (Decode.field "filepath" Decode.string)
    (Decode.field "title" Decode.string)
    (Decode.field "size_bytes" Decode.int)
    (Decode.field "duration_seconds" Decode.float)


main = text ""
