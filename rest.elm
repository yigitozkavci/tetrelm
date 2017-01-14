module Rest exposing (..)
import Types exposing (..)

sendModel : Model -> Cmd Msg
sendModel model =
  let _ = Debug.log "model" model in
    Cmd.none
