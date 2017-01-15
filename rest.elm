module Rest exposing (..)
import Types exposing (..)

type UrlType =
  CreateNewGameUrl
  | CreateMoveUrl (Maybe Int)

getUrl : UrlType -> String
getUrl urlType =
  case urlType of
    CreateNewGameUrl ->
      "http://localhost:3000/tetrelm/games/create"
    CreateMoveUrl Nothing ->
      ""
    CreateMoveUrl (Just gameId) ->
      "http://localhost:3000/tetrelm/games/" ++ toString gameId ++ "/moves/create"
