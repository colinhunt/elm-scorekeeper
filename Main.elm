port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias State a = { a |
    name: String,
    playerId: Maybe Int,
    persisting: Int
}

type alias Player = {
    id: Int,
    name: String
}

type alias Play = {
    id: Int,
    playerId: Int,
    points: Int
}

type alias Data = {
    players: List Player,
    plays: List Play
}

type alias Model = State Data

initModel: (Model, Cmd msg)
initModel = ({
        players = [],
        name = "",
        playerId = Nothing,
        plays = [],
        persisting = 0
    }, Cmd.none)

type Msg =
    Edit Player |
    Score Player Int |
    Input String |
    Save |
    Cancel |
    DeletePlay Play |
    ReceiveData Model |
    DonePersisting Bool

update: Msg -> Model -> (Model, Cmd msg)
update msg model = 
    case msg of
        ReceiveData newModel ->
            ( newModel, Cmd.none )

        Input name ->
            ({ model | name = name }, Cmd.none)

        Cancel ->
            ({ model | name = "", playerId = Nothing }, Cmd.none)

        Save ->
            persist (
                if model.name == "" then
                    model
                else
                    save model
            )

        Edit player ->
            ({ model | name = player.name, playerId = Just player.id }, Cmd.none)

        Score player points ->
            persist (score model player points)

        DeletePlay play ->
            persist { model | plays = (List.filter (\p -> p /= play) model.plays) }

        DonePersisting _ ->
            ({ model | persisting = model.persisting - 1 }, Cmd.none)


persist: Model -> (Model, Cmd msg)
persist model =
    ( { model | persisting = model.persisting + 1 }, persistData (Data model.players model.plays) )


score: Model -> Player -> Int -> Model
score model player points =
    let
        play = Play (List.length model.plays) player.id points
    in
        { model | plays = play :: model.plays }

save: Model -> Model
save model = 
    case model.playerId of
        Just id ->
            modifyPlayer model id

        Nothing ->
            add model


modifyPlayer: Model -> Int -> Model
modifyPlayer model id =
    let
        newPlayers = model.players
            |> List.map (\player ->
                if player.id == id then
                    { player | name = model.name }
                else
                    player
            )
    in
        { model | players = newPlayers, name = "", playerId = Nothing }


add: Model -> Model
add model =
    let
        player = Player (List.length model.players) model.name

        newPlayers = player :: model.players
    in
        { model | players = newPlayers, name = "" }


view: Model -> Html Msg
view model =
    div [] [
        h1 [] [ text "Score Keeper!" ],
        loadingMsg model,
        playerSection model,
        playerForm model,
        playsSection model
    ]

loadingMsg: Model -> Html Msg
loadingMsg {persisting} =
    div [] [
        if persisting > 0 then
            text "saving..."
        else
            text "saved."
    ]

playsSection: Model -> Html Msg
playsSection model =
    div [] [
        playListHeader,
        playList model
    ]

playListHeader : Html msg
playListHeader =
    header [] [
        div [] [ text "Plays" ],
        div [] [ text "Points" ]
    ]

playList: Model -> Html Msg
playList model =
    model.plays
        |> List.map (play model)
        |> ul []

play: Model -> Play -> Html Msg
play model play =
    li [] [
        i [ class "remove", onClick (DeletePlay play) ] [],
        span [] [ text ((findPlayer model.players play.playerId) |> .name) ],
        div [] [ text (toString play.points) ]
    ]

findPlayer: List Player -> Int -> Player
findPlayer players id =
    players 
       |> List.filter (\player -> player.id == id)
       |> List.head
       |> Maybe.withDefault (Player -1 ("PLAYER NOT FOUND WITH ID=" ++ (toString id)))


playerSection: Model -> Html Msg
playerSection model =
    div [] [
        playerListHeader,
        playerList model,
        pointTotal model
    ]

playerListHeader: Html Msg
playerListHeader =
    header [] [
        div [] [ text "Name" ],
        div [] [ text "Points" ]
    ]

playerList: Model -> Html Msg
playerList model =
    model.players
        |> List.sortBy .name
        |> List.map (player model)
        |> ul []


player: Model -> Player -> Html Msg
player model player =
    li [] [
        i [ class "edit", onClick (Edit player) ] [],
        span [] [
            text player.name
        ],
        br [] [],
        button [ type_ "button", onClick (Score player 2) ] [ text "2pt" ],
        button [ type_ "button", onClick (Score player 3) ] [ text "3pt" ],
        div [] [
            playerPointTotal model player 
        ]
    ]

playerPointTotal: Model -> Player -> Html Msg
playerPointTotal model player =
    model.plays
        |> List.filter (\play -> play.playerId == player.id)
        |> List.map .points
        |> List.sum
        |> toString
        |> text

pointTotal: Model -> Html Msg
pointTotal model =
    let
        total = List.map .points model.plays
            |> List.sum
    in
        footer [] [
            div [] [ text "Total:" ],
            div [] [ text (toString total) ]
        ]

playerForm: Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save  ] [
        input [
            type_ "text",
            placeholder "Add/Edit Player...",
            onInput Input,
            value model.name
        ] [],
        button [ type_ "submit" ] [ text "+" ],
        button [ type_ "button", onClick Cancel ] [ text "←" ]
    ]

port persistData: Data -> Cmd msg
port donePersisting: (Bool -> msg) -> Sub msg
port receiveData: (Data -> msg) -> Sub msg

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch [
        receiveData (\data -> ReceiveData {model | players = data.players, plays = data.plays}),
        donePersisting DonePersisting
    ]

main: Program Never Model Msg
main = 
    Html.program {
        init = initModel,
        view = view,
        update = update,
        subscriptions = subscriptions
    }