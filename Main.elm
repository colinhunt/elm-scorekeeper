module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model = {
    players: List Player,
    name: String,
    playerId: Maybe Int,
    plays: List Play
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

initModel: Model
initModel = {
        players = [],
        name = "",
        playerId = Nothing,
        plays = []
    }

type Msg =
    Edit Player |
    Score Player Int |
    Input String |
    Save |
    Cancel |
    DeletePlay Play

update: Msg -> Model -> Model
update msg model = 
    case msg of
        Input name ->
            { model | name = name }

        Cancel ->
            { model | name = "", playerId = Nothing }

        Save ->
            if model.name == "" then
                model
            else
                save model 

        Edit player ->
            { model | name = player.name, playerId = Just player.id }

        Score player points ->
            score model player points

        _ ->
            model


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
    div [ class "scoreboard" ] [
        h1 [] [ text "Score Keeper" ],
        playerSection model,
        playerForm model,
        p [] [ text (toString model) ]
    ]

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
        div [] [
            text player.name
        ],
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
        button [ type_ "submit" ] [ text "Save" ],
        button [ type_ "button", onClick Cancel ] [ text "Cancel" ]
    ]

main: Program Never Model Msg
main = 
    Html.beginnerProgram {
        model = initModel,
        view = view,
        update = update
    }