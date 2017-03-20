module GameOfLife exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Set
import Starts


main : Program Never Model Msg
main =
    beginnerProgram { model = [], view = view, update = update }


type alias Position =
    ( Int, Int )


type alias Model =
    List Position


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map renderPosition model)
        , div [ style [ ( "margin-left", "500px" ) ] ]
            [ button [ onClick Something ] [ text "Something" ]
            , button [ onClick Many ] [ text "Many" ]
            , button [ onClick Stars ] [ text "Stars" ]
            , button [ onClick NextStep ] [ text "Next Step" ]
            ]
        ]


renderPosition : Position -> Html Msg
renderPosition ( xPos, yPos ) =
    div
        [ style
            [ ( "background-color", "black" )
            , ( "position", "absolute" )
            , ( "top", (toString (10 * yPos)) ++ "px" )
            , ( "left", (toString (10 * xPos)) ++ "px" )
            , ( "width", "10px" )
            , ( "height", "10px" )
            ]
        ]
        []


type Msg
    = Something
    | Many
    | Stars
    | NextStep


update : Msg -> Model -> Model
update msg model =
    case msg of
        NextStep ->
            List.foldr (updatePosition model) [] (positionsToUpdate model)

        Something ->
            Starts.something

        Many ->
            Starts.many

        Stars ->
            Starts.stars


positionsToUpdate : Model -> List Position
positionsToUpdate model =
    model
        |> List.foldr collectNeighbours model
        |> unique


unique : List comparable -> List comparable
unique list =
    list
        |> Set.fromList
        |> Set.toList


collectNeighbours : Position -> List Position -> List Position
collectNeighbours position collectedNeighbours =
    (possibleNeighbours position) ++ collectedNeighbours


updatePosition : Model -> Position -> Model -> Model
updatePosition model position updatedModel =
    case neighbourCount position model of
        3 ->
            position :: updatedModel

        2 ->
            if List.member position model then
                position :: updatedModel
            else
                updatedModel

        _ ->
            updatedModel


neighbourCount : Position -> Model -> Int
neighbourCount position model =
    position
        |> possibleNeighbours
        |> Set.fromList
        |> Set.intersect (Set.fromList model)
        |> Set.size


possibleNeighbours : Position -> List Position
possibleNeighbours ( xPos, yPos ) =
    [ ( xPos, yPos + 1 )
    , ( xPos, yPos - 1 )
    , ( xPos - 1, yPos + 1 )
    , ( xPos - 1, yPos )
    , ( xPos - 1, yPos - 1 )
    , ( xPos + 1, yPos + 1 )
    , ( xPos + 1, yPos )
    , ( xPos + 1, yPos - 1 )
    ]
