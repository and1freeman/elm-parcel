module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, input, li, text, ul)
import Html.Attributes exposing (class, placeholder, style, value)
import Html.Events exposing (onClick, onInput, onSubmit)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL --


type TodoState
    = Initial
    | InProgress
    | Completed


type alias Todo =
    { id : Int
    , text : String
    , state : TodoState
    }


type alias Model =
    { todos :
        List Todo
    , currentValue : String
    }


init : Model
init =
    Model [] ""


type Msg
    = AddTodo
    | RemoveTodo Int
    | ToggleTodo Int
    | UpdateInputText String


getLastTodo : List Todo -> Maybe Todo
getLastTodo todos =
    todos
        |> List.reverse
        |> List.head


calcTodoId : List Todo -> Int
calcTodoId todos =
    case getLastTodo todos of
        Just todo ->
            todo.id + 1

        Nothing ->
            1


toggleTodo : List Todo -> Int -> List Todo
toggleTodo todos id =
    List.map
        (\todo ->
            if todo.id == id then
                case todo.state of
                    Initial ->
                        { todo | state = InProgress }

                    InProgress ->
                        { todo | state = Completed }

                    Completed ->
                        { todo | state = Initial }

            else
                todo
        )
        todos



-- UPDATE --


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTodo ->
            if String.isEmpty model.currentValue then
                model

            else
                { todos = model.todos ++ [ Todo (calcTodoId model.todos) model.currentValue Initial ]
                , currentValue = ""
                }

        RemoveTodo id ->
            { model | todos = List.filter (\todo -> todo.id /= id) model.todos }

        ToggleTodo id ->
            { model
                | todos = toggleTodo model.todos id
            }

        UpdateInputText newTodo ->
            { model | currentValue = newTodo }



-- VIEW --


todoView : Todo -> Html Msg
todoView todo =
    li
        [ style "text-decoration"
            (case todo.state of
                Initial ->
                    "none"

                InProgress ->
                    "underline"

                Completed ->
                    "line-through"
            )
        ]
        [ text todo.text
        , button [ onClick (ToggleTodo todo.id) ] [ text "toggle" ]
        , button [ onClick (RemoveTodo todo.id) ] [ text "remove" ]
        ]


view : Model -> Html Msg
view model =
    div [ class "h-full flex flex-col pt-8 px-8" ]
        [ form
            [ onSubmit AddTodo
            , class "border border-[#f00] p-4"
            ]
            [ input
                [ placeholder "Type here.."
                , value model.currentValue
                , onInput UpdateInputText
                , class ""
                ]
                []
            , button [] [ text "add todo" ]
            ]
        , ul [] (List.map (\todo -> todoView todo) model.todos)
        ]
