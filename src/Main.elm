-- Tutorial todo for blog you can find at
module Main exposing (..)

import Browser exposing (sandbox)
import Html exposing (Html, Attribute, header, div, ul, li, h1, h3, text, input)
import Html.Attributes exposing (class, id, value, placeholder)
import Html.Events exposing (onInput, onClick, onDoubleClick, on, keyCode)
import Json.Decode as Json

main : Program () Model Msg
main =
    Browser.sandbox
        {init = init
        , update = update
        , view = view
        }

-- MODEL
-- Consider the Model as all the states of our app, any update be made to it.
type alias Model =
    {todos:  List Todo
    , field: String
    }

type alias Todo =
    {id : Int
    , text : String
    , isDone: Bool
    }

-- Prepopulate list with initial values
todos : List Todo
todos =
    [ Todo 0 "Dinner at John's" False
    , Todo 1 "Mom's birthday soon" False
    , Todo 2 "Call Jerry asap!" False
    , Todo 3 "Meeting at Dublin on Saturday" True
    , Todo 4 "Feed the dog" False
    , Todo 5 "" False
    , Todo 6 "www.littlescrawl.com" False
    , Todo 7 "Thank you" False
    , Todo 8 "" False
    ]

init : Model
init =
    {todos = todos
    , field = ""
    }

-- END MODEL


-- UPDATE
type Msg =
    AddTodo
    | AddText String
    | RemoveTodo Int
    | KeyDown Int
    | ToggleTodo Int

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddText text ->
        {model | field = text}

    AddTodo  ->
        {model | todos = model.todos ++ [Todo (List.length model.todos) model.field False], field = "" }

    KeyDown key ->
      if key == 13 then
        {model | todos = model.todos ++ [Todo (List.length model.todos) model.field False], field = "" }
      else
        model

    RemoveTodo id ->
        {model | todos = List.filter (\x->x.id /= id ) model.todos}

    ToggleTodo id ->
        {model | todos = List.map (\x -> if (x.id == id) then {x | isDone = not (x.isDone) } else x ) model.todos}


-- END UPDATE


-- VIEWS
appHeader : Html Msg
appHeader =
    header []
           [h1 [] [ text "TO:DO" ]
           , h3 [] [ text "Do it with Elm!" ]
           ]

appInput : Model -> Html Msg
appInput model =
    input [ placeholder "Enter todo", value model.field, onInput AddText, onKeyDown KeyDown] []

todoList : Model -> Html Msg
todoList model =
    ul [ class "todos-container" ]
         (List.map (\l ->
            li [ class (if l.isDone then "is-done" else ""), onClick (ToggleTodo l.id), onDoubleClick (RemoveTodo l.id) ]
               [ text l.text ])
            model.todos)

view : Model -> Html Msg
view model =
    div [ id "app" ]
        [appHeader
        , appInput model
        , todoList model
        ]


-- END VIEW