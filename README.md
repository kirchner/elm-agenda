# Agenda

Convenient way of handling chains of user actions.  This is inspired by **[this
parser][parser]**.

[parser]: http://package.elm-lang.org/packages/elm-tools/parser/1.0.2/

Suppose you are writing a vector graphics program.  You want the user to
be able to add a line segment

```elm
type alias Line =
    { start : Vec2
    , end : Vec2
    }
```

by clicking two times into the canvas in order to give the position of
its start and its end point.  We can implement such a tool as the
following `Agenda`:

```elm
lineTool : Agenda Msg Line
lineTool =
    succeed Line
        |= inputPosition
        |= inputPosition


inputPosition  : Agenda Msg Vec2
inputPosition =
    try "input position" <|
        \msg ->
            case msg of
                InputPosition v ->
                    Just <| succeed v

                _ ->
                    Nothing


type Msg
    = NoOp
    | InputPosition Vec2
```

Then the model is given by

```elm
type alias Model =
    { selectedTool : Maybe (Agenda Msg Line)
    , ...
    }
```

When the user chooses to add a line, we set `selectedTool = Just
lineTool`.  Then each time the user triggers a message our update
function has to update `selectedTool` via `run tool msg`, which either
returns a new Agenda `newAgenda` which we store by setting `selectedTool
= Just newAgenda` in order to be ready for more user input, or it
returns `Ok Line`, which we then can add to the set of lines and we
set `selectedTool = Nothing`.


## Credits and License

&copy; 2017 Alexander Foremny and Fabian Kirchner

Licensed under [Apache License, version 2.0](LICENSE)
