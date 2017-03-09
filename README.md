# Agenda

Convenient way of handling chains of user actions.  This is inspired by **[this
parser][parser]**.

[parser]: http://package.elm-lang.org/packages/elm-tools/parser/1.0.2/


## How to use this library?

We explain a simple use case where `Agenda`'s might be handy.  Also, take
a look at the `demos` directory in this repository.

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
lineTool : Agenda Msg Line Error
lineTool =
    Line
        |~ inputPosition
        |= inputPosition


inputPosition  : Agenda Msg Vec2 Error
inputPosition =
    try "input position" <|
        \msg ->
            case msg of
                InputPosition v ->
                    Succees v

                _ ->
                    Error error


type Msg
    = NoOp
    | InputPosition Vec2
```

with some suitable `Error` type. Then the model is given by

```elm
type alias Model =
    { selectedTool : Maybe (Agenda Msg Line Error)
    , ...
    }
```

When the user chooses to add a line, we set `selectedTool = Just lineTool`.
Then each time the user triggers a message our update function has to update
`selectedTool` via `run tool msg`, which either returns a new Agenda via `Next
newAgenda` which we store by setting `selectedTool = Just newAgenda` in order
to be ready for more user input, or it returns `Success Line`, so we can add
the new line to our collection and set `selectedTool = Nothing`, or it returns
`Error error` indicating that the given `msg` did not fit the current agenda,
in which case we can either abort the whole tool or just continue with the last
agenda.


## Credits and License

&copy; 2017 Alexander Foremny and Fabian Kirchner

Licensed under [Apache License, version 2.0](LICENSE)
