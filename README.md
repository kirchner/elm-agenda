# Agenda

Convenient way of handling chains of user actions.  This is inspired by **[this
parser][parser]**.  Note that this is still under development and the interface
may change in the future.  All contritbutions welcome!  (especially more demos)

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
lineTool : Agenda () Msg Line
lineTool =
    succeed Line
        |= inputPosition
        |= inputPosition


inputPosition  : Agenda () Msg Vec2
inputPosition =
    try <|
        \msg ->
            case msg of
                InputPosition v ->
                    succeed v

                _ ->
                    fail


type Msg
    = NoOp
    | InputPosition Vec2
```

Then the model is given by

```elm
type alias Model =
    { selectedTool : Maybe (Agenda () Msg Line)
    , ...
    }
```

When the user chooses to add a line, we set `selectedTool = Just lineTool`.
Then each time the user triggers a message our update function has to update
`selectedTool` via `run tool msg`, which returns a new Agenda say `newAgenda`.
We then can check with `result` if we already have produced a new line which we
then can add to our collection and set `selectedTool = Nothing`, and we can
check with `error` whether our Agendas has failed, in which case we can either
abort the whole tool or just continue with the previous agenda.  If none of
this is the case we store the new Agenda with `selectedTool = Just newAgenda`
in order to be ready for more user input.


## Intermediate States and Monadic Interface

Instead of using the applicative interface as above, we can also write an
Agenda in the following way:

```elm
lineTool : Agenda () Msg Line
lineTool =
    inputPosition >>= \p ->
    inputPosition >>= \q ->
    succeed (Line p q)
```

The advantage is, that we can use already obtained values (like `p`) in the
chain.  This becomes very usefull, if we want to add intermediate states, which
can be retrieved even if the agenda has not produced a final result, yet.  For
our `lineTool` this would be a `Point p` after the first `inputPosition`:

```
lineTool : Agenda Element Msg Element
lineTool =
    inputPosition >>= \p ->
    tell (point p) >>>
    inputPosition >>= \q ->
    succeed (line p q)


type Element
    = Point PointInfo
    | Line LineInfo


point : Vec2 -> Element

line : Vec2 -> Vec2 -> Element
```

If we call `tell`, we add the given state onto the state stack of the Agenda.
This stack can then be retrieved at any time via `state : Agenda s msg a ->
List s'`.  The SvgEditor and the GameControls demos make extensive use of this
feature.

Note, that if you are using `elm-format`, your Agendas will unfortunately not
be formatted in a very desireable way:

```elm
lineTool : Agenda Element Msg Element
lineTool =
    inputPosition
        >>= \p ->
            tell (point p)
                >>> inputPosition
                >>= \q ->
                    succeed (line p q)
```

which gets even worse if your chains get very long.


## Demos

We implemented some short working demos, to show how one could use this module:

* `SvgAgenda`: draw svg graphics.
* `Vim`: parsing vim-commands.
* `GameControls`: an idea, how one could implemente combos in a game.


## Credits and License

&copy; 2017 Alexander Foremny and Fabian Kirchner

Licensed under [Apache License, version 2.0](LICENSE)
