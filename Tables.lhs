Creating tables
===============
Next widget in the series of containers is the table.

> module Tables where

> import Graphics.UI.Gtk

The new functions I will use are:

> import Graphics.UI.Gtk.Entry.Entry (entryNew)

> import Graphics.UI.Gtk.Layout.Table (tableNew,tableSetRowSpacings,
>                                      tableSetColSpacings)

> import Graphics.UI.Gtk.Layout.Table (tableAttach)

Here for the main course:

> main :: IO ()
> main = do
>    initGUI
>    window <- windowNew
>    set window [windowTitle := "Tables"]
>    window `on` objectDestroy $ mainQuit
>    containerSetBorderWidth window 10
>    widgetSetSizeRequest window 150 100

`tableNew :: Int -> Int -> Bool -> IO Table` creates a new table which we can
imagine as a regular grid of points. It gets as arguments the number of
horizontal and vertical points (minus one, because they are indexed from zero).

For example:

>    table <- tableNew 2 2 True

will create a table like this:


           0          1          2

         0 o----------o----------o
           |          |          |
           |          |          |
           |          |          |
           |          |          |
         1 o----------o----------o
           |          |          |
           |          |          |
           |          |          |
           |          |          |
         2 o----------o----------o


Now I create a couple of labels that I will put in the table later:

>    label <- labelNew $ Just "Enter the following information ..."
>    label2 <- labelNew $ Just "Name: "

And an input field using `entryNew :: IO Entry`:

>    input <- entryNew

On can attach the two labels and entry widget to their parent container
using

    tableAttach :: (WidgetClass child, TableClass self) => self ->
                   child -> Int-> Int -> Int -> Int -> [AttachOptions] ->
                   [AttachOptions] -> Int -> Int -> IO ()`.

The first 4 `Int`s arguments indicate left,right,top and bottom anchor
points where the widget should be attached. The last two `Int` parameters
are the amount of padding between the widget and his siblings.

>    tableAttach table label 0 2 0 1 [Expand] [Shrink] 0 0
>    tableAttach table label2 0 1 1 2 [Expand] [Shrink] 0 0
>    tableAttach table input 1 2 1 2 [Expand] [Shrink] 0 0

The above commands generate the following layout:


           0          1          2

         0 o----------o----------o
           |                     |
           |                     |
           |  "Enter the foll."  |
           |                     |
         1 o----------o----------o
           |          |          |
           |  "Name:" |  Entry   |
           |          |          |
           |          |          |
         2 o----------o----------o


Now add five pixels of spacing between every row and every column:

    tableSetRowSpacings :: TableClass self => self -> Int -> IO ()

>    tableSetRowSpacings table 5
>    tableSetColSpacings table 5

And some other routine command:

>    containerAdd window table
>    widgetShowAll window
>    mainGUI
