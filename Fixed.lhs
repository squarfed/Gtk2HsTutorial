Fixed containers
================
This module shows how it is possible to nest widgets specifying their
position using coordinates. Fixed containers are not often used.

> import Graphics.UI.Gtk

Two new functions:

> import Graphics.UI.Gtk.Layout.Fixed (fixedNew,fixedPut)

The program:

> main :: IO ()
> main = do
>    initGUI
>    window <- windowNew
>    set window [windowTitle := "Fixed layout"]
>    containerSetBorderWidth window 10
>    window `on` objectDestroy $ mainQuit

The function `fixedNew :: IO Fixed` creates a fixed position container:

>    fixed <- fixedNew

Some buttons to put in the container:

>    button1 <- buttonNewWithLabel "This will desrtoy me"
>    button2 <- buttonNewWithLabel "And this too."
>    button1 `on` buttonActivated $ widgetDestroy window
>    button2 `on` buttonActivated $ widgetDestroy window

The function:

    fixedPut :: (WidgetClass widget, FixedClass self) => self -> widget
             -> (Int, Int) -> IO ()

puts a widget at some specified coordinates into a fixed type of
container. The pair `(0,0)` is the top left point of the container.

>    fixedPut fixed button1 (0,0)
>    fixedPut fixed button2 (20,30)

That's all for novel concepts...

>    containerAdd window fixed
>    widgetShowAll window
>    mainGUI

Enough said about fixed containers!
