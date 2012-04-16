Manipulating boxes
==================
We have seen the basics of signals and how to add a button to our windows,
in this section I will show how to add more than one widget inside a
window using boxes.

> module Boxes where

> import Control.Monad (forM_)
> import Graphics.UI.Gtk

There are jut two new functions that I will need:

> import Graphics.UI.Gtk.Abstract.Box (boxPackStartDefaults)
> import Graphics.UI.Gtk.Layout.VBox (vBoxNew)

A list of names which I will use to label buttons later:

> names :: [String]
> names = ["Nemo","Quatermain","Griffin","Hyde"]

The program:

> main :: IO ()
> main = do
>    initGUI
>    window <- windowNew
>    set window [windowTitle := "Boxes"]
>    window `on` objectDestroy $ mainQuit
>    containerSetBorderWidth window 10

The $-1$ passed to the next function means that we let the implementation
free of choosing the best size for the widget. Incidentally passing a 0
would make the widget as small as possible (but still functional).

>    widgetSetSizeRequest window 200 (-1)

One can create a set of vertical boxes using

  vBoxNew :: Bool -> Int -> IOVBox.

Its arguments are a boolean indicating whether the children are
homogenous in size, and the amount of spacing, in pixels, between
children. There is another function which creates horizontal boxes
`hBoxNew`.

>    vbox <- vBoxNew True 5
>    buttons <- mapM buttonNewWithLabel names

Now that there is a box and some buttons, I need:

    boxPackStartDefaults :: (WidgetClass widget, BoxClass self)
                          => self -> widget -> IO ()

It adds widgets, one at the time and top to bottom, to the vertical box.
The alternative `boxPackEndDefaults` does the same but bottom to top. I
associate to each button press a quit event.

>    forM_ buttons $ \button ->
>           do boxPackStartDefaults vbox button
>              button `on` buttonActivated $ do widgetDestroy window
>                                               mainQuit

What remains to be done should be clear by now\ldots

>    containerAdd window vbox
>    widgetShowAll window
>    mainGUI
