Panes
=====
We have seen boxes, in this section I will introduce a new containr. The
Pane is a widget which contains two child widgets that can be arranged
either horizontally or vertically. There is a  handle between the
two that can be used to resize.

> module Panes where

> import Graphics.UI.Gtk

> import Graphics.UI.Gtk.Abstract.Paned (panedAdd1,panedAdd2)

> import Graphics.UI.Gtk.Layout.HPaned (hPanedNew)

> main :: IO ()
> main = do
>    initGUI
>    window <- windowNew
>    set window [windowTitle := "Panes"]
>    window `on` objectDestroy $ mainQuit
>    containerSetBorderWidth window 10
>    widgetSetSizeRequest window 225 150

`hPanedNew :: IO HPaned` and `vPanedNew :: IO HPaned` create horizontal
and vertical panes:

>    hpaned <- hPanedNew
>    button1 <- buttonNewWithLabel "Pull me right"
>    button2 <- buttonNewWithLabel "Push me left"
>    button1 `on` buttonActivated $ mainQuit
>    button2 `on` buttonActivated $ mainQuit

I put the buttons inside the panes using:

   panedAdd1 :: (PanedClass self, WidgetClass child) => self -> child -> IO ()

>    panedAdd1 hpaned button1
>    panedAdd2 hpaned button2

I conclude adding the panes to the window and starting the loop:

>    containerAdd window hpaned
>    widgetShowAll window
>    mainGUI

