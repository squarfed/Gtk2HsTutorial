Widgets that expand
===================
A container that may be useful once in a while is one that can show or
hide some other widget, let's see how to realize it.

> module Expanders where

> import Graphics.UI.Gtk


> import Graphics.UI.Gtk.Layout.Expander (expanderNewWithMnemonic,
>                                         expanderSetExpanded)

The main function of this lesson is:

> main :: IO ()
> main = do
>    initGUI
>    window <- windowNew
>    set window [windowTitle := "Expander"]
>    window `on` objectDestroy $ mainQuit
>    containerSetBorderWidth window 10
>    widgetSetSizeRequest window 200 100

`expanderNewWithMnemonic :: String -> IO Expander` creates the expander kind
of container:

>    expander <- expanderNewWithMnemonic "Expand _Me"

Inside we will put the usual label:

>    label <- labelNew $ Just "Booh!"
>    containerAdd expander label

With a call to `expanderSetExpanded :: Expander -> Bool -> IO ()` one can
decide whether the expander will be shown in its expanded form or not,
obviously according to the value of the `Bool` argument:

>    expanderSetExpanded expander False

And that's more or less everything we need to know on the subject.

>    containerAdd window expander
>    widgetShowAll window
>    mainGUI
