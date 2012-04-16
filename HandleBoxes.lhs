Containers with handles
=======================
In this lesson I will show how to create containers with handles that can
be used to attach or detach a child widget.

> module HandleBoxes where

> import Graphics.UI.Gtk

> import Graphics.UI.Gtk.Misc.HandleBox (handleBoxNew)

> import Graphics.UI.Gtk.General.Enums (PositionType,ShadowType)

> main :: IO ()
> main = do
>    initGUI
>    window <- windowNew
>    set window [windowTitle := "Handle box"]
>    window `on` objectDestroy $ mainQuit
>    containerSetBorderWidth window 10

A call to `handleBoxNew :: IO HandleBox` creates a container with a handle:

>    handle <- handleBoxNew

Inside which I will put a label widget:

>    label <- labelNew $ Just "Pull me out"

One can modify the appearance of a HandleBox in several ways. For one it is
possible to add a shadow, choosing from the following enumeration:

    ShadowNone | ShadowIn | ShadowOut | ShadowEtchedIn | ShadowEtchedOut

>    handleBoxSetShadowType handle ShadowIn

Furthermore it is possible to set the position of the handle and the edge
to which it should snap (if any):

    PosLeft | PosRight |PosTop | PosBottom

>    handleBoxSetHandlePosition handle PosLeft
>    handleBoxSetSnapEdge handle PosTop


>    containerAdd handle label
>    containerAdd window handle
>    widgetShowAll window
>    mainGUI

