The GTK button widget
=======================
In this lesson I will go on with creating interesting windows. More
precisely I will add a nice button to the previous one.

> module GtkButton where

> import Graphics.UI.Gtk

Here are the novel functions that we will meet:

> import Graphics.UI.Gtk.Buttons.Button (buttonNewWithMnemonic,buttonSetRelief)


> main :: IO ()
> main = do
>    initGUI
>    window <- windowNew
>    set window [windowTitle := "Buttons" ]
>    containerSetBorderWidth window 25
>    widgetSetSizeRequest window 200 200
>    window `on` objectDestroy $ mainQuit

The function `buttonNewWithMnemonic :: String -> IO Button` creates a new
button that has a mnemonic key if the name string is prepended with an
underscore. For example the following button can be activated with the
keyboard using the control sequence "Alt+c":

>    button <- buttonNewWithMnemonic "_close"

The button appearance can be changed using `buttonSetRelief :: ButtonClass
self => self -> ReliefStyle -> IO ()` to add some visual depth to
it. There are three possible values `ReliefNormal`, `ReliefHalf` and
`ReliefNone`.

>    buttonSetRelief button ReliefNormal

However a button would be a rather dull thing if it didn't do anything
when we pressed it. The following event solves the problem:

    buttonActivated :: ButtonClass self => Signal self (IO ())

>    button `on` buttonActivated $ do widgetDestroy window
>                                     mainQuit

Now let's add the button to the window:

>    containerAdd window button

Show our work and start the loop.

>    widgetShowAll window
>    mainGUI

Kind of Events
--------------
Events are defined in `Graphics.UI.Gtk.Abstract.Widget` and are:

  buttonPressEvent

  buttonReleaseEvent
  configureEvent

The size of the window has changed.

  deleteEvent

The deleteEvent signal is emitted if a user requests that a toplevel
window is closed. The default handler for this signal destroys the
window. Calling widgetHide and returning True on reception of this signal
will cause the window to be hidden instead, so that it can later be shown
again without reconstructing it.

  destroyEvent

The destroyEvent signal is emitted when a DrawWindow is destroyed. You
rarely get this signal, because most widgets disconnect themselves from
their window before they destroy it, so no widget owns the window at
destroy time. However, you might want to connect to the objectDestroy
signal of Object.

  enterNotifyEvent


  exposeEvent

  focusInEvent

  focusOutEvent

  grabBrokenEvent

Emitted when a pointer or keyboard grab on a window belonging to widget
gets broken. On X11, this happens when the grab window becomes unviewable
(i.e. it or one of its ancestors is unmapped), or if the same application
grabs the pointer or keyboard again.


  keyPressEvent

  keyReleaseEvent

  leaveNotifyEvent

  mapEvent

The window is put onto the screen.

  motionNotifyEvent

The mouse pointer has moved. Since receiving all mouse movements is expensive,
it is necessary to specify exactly what mouse motions are required by calling
widgetAddEvents on this widget with one or more of the following flags:

    PointerMotionMask: Track all movements.
    ButtonMotionMask: Only track movements if a button is depressed.
    Button1MotionMask: Only track movements if the left button is depressed.
    Button2MotionMask: Only track movements if the middle button is depressed.
    Button3MotionMask: Only track movements if the right button is depressed.

 noExposeEvent

 proximityInEvent

The pen of a graphics tablet was put down.

 proximityOutEvent

The pen of a graphics tablet was lifted off the tablet.

 scrollEvent

The scroll wheel of the mouse has been used. Sets the widget's ScrollMask flag.

 unmapEvent

The window is taken off the screen.

 visibilityNotifyEvent

Emitted when the window visibility status has changed. Sets the widget's
VisibilityNotifyMask flag.

 windowStateEvent

Emitted when the state of the window changes, i.e. when it is minimized, moved
to the top, etc.
