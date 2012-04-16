"Hello word" window - Adding signal handlers
============================================
In this second lesson I will extend the "Hello world" window with the
capability of handling external events.

> import Graphics.UI.Gtk

As before, some redundant imports just to see who is exporting what:

> import System.Glib.Signals (on)

> import Graphics.UI.Gtk.Abstract.Container (containerAdd,
>                                            containerSetBorderWidth)

> import Graphics.UI.Gtk.Abstract.Object (objectDestroy)

> import Graphics.UI.Gtk.Abstract.Widget (deleteEvent,destroyEvent,
>                                         widgetSetSizeRequest,
>                                         widgetShowAll)

> import Graphics.UI.Gtk.Buttons.Button (buttonActivated)

> import Graphics.UI.Gtk.Display.Label (labelNew,labelSetSelectable)

> import Graphics.UI.Gtk.Gdk.EventM (EventM)


Ok, let's code!

> main :: IO ()
> main = do

The first step are exactly as in the previous section:

>    initGUI
>    window <- windowNew
>    set window [windowTitle := "Hello world!"]

The window I just created will act as container for a widget showing some
text. A useful function to shape the container is:

    containerSetBorderWidth :: ContainerClass self => self -> Int -> IO ()

that sets the width, in pixels, of the gap between the border of the
parent container and that of the child widget:

>    containerSetBorderWidth window 10

I also suggest the size (again in pixels) that the window should have
when it's created. However note that window managers (e.g. xmonad) are
free to ignore it.

>    widgetSetSizeRequest window 200 100

The Gtk library has an event driven structure, that is the logic of the
program is determined associating widgets to signals and to actions to be
executed as response.

The function used to link widgets to signals is `on`:

   on :: object -> Signal object callback -> callback -> IO (ConnectId object)


The first argument is the widget, the second is a specific signal that the
widget may be receiving at some point in time, and the third is the
function that will be executed when the widget actually receives the
signal (callback in C speak). The returned id can be used later to
disconnect or suspend the link between widget and event.

The simplest type of callback function is `IO ()` as for example in the
case of the signal:

    objectDestroy :: ObjectClass self => Signal self (IO ())

>    window `on` objectDestroy $ mainQuit

However, not all callbacks are simple `IO` actions. One relevant class of
signals is those sent by the X-window system to the application, they are
termed "events" and their associated callback function is the *event
monad*.

    EventM a

This is actually a monad transformer built on an underlying `IO ()` monad,
but in addition to that it carries information on the signal that triggered
the callback.

For example:

    `deleteEvent :: WidgetClass self => Signal self (EventM EAny Bool)`

is a signal sent when the window is closed through the window manager.

If the monad returns False, the widget emits the signal `objectDestroy`

>    window `on` deleteEvent $ return False

And actually returning `False` is the default behaviour, so that the above
command is, in fact, redundant.

To end this first introduction to signals let me add that, as already
mentioned, the return value of `on` can be used to modify the linkage
between widget and signal. One can use the functions `signalBlock` and
`signalUnblock` to temporary activate/deactivate it and `signalDisconnect`
to delete it.

Back to the creation our window we introduce a new widget, the label:

   labelNew :: Maybe String -> IO Label

>    l <- labelNew (Just "Hello World")

The command `labelSelectable :: LabelClass self => Attr self Bool` makes
the label selectable with the cursor, this is important for example for
windows with error messages, we want to be able to copy and paste them in
the browser:

>    labelSetSelectable l True

    containerAdd :: (WidgetClass widget, ContainerClass self) =>
                    self -> widget -> IO ()

adds the label as a child widget of the window.

>    containerAdd window l

We also need to show the window. In the previous section we have used
`widgetShow` now we will use its bigger brother `widgetShowAll ::
WidgetClass self => self -> IO ()` that shows a window, and all his
children, and the children of the children... recursively.

>    widgetShowAll window

There is a command which does the opposite: `widgetHideAll`.

That's all, let's start the loop:

>    mainGUI
