Creation of a simple "Hello world" window
=========================================
Our journey in the world of graphical interfaces will begin with a simple
program to show a "Hello world" window.

First and foremost one must import the GTK library:

> import Graphics.UI.Gtk

The following imports are redundant, given the one above, but I include
them to show where each new function used in the module is located.

> import System.Glib.Attributes (AttrOp((:=)),set)

> import Graphics.UI.Gtk.Abstract.Widget (widgetShow)
> import Graphics.UI.Gtk.General.General (mainGUI)
> import Graphics.UI.Gtk.Windows.Window (windowNew)

Now that all the relevant functions have been imported, let's get busy:

> main :: IO ()
> main = do

The function `initGUI :: IO [String]` initializes the window system. It is
always the first thing we call.

>    initGUI

Next with `windowNew :: IO Window` I create a new window:

>    window <- windowNew

Widget parameters can be set using the function:

    set :: w -> [AttrOp w] -> IO ()

that takes the widget and a list of `AttrOp` (Attribute operations) and
set their values. AttrOps can be constructed using infix operators, the
one I use in this exmple is `:=` which is used to assign a value to an
attribute.

>    set window [windowTitle := "Hello world!"]

The window itself won't show up unless told it explicitly with the
`widgetShow :: WidgetClass self => self -> IO ()` function:

>    widgetShow window

Last but not least we need to start the graphic loop. `mainGUI :: IO ()`
will take care of that:

>    mainGUI

And that's all, enjoy your first Haskell window!
