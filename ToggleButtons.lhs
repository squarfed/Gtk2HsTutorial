Buttons that toggle things
==========================


> import Graphics.UI.Gtk


> main = do initGUI
>           window <- windowNew
>           set window [ windowTitle := "Toggle Buttons"]
>           containerSetBorderWidth window 10
>           vbox <- vBoxNew True 5
>           quit <- buttonNewWithMnemonic "_quit"
>           quit `on` buttonActivated $ do widgetDestroy window
>                                          mainQuit
>           toggle1 <- toggleButtonNewWithMnemonic "_Deactivate the other one!"
>           toggle2 <- toggleButtonNewWithMnemonic "_No! Deactivate that one!"
>           toggle1 `on` toggled $ buttonToggled toggle1 toggle2
>           toggle2 `on` toggled $ buttonToggled toggle2 toggle1
>           boxPackStartDefaults vbox quit
>           boxPackStartDefaults vbox toggle1
>           boxPackStartDefaults vbox toggle2
>           containerAdd window vbox
>           widgetShowAll window
>           mainGUI

> buttonToggled :: (WidgetClass self, ToggleButtonClass self) =>
>                  self -> self -> IO ()
> buttonToggled toggle1 toggle2 = do p <- toggleButtonGetActive toggle1
>                                    if p
>                                       then widgetSetSensitive toggle2 False
>                                       else widgetSetSensitive toggle2 True


