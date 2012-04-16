Creating Notebooks containers
=============================
In this module I show how to create widgets that have different pages which can
browse. A typical use is in option windows.

> module Notebooks where

> import Graphics.UI.Gtk

> import Graphics.UI.Gtk.Layout.Notebook (notebookAppendPage,notebookNew,
>                                         notebookSetTabPos,switchPage)

> main :: IO ()
> main = do
>    initGUI
>    window <- windowNew
>    set window [windowTitle := "Notebook"]
>    window `on` objectDestroy $ mainQuit
>    containerSetBorderWidth window 10
>    widgetSetSizeRequest window 250 100

First, create a notebook widget `notebookNew :: IO Notebook`

>    notebook <- notebookNew

The two browsable pages of the notebook will contain two label widgets:

>    child1 <- labelNew $ Just "Go to page two to find the answer"
>    child2 <- labelNew $ Just "Go to page one to find the answer"



>    notebook `on` switchPage $ notebookSetCurrentPage notebook -- SEGMENTATION FAULT!

We need to appned the child widget to the notebooks:

    notebookAppendPage :: (WidgetClass child, NotebookClass self) =>
                          self -> child -> String -> IO Int

>    notebookAppendPage notebook child1 "Page 1"
>    notebookAppendPage notebook child2 "Page 2"

And set the position of the tabs, using positions as we have seen previously.

>    notebookSetTabPos notebook PosTop
>    containerAdd window notebook
>    widgetShowAll window
>    mainGUI


