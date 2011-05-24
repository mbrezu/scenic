-*- markdown -*-

# Introduction

A simple scene renderer/GUI.

         "Hamill: How was the road in?
          Miller: Scenic."
                 from "Saving Private Ryan"

Uses SDL via LISPBUILDER to define a sane way to draw 2D scenes and
graphical user interfaces.

Drawing is done using Cairo.

# WARNING - DANGER, DANGER, DANGER

Scenic contains a hack to make LISPBUILDER-SDL create SDL surfaces
that are compatible with Cairo (see file `sdl-patch.lisp`). If you
load Scenic into a Common Lisp image that you also use for other
projects that use LISPBUILDER-SDL, it will break LISPBUILDER-SDL
surface creation.

This will probably be fixed by patching LISPBUILDER-SDL (see
http://code.google.com/p/lispbuilder/issues/detail?id=23), but until
then this ugly hack is necessary to let Scenic use Cairo to draw on
SDL surfaces.

# Install

I assume that your Common Lisp implementation has Quicklisp
installed. If not, see http://www.quicklisp.org/beta/ about
instructions on how to install Quicklisp.

The implementations I tested the instructions below are:

 * SBCL (on Ubuntu)
 * CCL (on Ubuntu and Windows XP 32bit and Windows 7 32bit)
 * CLISP (on Ubuntu and Windows XP 32bit and Windows 7 32bit)

Scenic uses SDL (http://www.libsdl.org/) and Cairo
(http://cairographics.org/), so you need to have these installed. On
Ubuntu, this is usually already done (and apt-get can help if not). On
Windows, there are some DLLs in `win32-dlls.zip`. They work for
Windows 32bit (tested on Windows XP and Windows 7). Extract the DLL
files in the same directory as the archive.

Loading Scenic can be done by loading the Common Lisp implementation,
setting the working directory to point to the Scenic root directory,
and running:

    (load "deploy.lisp")
    (in-package :scenic-test)

and then

    (test-scene (background-clear))
    (test-scene (colored-rectangles))
    (test-scene (hello-world))
    (test-scene (buttons))
    (test-scene (slider))
    (test-scene (scrollbars))
    (test-scene (icon))
    (test-scene (text-baseline-alignment))

to run the various tests/demos, or

    (run-all-tests)

to run all tests in sequence.


# Other Notes

Since Scenic is tightly linked to Cairo (it uses it for drawing in a
lot of places), and only lightly coupled to SDL (it uses it to render
the GUI and to get an event loop - see file `scenic.lisp`), it could
be changed to use something else instead of SDL (maybe OpenGL and
GLUT?) for rendering the GUI and processing events.
