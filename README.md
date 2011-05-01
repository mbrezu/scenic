-*- markdown -*-

# Introduction

A simple scene renderer/GUI.

         "Hamill: How was the road in?
          Miller: Scenic."
                 from "Saving Private Ryan"

Uses SDL via LISPBUILDER to define a sane way to draw 2D scenes and
graphical user interfaces.

Drawing is done using Cairo.

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

Loading scenic can be done by loading the Common Lisp implementation,
set the working directory to point to the `scenic` root directory, and
run:

   (load "deploy.lisp")
   (in-package :scenic-test)
   (scenic:run-scene (make-scene))


